options(shiny.maxRequestSize=30*1024^2) 

library(lubridate)
library(curl)
library( leaflet )    
library( shiny )
library( shinydashboard )
library(sp) #spatial data wrangling & analysis
library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) # data wrangling
library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations
library(data.table)
library(htmltools)
library(leaflet.extras)
library(lubridate)
library(dplyr)
library(maptools)
library(bit64)
library(plyr)
library(ggthemr)
library(scales)

ggthemr::ggthemr("fresh")
comarea606 <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/chizip.csv")
hospitals <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/hospitals/hospitals.csv")
hosp_dist <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/processed/nodes_hosp_df.csv")
#schools <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/schools/School%20Point%202014.csv")
#senior <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/senior_centers/senior.centers.csv")
comarea606 <- readRDS( gzcon( url( description = "https://github.com/rsangole/capstone_project/blob/master/data/raw/chizip.RDS?raw=true" ) ) )
#weather <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/chi_weather/Chicago_Only_Weather/Weather_2017.csv")
wnv_df <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/processed/wnv.trap.date.rev3b.csv")
nodes_trap_df <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/processed/nodes_trap_df.csv")

wnv_df$zip <- as.character(wnv_df$loc_zipcode)
comarea606_df <- fortify(comarea606, region="zip")
comarea606_df$id <- as.integer(comarea606_df$id) #convert to sameformat
comarea606_df$loc_zipcode <- comarea606_df$id

train_df <- wnv_df

pos_wnv <- train_df %>%
  dplyr::filter(mos_any_wnv_present %in% "TRUE")

neg_wnv <- train_df %>%
  dplyr::filter(!mos_any_wnv_present %in% "TRUE")

hospitals$LATITUDE <- as.numeric(hospitals$LATITUDE)
hospitals$LONGITUDE <- as.numeric(hospitals$LONGITUDE)


hosp2 <- hospitals %>% drop_na(LATITUDE)

chihosp <- hosp2 %>%
  dplyr::filter(Community %in% 'Chicago')


mos.by.zip <- train_df %>%
  dplyr::group_by(loc_zipcode) %>%
  dplyr::summarize(total_mos_zip = sum(mos_tot_num_mosquitos))


wnv.by.zip <- train_df %>%
  dplyr::group_by(loc_zipcode) %>% 
  dplyr::filter(mos_any_wnv_present %in% "TRUE") %>% 
  dplyr::summarize(total_wnv_zip = n()) 

# join datasets above
wnv.mos.tot <- left_join(mos.by.zip, wnv.by.zip, by = c("loc_zipcode"))

# set NAs to 0  
wnv.mos.tot[is.na(wnv.mos.tot)] <- 0

train_df$zip <- as.character(train_df$loc_zipcode)

####################################
#---------------- create the server
####################################
server <- function( input, output, session ){
  data <- reactiveValues(clickedMarker=NULL)
  
  isFirstInRun <- function(x,...) {
    lengthX <- length(x)
    if (lengthX == 0) return(logical(0))
    retVal <- c(TRUE, x[-1]!=x[-lengthX])
    for(arg in list(...)) {
      stopifnot(lengthX == length(arg))
      retVal <- retVal | c(TRUE, arg[-1]!=arg[-lengthX])
    }
    if (any(missing<-is.na(retVal))) # match rle: NA!=NA
      retVal[missing] <- TRUE
    retVal
  }
  
  
  redhosp.icon <- makeAwesomeIcon(icon='heartbeat', library='fa', markerColor = 'red', iconColor = 'black')
  amberhosp.icon <- makeAwesomeIcon(icon='heartbeat', library='fa', markerColor = 'orange', iconColor = 'black')
  greenhosp.icon <- makeAwesomeIcon(icon='heartbeat', library='fa', markerColor = 'green', iconColor = 'black')
  wnv.icon <- makeAwesomeIcon(icon='exclamation-triangle', library='fa', markerColor = 'purple', iconColor = 'black')
  
  poswnv_df <- reactive({
    
    pos_wnv <- wnv_df %>%
      dplyr::filter(t_date >= input$datesSlider[1] & t_date <= input$datesSlider[2]) %>%
      dplyr::filter(mos_any_wnv_present %in% "TRUE") %>%
      dplyr::select(trap_trap_name, t_date)
    
    # get the last date wnv tested positive
    j2<- pos_wnv[with(pos_wnv, order(trap_trap_name,rev(t_date))),]
    
    j3 <- within(j2, first <- isFirstInRun(trap_trap_name)) # TRUE if either changes
    
    j4 <- j3 %>%
      dplyr::filter(first %in% "TRUE") %>%
      dplyr::select(trap_trap_name, t_date) %>%
      dplyr::rename(last_wnv_dt = t_date)
    
    return(j4)
    
  })
  
  all_hosp_trap_df <- reactive({
    
    t <- wnv_df %>%
      dplyr::filter(t_date >= input$datesSlider[1] & t_date <= input$datesSlider[2]) %>%
      dplyr::select(trap_trap_name, mos_any_wnv_present, t_date)
    
    j <- inner_join(t,hosp_dist) #join mast wnv file with hospital df
    
    j <- left_join(j, poswnv_df()) # join new file with positive wnv
    
    j[c("last_wnv_dt")][is.na(j[c("last_wnv_dt")])] <- "2000-01-01" # set any NA to this date
    
    j<- j[with(j, order(rev(t_date),id,dist_to_trap)),]
    
    # ----------------- calculate days since last test date and last wnv date
    j$days_since_wnv<-difftime(as.Date(j$t_date),as.Date(j$last_wnv_dt),units='days')
    
    # create RAG based on proximity and less tha 2 weeks
    wnv_dur <- 14
    
    j<- mutate(j, RAG = ifelse(dist_to_trap  <= 2.9  & days_since_wnv <= wnv_dur, "Red",
                               ifelse(dist_to_trap  <= 4.9 & days_since_wnv <= wnv_dur, "Amber","Green")))
    
    return(j)
  })
  
  
  shiny_train_df <- reactive({
    shiny_train_df <- train_df
    return(shiny_train_df)
  })
  
  pos_wnv_filtered <- reactive({
    
    pos_wnv[pos_wnv$t_date >= input$datesSlider[1] & pos_wnv$t_date <= input$datesSlider[2],]
    
  })
  
  wnv_df_filtered <- reactive({
    
    train_df[train_df$t_date >= input$datesSlider[1] & train_df$t_date <= input$datesSlider[2],]
    
  })
  
  
  output$slidertest <- renderPrint({
    pos_wnv_filtered() %>%
      dplyr::summarise(cnt = n())
  })
  
  
  uniq_hosp_trap_df <- reactive({
    
    j <- all_hosp_trap_df()
    
    j <- within(j, first <- isFirstInRun(t_date, id)) # TRUE if either changes
    
    j <- plyr::ddply(j, "id", function(t_date) head(t_date,1))
    
    j <- j %>%
      dplyr::filter(first %in% "TRUE")
    
    
    j$label_content <- paste0("<strong>Location:</strong> ", j$id,"<br>",
                              "<strong>WNV Alert:</strong> ", j$RAG,"<br>",
                              "<strong>Days Since Last WNV Present:</strong> ", j$days_since_wnv,"<br>",
                              "<strong>Last Date Closest Trap Test for WNV:</strong> ", j$last_wnv_dt,"<br>",
                              "<strong>Last Date Closest Trap Tested:</strong> ", j$t_date,"<br>",
                              "<strong>Closest Trap ID:</strong> ", j$trap_trap_name,"<br>",
                              "<strong>Distance to Closest Trap (mi):</strong> ", j$dist_to_trap,"<br>")
    
    return(j)
    
  })
  
  
  #---------------------------------------------------------------------------
  # ------ this section is so the pop-up box has the correct label information
  #---------------------------------------------------------------------------
  hosp_label <- reactive({
    
    z <- left_join(chihosp, uniq_hosp_trap_df(), by = c("LONGITUDE" = "longitude",
                                                        "LATITUDE" = "latitude"))
    return(z)
    
  })
  
  red_hosp <- reactive ({
    x <- hosp_label() %>% 
      filter(RAG %in% "Red")
    
  })
  
  amber_hosp <- reactive ({
    x <- hosp_label() %>% 
      filter(RAG %in% "Amber")
    
  })
  
  green_hosp <- reactive ({
    x <- hosp_label() %>% 
      filter(RAG %in% "Green")
    
  })
  
  #------- get distinct traps for the map
  #------- removing the distinct function has it match the total # of wnv from the dashboard
  #------- but it shows multiple markers for the same location because of the date range e.g.
  #------- 10 wnv between start & end date shows 10 markers.  we only need to show the marker 1x
  dist_marker_pos_wnv <- reactive({
    
    j <- pos_wnv_filtered() %>% 
      dplyr::arrange(desc(t_date)) %>% 
      dplyr::distinct(trap_trap_name, loc_lng, loc_lat)
    
    return(j)
    
  })
  
  
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet() %>%
      clearShapes() %>%
      addResetMapButton() %>%
      addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
      addProviderTiles(providers$CartoDB.Voyager, group="Voyager") %>%
      
    addAwesomeMarkers(lng=red_hosp()$LONGITUDE, lat = red_hosp()$LATITUDE,
                      icon = redhosp.icon,
                      group = "Hospitals - High Alert",
                      popup = red_hosp()$label_content,
                      clusterId = "hospital",
                      clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng=amber_hosp()$LONGITUDE, lat = amber_hosp()$LATITUDE,
                        icon = amberhosp.icon,
                        group = "Hospitals - Medium Alert",
                        popup = amber_hosp()$label_content,
                        clusterId = "hospital",
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng=green_hosp()$LONGITUDE, lat = green_hosp()$LATITUDE,
                        icon = greenhosp.icon,
                        group = "Hospitals - Low Alert",
                        popup = green_hosp()$label_content,
                        clusterId = "hospital",
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng = dist_marker_pos_wnv()$loc_lng, lat = dist_marker_pos_wnv()$loc_lat,
                        icon=wnv.icon,
                        group = "WNV - confirmed",
                        label=as.character(dist_marker_pos_wnv()$trap_trap_name),
                        clusterId = "poswnv",
                        clusterOptions = markerClusterOptions()) %>%
      
    addLayersControl(baseGroups = c("Voyager", "Light"),
                     #overlayGroups = c("schools", "hospitals", "senior centers", "WNV - confirmed"),
                     overlayGroups = c("Hospitals - High Alert", "Hospitals - Medium Alert", "Hospitals - Low Alert","WNV - confirmed"),
                     options = layersControlOptions(collapsed = TRUE))%>%
      
      
      setView( lng = -87.567215
               , lat = 41.822582
               , zoom = 11 ) %>%
      addPolygons( data = comarea606
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 4
                   , layerId = comarea606$zip
                   , group = "click.list"
      )
  })
  
  output$myMap <- renderLeaflet({
    foundational.map() %>%
      clearShapes()
    
  }) # end of leaflet::renderLeaflet({})
  
  
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$myMap_marker_click,{
    data$clickedMarker <- input$myMap_marker_click
    print(data$clickedMarker)}
  )
  
  
  output$clicktest <- renderPrint({
    data$clickedMarker <- input$myMap_marker_click
    
    long <- round(data$clickedMarker$lng, digits = 5)
    lat <- round(data$clickedMarker$lat, digits = 5)
    
    long <- data$clickedMarker$lng
    lat <- data$clickedMarker$lat
    
    print(data$clickedMarker)
  })
  
  
  output$ragtest <- renderPrint({
    
    print(getMarkerClick2()) 
    
    
  })
  
  
  
  getMarkerClick <- shiny::reactive({
    #output$clusterPlot2 <- renderPrint({
    if (is.null(input$myMap_marker_click))
      return(NULL)
    
    
    data$clickedMarker <- input$myMap_marker_click
    
    # get the location of the click and the nearest trap testing in the date range
    t <- uniq_hosp_trap_df()
    
    longitude = data$clickedMarker$lng
    latitude = data$clickedMarker$lat
    
    # --- this is needed to see if the marker is a hospital or trap
    cluster = data$clickedMarker$clusterId
    
    
    
    # --- create a geospatial data for the marker selected
    coord_df <- data.frame(longitude,latitude)
    
    # --- if wnv marker clicked then do this:
    
    if (cluster %in% "hospital") {
      t <- subset(t, select = -c(t_date,mos_any_wnv_present))
      z <- merge(t, coord_df, all.y=T)  
    } else {
      y <- wnv_df_filtered() %>% 
        dplyr::select(t_date, trap_trap_name, loc_lng, loc_lat) %>% 
        dplyr::distinct(trap_trap_name, loc_lng, loc_lat) %>% 
        dplyr::rename(longitude = loc_lng, latitude = loc_lat)
      
      z <- merge(y, coord_df, all.y=T)  
      #z <- coord_df
    }
    
    return(z)
  })
  
  getMarkerClick2 <- shiny::reactive({ 
    
    if (is.null(getMarkerClick()))
      return(NULL)
    
    z <- getMarkerClick() 
    
    
    #------- this is for cluster_id = hospital
    # ----- movedd this to the if else statement above since now bringing in clusters
    
    # get the trap info from the master wnv file
    x <- inner_join(wnv_df_filtered(), z, by="trap_trap_name") 
    
    plot2_mos_cnt <- x %>%
      dplyr::group_by(t_date, trap_trap_name) %>%
      dplyr::filter(t_date >= (input$datesSlider[2] - 180)) %>% #Get last 2 months from enddate
      dplyr::summarise(tot_mos = sum(mos_tot_num_mosquitos)) %>% 
      dplyr::select(t_date, trap_trap_name, tot_mos)
    
    plot2_wnv_cnt <- x %>%
      dplyr::group_by(t_date, trap_trap_name) %>%
      dplyr::filter(mos_any_wnv_present  %in% "TRUE") %>%
      dplyr::summarise(tot_wnv = n()) %>%
      dplyr::select(t_date, trap_trap_name, tot_wnv)
    
    wnv_mos_df <- left_join(plot2_mos_cnt,plot2_wnv_cnt, by=c("t_date","trap_trap_name"))
    
    wnv_mos_df[is.na(wnv_mos_df)] <- 0
    
    wnv_mos_df$t_date <- as.Date(wnv_mos_df$t_date, format='%Y-%m-%d')
    
    return(wnv_mos_df)
    
  })
  
  output$clusterPlot2 <- renderPlot({
    if (is.null(getMarkerClick2()))
      return(NULL)

    wnv_mos_ym_df <- getMarkerClick2()

    title_list <- wnv_mos_ym_df$trap_trap_name

    dynTitle <- paste("Mosquitos from Trap ID: ", wnv_mos_ym_df$trap_trap_name)
    p <- ggplot(wnv_mos_ym_df, aes(t_date, tot_mos, group=1)) +
      geom_line() +
      labs(title = dynTitle, x="Date", y="# Mosquitos")+
      scale_x_date(breaks = wnv_mos_ym_df$t_date, labels=date_format("%d-%b-%y")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

    p
  })
  
  output$clusterPlot3 <- renderPlot({  
    if (is.null(getMarkerClick2()))
      return(NULL)
    
    wnv_mos_ym_df <- getMarkerClick2()
    
    p <- ggplot(wnv_mos_ym_df, aes(MonthAbb, total_wnv)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    p  + facet_wrap(~year, ncol = 3)
    
  })
  
  
  foundational.heatmap <- shiny::reactive({
    leaflet() %>%
      clearShapes() %>%
      addResetMapButton() %>%
      
      addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
      addProviderTiles(providers$CartoDB.Voyager, group="Voyager") %>%
      
      addHeatmap(group = "WNV heat map", lng = pos_wnv$loc_lng, lat = pos_wnv$loc_lat,
                 gradient = c("green", "red"),
                 intensity=pos_wnv$mos_tot_num_mosquitos, max=.6, blur=60) %>%
      addAwesomeMarkers(lng=red_hosp()$LONGITUDE, lat = red_hosp()$LATITUDE,
                        icon = redhosp.icon,
                        group = "Hospitals - High Alert",
                        popup = red_hosp()$label_content,
                        clusterId = "hospital",
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng=amber_hosp()$LONGITUDE, lat = amber_hosp()$LATITUDE,
                        icon = amberhosp.icon,
                        group = "Hospitals - Medium Alert",
                        popup = amber_hosp()$label_content,
                        clusterId = "hospital",
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng=green_hosp()$LONGITUDE, lat = green_hosp()$LATITUDE,
                        icon = greenhosp.icon,
                        group = "Hospitals - Low Alert",
                        popup = green_hosp()$label_content,
                        clusterId = "hospital",
                        clusterOptions = markerClusterOptions()) %>%
      addLayersControl(baseGroups = c("Voyager", "Light"),
                       overlayGroups = c("Hospitals - High Alert", "Hospitals - Medium Alert", "Hospitals - Low Alert"),
                       options = layersControlOptions(collapsed = TRUE))%>%
      
      setView( lng = -87.567215
               , lat = 41.822582
               , zoom = 11 ) %>%
      addPolygons( data = comarea606
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 4
                   , layerId = comarea606$zip
                   , group = "click.list"
      )
  })
  
  output$myHeatMap <- renderLeaflet({
    
    foundational.heatmap() %>%
      clearShapes()
    
  }) # end of leaflet::renderLeaflet({})
  
  # store the list of clicked polygons in a vector
  click.list <- shiny::reactiveValues( ids = vector() )
  
  
  shiny::observe({
    if (nrow(pos_wnv_filtered()) == 0) {
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        clearMarkerClusters() %>%
        clearMarkers() %>% 
        clearShapes() 
    } else {
      leaflet::leafletProxy( mapId = "myMap", data=pos_wnv_filtered() ) %>%
        clearMarkerClusters() %>%
        
        addAwesomeMarkers(lng=red_hosp()$LONGITUDE, lat = red_hosp()$LATITUDE,
                          icon = redhosp.icon,
                          group = "Hospitals - High Alert",
                          popup = red_hosp()$label_content,
                          clusterId = "hospital",
                          clusterOptions = markerClusterOptions()) %>%
        addAwesomeMarkers(lng=amber_hosp()$LONGITUDE, lat = amber_hosp()$LATITUDE,
                          icon = amberhosp.icon,
                          group = "Hospitals - Medium Alert",
                          popup = amber_hosp()$label_content,
                          clusterId = "hospital",
                          clusterOptions = markerClusterOptions()) %>%
        addAwesomeMarkers(lng=green_hosp()$LONGITUDE, lat = green_hosp()$LATITUDE,
                          icon = greenhosp.icon,
                          group = "Hospitals - Low Alert",
                          popup = green_hosp()$label_content,
                          clusterId = "hospital",
                          clusterOptions = markerClusterOptions()) %>%
        
        
      addAwesomeMarkers(lng = dist_marker_pos_wnv()$loc_lng, lat = dist_marker_pos_wnv()$loc_lat,
                        icon=wnv.icon,
                        group = "WNV - confirmed",
                        label=as.character(dist_marker_pos_wnv()$trap_trap_name),
                        clusterId = "poswnv",
                        clusterOptions = markerClusterOptions())
      
    }
    
  }) #end shiny::observe
  
  observeEvent(input$clear, {
    leaflet::leafletProxy("myMap") %>% clearMarkerClusters()
  })
  
  
  shiny::observe({
    if (nrow(pos_wnv_filtered()) == 0) {
      leaflet::leafletProxy( mapId = "myHeatMap" ) %>%
        clearHeatmap()
    } else {
      leaflet::leafletProxy( mapId = "myHeatMap", data=pos_wnv_filtered() ) %>%
        clearHeatmap() %>% 
        addHeatmap(group = "WNV heat map", lng = pos_wnv_filtered()$loc_lng, lat = pos_wnv_filtered()$loc_lat,
                   gradient = c("green", "red"),
                   intensity=pos_wnv_filtered()$mos_tot_num_mosquitos, max=.6, blur=60) 
      
    }
    
  }) #end shiny::observe
  
  output$chiMap<- renderPlot({
    
    chicagoBaseMap <- ggplot() 
    chicagoBaseMap <- chicagoBaseMap + geom_polygon(data=comarea606_df, aes(x=long, y=lat, group=group, fill="#528076"), color = "black", fill=NA, size=0.5)  
    
    chicagoBaseMap + geom_density2d(data = pos_wnv_filtered(), aes(x = loc_lng, y = loc_lat), size = 0.3) +
      stat_density2d(data = pos_wnv_filtered(), aes(x = loc_lng, y = loc_lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
      scale_fill_gradient(name="Number of WNV+", low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      ggtitle("Hospitals with High Alert") +
      xlab("Longitude") + ylab("Latitude") +
      geom_point(data = red_hosp(), aes(x = LONGITUDE, y = LATITUDE), shape = 72, size = 3, colour="red") +
      theme(legend.position="bottom")
    
  })
  
  
  output$wnvcount <- renderPlot({
    
    t <- pos_wnv_filtered() %>%
      mutate(highlight_flag = ifelse(t_yr < '2017', 'Actual', 'Projected'))
    
    ggplot(t, aes(x = t_yr)) +
      geom_bar(aes(fill = highlight_flag))+
      geom_text(aes(label=..count..),stat="count", vjust=-.85, color="black") +
      scale_x_continuous(breaks = t$t_yr, labels = t$t_yr) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom") +
      labs(title = "Confirmed WNV by Year\n", caption = "", x = "Year", y = "# WNV",fill = "Legend") 
    
  })
  
  
  output$wnv_zip <- renderPlot({
    wnv_per_zip <- pos_wnv_filtered() %>%
      dplyr::group_by(zip, trap_trap_name, t_date) %>%
      dplyr::summarise(num_wnv_zip = n_distinct(mos_any_wnv_present))
    
    tot_wnv_per_zip <- wnv_per_zip %>%
      dplyr::group_by(zip) %>%
      dplyr::summarise(tot_wnv_zip = sum(num_wnv_zip)) %>% 
      dplyr::top_n(10)
    
    ggplot(tot_wnv_per_zip, aes(x=zip, y=tot_wnv_zip)) +
      geom_bar(stat="identity", width=.5 ) +
      geom_text(aes(label=tot_wnv_zip), hjust=1.5) +
      labs(title="Top 10 Zip Codes",
           subtitle="With West Nile Virus",
           x="Zip Codes", y = "# WNV",
           caption="") +
      theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
      coord_flip()
    
  })
  
  output$traps_zip <- renderPlot({
    
    trap_per_zip <- shiny_train_df() %>%
      dplyr::group_by(zip) %>%
      dplyr::summarise(num_traps = n_distinct(trap_trap_name))
    
    ggplot(trap_per_zip, aes(x=zip, y=num_traps)) +
      geom_bar(stat="identity", width=.5, fill="tomato3") +
      labs(title="Total Traps by Zipcode",
           subtitle="2007 - 2013",
           caption="training data") +
      theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
      coord_flip()
    
  })
  
  output$wnvrate <- renderValueBox({
    wnvcnt <- pos_wnv_filtered() %>%
      dplyr::summarise(cnt = n())
    
    valueBox(
      value = formatC(wnvcnt$cnt , digits = 0, format = "f"),
      subtitle = "Total Confirmed West Nile Virus",
      icon("exclamation-triangle", lib="font-awesome"), 
      color = if (wnvcnt$cnt >= 150) "yellow" else "aqua"
    )
    
  })
  
  
  
  output$mos_count <- renderValueBox({
    
    moscnt <- wnv_df_filtered() %>%
      dplyr::summarise(tot_mos = sum(mos_tot_num_mosquitos))
    
    valueBox(
      value = moscnt$tot_mos,
      subtitle = "Total mosquitos trapped",
      icon("bug", lib="font-awesome") 
      
    )
  })
  
  
  output$hosp_kpi <- renderValueBox({
    
    hosp_kpi <- hosp_label() %>% 
      dplyr::filter(RAG %in% c("Red")) %>% 
      dplyr::summarise(cnt = n())
    
    valueBox(
      value = formatC(hosp_kpi$cnt , digits = 0, format = "f"),
      subtitle = "Total Hospitals in High Alert",
      icon("hospital", lib="font-awesome"), 
      color = if (hosp_kpi$cnt >= 3) "red" else "aqua"
    )
  })
} # end of server

#shiny::shinyApp( ui = ui, server = server)

