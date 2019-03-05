# load necessary packages
library( leaflet )    
library( shiny )
library( shinydashboard )
library(lubridate)
library(data.table)
library(dplyr)
library(maptools)
library(bit64)
library(curl)

wnv_df <- fread("https://raw.githubusercontent.com/rsangole/capstone_project/master/data/processed/wnv.trap.date.rev3b.csv")


train_df <- wnv_df %>% 
  dplyr::select(part_train, t_date)  

min_date <- lubridate::ymd(min(train_df$t_date))
max_date <- lubridate::ymd(max(train_df$t_date))



ui <- dashboardPage(
  dashboardHeader(title = "West Nile Virus"),
  dashboardSidebar(
    sliderInput("datesSlider",
                "Date Range:",
                min = min_date,
                max = max_date,
                value = c(min_date,max_date),
                step=7,
                animate=FALSE
    ), #end sliderInput
    actionButton("clear", "Clear Markers"),
    #),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Cluster Map", tabName = "myMap"),
      menuItem("Heat Map", tabName = "myHeatMap")
    ) # end sidebarMenu
  ), #end dashboardSidebar
  dashboardBody(
    
    ### ----- start dashboard section -----###
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("wnvrate"),
                valueBoxOutput("mos_count"),
                #shinydashboard::valueBoxOutput("pred_wnv"),
                valueBoxOutput("hosp_kpi")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Confirmed Cases of West Nile virus in Chicago",
                  splitLayout(cellWidths = c("50%", "50%"),
                              plotOutput( outputId = "wnvcount"),
                              plotOutput( outputId = "wnv_zip")
                  ) #end splitLayout
                ), #end box
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Mosquitos Trapped Density Map",
                  #tableOutput("packageTable1")
                  plotOutput(outputId = "chiMap")
                ) #end box
              ) #end fluidRow
      ), #end tabItem - dashboard
      ### ----- end dashboard section -----###
      ### ----- start cluster map section -----###
      tabItem("myMap",
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Cluster Map",
                  leaflet::leafletOutput( outputId = "myMap",width="100%",height="500px")
                ), #end box
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Trap History",
                  plotOutput(outputId = "clusterPlot2")
                ) #end box
              ) #end fluidRow
      ), #end tabItem - dashboard
      ### ----- end cluster map section -----###
      ### ----- start heat map section -----###
      tabItem("myHeatMap",
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Concentration of West Nile virus",
                  leaflet::leafletOutput( outputId = "myHeatMap",width="100%",height="500px")
                ) #end box
              ) #end fluidRow
              #leaflet::leafletOutput( outputId = "myHeatMap",width="100%",height="500px")
      )#end tabItem - myHeatMap
      ### ----- end heat map section -----###
      ### ----- start tree map section -----###
      
      # ) # end tabItem - Treemap
    ) #end tabItems
  ) #end dashboardBody
) # end dashboardPage


