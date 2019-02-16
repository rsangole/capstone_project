





################################################################################
## EDA preparatory to restructuring
################################################################################

# noaa.weather.stations
noaa.weather.stations

str(weather.all)

# summary(weather.all[,regexpr('ATTRIBUTES',colnames(weather.all)) > 0])
# ftable(weather.all[,regexpr('ATTRIBUTES',colnames(weather.all)) > 0])
# 
# lapply(colnames(weather.all)[regexpr('ATTRIBUTES',colnames(weather.all)) > 0][1]
#       ,FUN=function(x) {summary(weather.all[,x])})
# 
# lapply(colnames(weather.all)[regexpr('ATTRIBUTES',colnames(weather.all)) > 0]
#        ,FUN=function(x) {summary(weather.all[,x])})


# lapply(colnames(weather.all)[regexpr('ATTRIBUTES',colnames(weather.all)) > 0]
#        ,FUN=function(x) {ftable(weather.all[,x])})


noaa.weather.stations
# table(noaa.weather.stations$NAME)
table(noaa.weather.stations[
  regexpr('MIDWAY',noaa.weather.stations$NAME) > 0
  | regexpr('HARE',noaa.weather.stations$NAME) > 0,]$NAME)
table(noaa.weather.stations[
  regexpr('MIDWAY',noaa.weather.stations$NAME) > 0
  | regexpr('HARE',noaa.weather.stations$NAME) > 0,]$STATION)


summary(noaa.weather.stations)
noaa.colnames <- colnames(noaa.weather.stations)[regexpr('\\.n',colnames(noaa.weather.stations)) > 0]
# noaa.weather.stations[, noaa.colnames]
lapply(noaa.colnames,FUN=function(x) {print(paste(x,' has at least one value for:',noaa.weather.stations[
  noaa.weather.stations[,x] > 0,"NAME"]),sep='')})

# AWND -- airports only

# ACMC = Average cloudiness midnight to midnight from 30-second ceilometer data (percent)
# ACMH = Average cloudiness midnight to midnight from manual observations (percent)
# ACSC = Average cloudiness sunrise to sunset from 30-second ceilometer data (percent)
# ACSH = Average cloudiness sunrise to sunset from manual observations (percent)
# AWND = Average daily wind speed (meters per second or miles per hour as per user preference)
# DAEV = Number of days included in the multiday evaporation total (MDEV)
# DAPR = Number of days included in the multiday precipitation total (MDPR)
# DASF = Number of days included in the multiday snowfall total (MDSF)
# DATN = Number of days included in the multiday minimum temperature (MDTN)
# DATX = Number of days included in the multiday maximum temperature (MDTX)
# DAWM = Number of days included in the multiday wind movement (MDWM)
# DWPR = Number of days with non-zero precipitation included in multiday precipitation total (MDPR)
# EVAP = Evaporation of water from evaporation pan (mm or inches as per user preference, or hundredths
#                                                   of inches on Daily Form pdf file)
# FMTM = Time of fastest mile or fastest 1-minute wind (hours and minutes, i.e., HHMM)
# FRGB = Base of frozen ground layer (cm or inches as per user preference)
# FRGT = Top of frozen ground layer (cm or inches as per user preference)
# FRTH = Thickness of frozen ground layer (cm or inches as per user preference)
# GAHT = Difference between river and gauge height (cm or inches as per user preference)
# MDEV = Multiday evaporation total (mm or inches as per user preference; use with DAEV)
# MDPR = Multiday precipitation total (mm or inches as per user preference; use with DAPR and DWPR, if
#                                      available)
# MDSF = Multiday snowfall total (mm or inches as per user preference)
# MDTN = Multiday minimum temperature (Fahrenheit or Celsius as per user preference ; use with DATN)
# MDTX = Multiday maximum temperature (Fahrenheit or Celsius as per user preference ; use with DATX)
# MDWM = Multiday wind movement (miles or km as per user preference)
# MNPN = Daily minimum temperature of water in an evaporation pan (Fahrenheit or Celsius as per user
#                                                                  preference)
# MXPN = Daily maximum temperature of water in an evaporation pan (Fahrenheit or Celsius as per user
#                                                                  preference)
# PGTM = Peak gust time (hours and minutes, i.e., HHMM)
# PSUN = Daily percent of possible sunshine (percent)
# SN*# = Minimum soil temperature where * corresponds to a code
#   for ground cover and # corresponds to a code for soil depth (Fahrenheit or Celsius as per user
# preference)

psun <- noaa.weather[!is.na(noaa.weather$PSUN),]
noaa.weather$PGTM
noaa.weather$PRCP
noaa.weather.stations$PRCP.n

summary(weather.stations.3)
  
noaa.weather <- left_join(ref.dates
                          ,weather.all[
                            regexpr('MIDWAY',weather.all$NAME) > 0
                            | regexpr('OHARE',weather.all$NAME) > 0
                            ,!colnames(weather.all) %in% 
                                         c("LATITUDE","LONGITUDE"
                                           ,"ELEVATION"
                                           ,colnames(weather.all)[regexpr('ATTRIBUTES',colnames(weather.all)) > 0])]
                          ,by=c("date"="date")) %>% 
  arrange(date,NAME)


## Pick two weather stations.  Compile all WNV trap dates.  For each date, 
## join to weather data and get lagged data at various points.  





################################################################################
## Just get Midway & O'Hare and see how variables compare
################################################################################

# weather.subset <- weather.all[1:200,c("STATION","date","PRCP","TMAX","TMIN","TAVG","tavg2")]
# weather.subset2 <- weather.subset %>% 
#   spread(STATION,PRCP)
# 
# df %>% 
#   gather(variable, value, -(month:student)) %>%
#   unite(temp, student, variable) %>%
#   spread(temp, value)
# 
# 
# df <- data.frame(month=rep(1:3,2),
#                  student=rep(c("Amy", "Bob"), each=3),
#                  A=c(9, 7, 6, 8, 6, 9),
#                  B=c(6, 7, 8, 5, 6, 7))
# 
# 
# weather.all$tavg2 <- ifelse(!is.na(weather.all$TAVG)
#                             ,weather.all$TAVG
#                             ,(weather.all$TMIN + weather.all$TMAX)/2)

chgo.airports.wide <- weather.all[weather.all$STATION 
                            %in% c("USW00014819","USW00094846")
  ,c("STATION","date","PRCP","TMAX","TMIN","tavg2")] %>% 
  gather(variable,value,-c(STATION,date)) %>%
  unite(station.variable, STATION, variable) %>%
  spread(station.variable,value)

summary(chgo.airports.wide)


chk.miss <- anti_join(ref.dates,chgo.airports.wide,by=c("date")) # only 12/31/2018 missing from weather
chk.miss2 <- anti_join(chgo.airports.wide,ref.dates,by=c("date")) # no missing data

chgo.airports.wide <- inner_join(ref.dates
                                 ,chgo.airports.wide
                                 ,by=c("date"))
# --n=4747

summary(chgo.airports.wide)


chk.miss3 <- chgo.airports.wide[rowSums(is.na(chgo.airports.wide)) > 0,] 
## n=38 rows.  Mostly Nov 2018 and later data, but a few points missing earlier.


################################################################################
## Impute missing values... via "Multiple Imputation by Chained Equations"
## using mice package in R
## Run 5 iterations and take mean of those five
################################################################################

require(mice)

miss.data.pat <- md.pattern(chgo.airports.wide)

# imputed.via.predmean <- mice(chgo.airports.wide, m=5, maxit = 30, method = 'pmm', seed = 500)
imputed.via.predmean <- mice(chgo.airports.wide, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(imputed.via.predmean)
summary(imputed.via.predmean$imp)
summary(imputed.via.predmean$imp$USW00014819_PRCP)
str(imputed.via.predmean$imp$USW00014819_PRCP)
chk <- imputed.via.predmean$imp$USW00014819_PRCP


chgo.airports.wide.imp <- chgo.airports.wide


chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00014819_PRCP)
                   ,]$USW00014819_PRCP <-
  imputed.via.predmean$imp$USW00014819_PRCP[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00014819_tavg2)
                   ,]$USW00014819_tavg2 <-
  imputed.via.predmean$imp$USW00014819_tavg2[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00014819_TMAX)
                   ,]$USW00014819_TMAX <-
  imputed.via.predmean$imp$USW00014819_TMAX[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00014819_TMIN)
                   ,]$USW00014819_TMIN <-
  imputed.via.predmean$imp$USW00014819_TMIN[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00094846_PRCP)
                   ,]$USW00094846_PRCP <-
  imputed.via.predmean$imp$USW00094846_PRCP[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00094846_tavg2)
                   ,]$USW00094846_tavg2 <-
  imputed.via.predmean$imp$USW00094846_tavg2[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00094846_TMAX)
                   ,]$USW00094846_TMAX <-
  imputed.via.predmean$imp$USW00094846_TMAX[[1]]

chgo.airports.wide.imp[rownames(imputed.via.predmean$imp$USW00094846_TMIN)
                   ,]$USW00094846_TMIN <-
  imputed.via.predmean$imp$USW00094846_TMIN[[1]]

# summary(chgo.airports.wide.imp)

date.metadata <- colnames(ref.dates)
date.metadata <- date.metadata[!date.metadata %in% c("date")]

chgo.airports.wide.imp <- 
  chgo.airports.wide.imp[,
                         !colnames(chgo.airports.wide.imp) %in% date.metadata]
chi.weather.v1 <- chgo.airports.wide.imp


################################################################################
## Save updated station data
################################################################################

save(chi.weather.v1, file="chi.weather.v1.RData", compress = FALSE)

write.csv(chi.weather.v1, paste(weather.path,'chi.weather.v1.csv',sep='\\'),row.names = FALSE)




################################################################################
## Cycle through weather variables.  For each, pull out the subset of 
## stations with nearly-complete data
################################################################################


# chk <- weather.all %>% group_by(STATION) %>% summarise(n=n(),n.date = n_distinct(date))
weather.all %>% group_by(STATION) %>% 
  summarise(n=n(),n.date = n_distinct(date)) %>% summary(.)
# Max # of observations is 4748


get.stations.nearly.complete <- function(col, df = weather.all,threshold=0.95)  {
  col.pcts <- df %>% group_by(STATION) %>%
    summarise(n=n()
              ,dates=n_distinct(date)
              ,n.nonmiss = sum(!is.na(!! sym(col)))
              ,pct.nonmiss=sum(!is.na(!! sym(col)))/4748
              ,n.miss=sum(is.na(!! sym(col)))
              ,pct.miss=sum(is.na(!! sym(col)))/4748
    ) %>% 
    mutate(keep = pct.nonmiss >= threshold)
  df.out <- df %>% semi_join(col.pcts %>% filter(keep),by=("STATION")) %>% 
    dplyr::select(STATION,date,col) %>% gather(attribute
                                               , value
                                               , (!! sym(col))
                                               , factor_key=FALSE)
  return(df.out)
}

# chk <-  get.stations.nearly.complete("AWND")
# chk <-  get.stations.nearly.complete("SNOW")

weather.subset <- lapply(
  # colnames(weather.all[,-(1:6)] %>% dplyr::select(-contains('attrib')) %>% .[,1:10])
  # colnames(weather.all[,-(1:6)] %>% dplyr::select(-contains('attrib')) )
  colnames(weather.all[,-(1:6)] %>% dplyr::select(c(-contains('attrib')),-location,-yr,-mo,-date2,-date) )
  ,get.stations.nearly.complete
)

# str(weather.subset)

weather.subset2 <- do.call("rbind", weather.subset)

str(weather.subset2)
summary(weather.subset2)

weather.attributes <- weather.subset2 %>% group_by(attribute) %>% 
  summarise(n=n()
            ,stations = n_distinct(STATION)
            ,dates = n_distinct(date)
            ,n.nonmissing = sum(!is.na(value))
            ,n.missing = sum(is.na(value)))
weather.chk.stations <- weather.subset2 %>% group_by(STATION) %>% 
  summarise(n=n()
            ,attributes = n_distinct(attribute)
            ,dates = n_distinct(date)
            ,n.nonmissing = sum(!is.na(value))
            ,n.missing = sum(is.na(value)))
weather.chk.stations.attr <- weather.subset2 %>% group_by(attribute,STATION) %>% 
  summarise(n=n()
            ,stations = n_distinct(STATION)
            ,attributes = n_distinct(attribute)
            ,dates = n_distinct(date)
            ,n.nonmissing = sum(!is.na(value))
            ,n.missing = sum(is.na(value)))
weather.chk.stations.attr2 <- left_join(weather.chk.stations.attr,
                                        weather.all %>% group_by(STATION,NAME,
                                                                 location) %>%
                                          summarise(n=n()) %>%
                                          dplyr::select(-n)
                                        ,by="STATION")

weather.subset.wide <- weather.subset2 %>% arrange(attribute,STATION,date) %>%
  group_by(STATION,date) %>%
  # gather(variable,value,-c(STATION,date)) %>%
  unite(station.attribute, STATION, attribute) %>%
  spread(station.attribute,value)

# summary(weather.subset.wide)

# sum.is.na <- function(x) {sum(is.na(x))}
# weather.subset.wide %>% summarise_all(.funs=c(sum(is.na),n))
weather.subset.wide.summary <- weather.subset.wide %>% ungroup() %>%
  summarise_all(funs(cnt = sum(!is.na(.)), sum = sum(.,na.rm=TRUE)))

weather.subset.wide2 <- as.data.frame(weather.subset.wide)


################################################################################
## Daily temp/precip weather data -- all stations
################################################################################


weather.all$tavg2 <- ifelse(is.na(weather.all$TAVG)
                            ,(weather.all$TMIN + weather.all$TMAX)/2
                            ,weather.all$TAVG)

weather.wide <- weather.all %>% 
  dplyr::select(-ends_with('ATTRIBUTES')) %>%
  group_by(STATION,date) %>%
  dplyr::select(c(starts_with('t'),contains('prcp'))) %>%
  gather(variable,value,-c(STATION,date)) %>%
  unite(station.variable, STATION, variable) %>%
  spread(station.variable,value)

summary(weather.all %>% dplyr::select(-ends_with('ATTRIBUTES')))

# kdepairs(weather.wide[rowSums(weather.wide,is.na) == 0,-c(1)])

# weather.wide %>% ungroup() %>% dplyr::select(contains("tavg2")) %>%
#   # .[., colSums(. != 0) > 0] %>%
#   select(which(!colSums(., na.rm=TRUE) %in% 0))
#   dplyr::select(colSums(is.na(.) == 0)) %>%
#   # filter(rowSums(is.na(.)) == 0) %>% 
#   kdepairs()

getwd()
setwd(paste("D:","ajc188","github","capstone_project","data","raw",sep='\\'))
getwd() 

weather_wide <- weather.wide %>%
  setNames(tolower(gsub(" ","_",names(.))) )


save(weather_wide, file="weather_wide.RData", compress = FALSE)
write.csv(weather_wide, 'weather_wide.csv',row.names = FALSE)


################################################################################
## Try imputing missing values for weather data
################################################################################

require(mice)

# weather.wide.imp <- weather.wide
imputed.via.predmean <- mice(weather.wide, m=5, maxit = 10, method = 'pmm', seed = 500)

str(imputed.via.predmean)

extract.mean <- function(x,imp.obj) {
  imputations <- imp.obj$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values <- function(x,imp.obj) {
  means <- extract.mean(x,imputed.via.predmean)
  weather.wide.imp[as.integer(names(means)),x] <- means
  return(weather.wide.imp)
}

weather.wide.imp <- weather.wide

for(col in names(imputed.via.predmean$imp)) {
  weather.wide.imp <- update.values(col,imputed.via.predmean)
}

rowSums(is.na(weather.wide[,-1]))
rowSums(is.na(weather.wide.imp[,-1]))

rowSums(!is.na(weather.wide[,-1]))
rowSums(!is.na(weather.wide.imp[,-1]))


str(weather.wide.imp)

summary(weather.wide.imp %>% dplyr::select(matches('tavg2')) %>% 
          select(matches())
          .[,1:20])


################################################################################
## Create distance matrix (weather stations to trap locations)
################################################################################

wea2trap <- inner_join(
  cbind(noaa.weather.stations[,c("STATION","NAME","LATITUDE","LONGITUDE")],dummy=1)
  ,cbind(wnv.traps[,c("trap.name","rev.lat","rev.lng")],dummy=1)
) %>% dplyr::select(-dummy) %>% ## rowwise() %>% 
  mutate(wea.dist.m = 
    geosphere::distHaversine(p1=cbind(LONGITUDE,LATITUDE)
                             ,p2=cbind(as.numeric(rev.lng),as.numeric(rev.lat))
    )) %>%
  mutate(wea.dist.ft = wea.dist.m * 3.28084) %>%
  mutate(wea.dist.mi = wea.dist.ft / 5280) %>%
  dplyr::select(-c(wea.dist.ft)) %>%
  dplyr::select(-c(LATITUDE,LONGITUDE,rev.lng,rev.lat,wea.dist.m)) 

wea_dist_matrix <- wea2trap %>%
  setNames(tolower(gsub(" ","_",names(.))) )

  
getwd()
setwd(paste("D:","ajc188","github","capstone_project","data","raw",sep='\\'))
getwd() 


save(wea_dist_matrix, file="wea_dist_matrix.RData", compress = FALSE)
write.csv(wea_dist_matrix, 'wea_dist_matrix.csv',row.names = FALSE)



################################################################################
## Rank weather stations in order of closest station to each trap
## Limit to stations with nearly-complete temp/precipitation data
################################################################################


wea2trap.2 <- wea2trap %>%
  arrange(trap.name,wea.dist.mi) %>%
  group_by(trap.name) %>%
  mutate(wea.seqn = row_number()) 
# summary(wea2trap.2$wea.dist.ft)

# head(wea2trap)

nearest.stations <- wea2trap.2 %>% filter(wea.seqn==1) %>% dplyr::select(-wea.seqn)




################################################################################
## Rank weather stations in order of closest station to each trap
## Limit to stations with nearly-complete temp/precipitation data
################################################################################

summary(noaa.weather.stations)


PRCP.stns <- noaa.weather.stations[noaa.weather.stations$PRCP.n >= 4500,]
tavg2.stns <- noaa.weather.stations[noaa.weather.stations$tavg2.n >= 4500,]

nearest.PRCP <- semi_join(wea2trap,PRCP.stns,by=c("STATION")) %>%
  arrange(trap.name,wea.dist.mi) %>%
  group_by(trap.name) %>%
  mutate(wea.seqn = row_number()) %>% filter(wea.seqn==1) %>% 
  dplyr::select(-wea.seqn)

nearest.TEMP <- semi_join(wea2trap,tavg2.stns,by=c("STATION")) %>%
  arrange(trap.name,wea.dist.mi) %>%
  group_by(trap.name) %>%
  mutate(wea.seqn = row_number()) %>% filter(wea.seqn==1) %>% 
  dplyr::select(-wea.seqn)

# I'm inclined to use nearest weather station for precipitation but not 
# temperature.  The latter could give very incorrect estimates if you match
# to a station close to the lake (e.g. Northerly Island) but the trap is 
# actually not.





nearest_temps <- inner_join(wnv.traps,nearest.TEMP) %>%
  inner_join(weather.all,by=c("STATION")) %>% 
  dplyr::select(-ends_with('ATTRIBUTES')) %>%
  group_by(trap.name,date) %>%
  dplyr::select(STATION,tavg2,wea.dist.mi) %>%
  dplyr::rename(nearest_temp = tavg2
                ,nearest_temp_station = STATION
                ,nearest_temp_dist_mi = wea.dist.mi)

nearest_temps %>% ungroup() %>% summarise(n=n(),n.traps = n_distinct(trap.name)
                                          ,n.stations = n_distinct(nearest_temp_station)
                                          ,n.dates = n_distinct(date)
                                          ,min.dist = min(nearest_temp_dist_mi)
                                          ,max.dist = max(nearest_temp_dist_mi)
                                          ,mean.dist = mean(nearest_temp_dist_mi))


nearest_precips <- inner_join(wnv.traps,nearest.PRCP) %>%
  inner_join(weather.all,by=c("STATION")) %>% 
  dplyr::select(-ends_with('ATTRIBUTES')) %>%
  group_by(trap.name,date) %>%
  dplyr::select(STATION,PRCP,wea.dist.mi) %>%
  dplyr::rename(nearest_prcp = PRCP
                ,nearest_prcp_station = STATION
                ,nearest_prcp_dist_mi = wea.dist.mi)

nearest_precips %>% ungroup() %>% summarise(n=n(),n.traps = n_distinct(trap.name)
                                          ,n.stations = n_distinct(nearest_prcp_station)
                                          ,n.dates = n_distinct(date)
                                          ,min.dist = min(nearest_prcp_dist_mi)
                                          ,max.dist = max(nearest_prcp_dist_mi)
                                          ,mean.dist = mean(nearest_prcp_dist_mi))

nearest_precips_imp 
summary(nearest_precips)
summary(nearest_temps)


################################################################################
## Add this station data to our dataset
################################################################################

df$t_date <- as.Date(df$t_date) 

df <- left_join(df,nearest_temps,by=c("trap_trap_name" = 'trap.name',"t_date" = "date"))
df <- left_join(df,nearest_precips,by=c("trap_trap_name" = "trap.name","t_date" = "date"))


kdepairs(df %>% dplyr::select(nearest_prcp,wea_midway_prcp,wea_ohare_prcp))


getwd()
setwd(paste("D:","ajc188","github","capstone_project","data","processed",sep='\\'))
getwd() 

save(df, file="df.RData", compress = FALSE)
write.csv(df, 'df.csv',row.names = FALSE)




################################################################################
## Quick EDA
################################################################################

popular.stations <- wea2trap.2 %>% filter(wea.seqn==1) %>% 
  group_by(STATION,NAME) %>%
  summarise(nrows=n()
            ,min.wea.dist.mi = min(wea.dist.mi)
            ,max.wea.dist.mi=max(wea.dist.mi)
            ,mean.wea.dist.mi=mean(wea.dist.mi)
            ,sd.wea.dist.mi=sd(wea.dist.mi)
            ) %>% 
  arrange(-nrows)
popular.stations





