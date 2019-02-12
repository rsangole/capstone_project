

################################################################################
## Identify files to load
################################################################################

# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\github\\capstone_project\\data\\raw"
weather.path <- paste(base.path,'chi_weather',sep='\\')

filename.list <- list.files(path = weather.path)           
# filename.list
weather.files <- filename.list[grep('Chicago area weather',filename.list)]
weather.files


################################################################################
## Load NOAA weather files
################################################################################

weather.1 <- read.csv(paste(weather.path,weather.files[1],sep="\\"),stringsAsFactors = FALSE)
weather.2 <- read.csv(paste(weather.path,weather.files[2],sep="\\"),stringsAsFactors = FALSE)
weather.3 <- read.csv(paste(weather.path,weather.files[3],sep="\\"),stringsAsFactors = FALSE)
weather.4 <- read.csv(paste(weather.path,weather.files[4],sep="\\"),stringsAsFactors = FALSE)
weather.5 <- read.csv(paste(weather.path,weather.files[5],sep="\\"),stringsAsFactors = FALSE)
weather.6 <- read.csv(paste(weather.path,weather.files[6],sep="\\"),stringsAsFactors = FALSE)
weather.7 <- read.csv(paste(weather.path,weather.files[7],sep="\\"),stringsAsFactors = FALSE)


################################################################################
## Combine NOAA weather files
################################################################################

str(weather.1)
summary(weather.1$DATE)

# Since I accidentally downloaded 2011 twice, we need to exclude it from one set.
weather.4$date <- as.Date(weather.4$DATE)
summary(weather.4$date)

weather.all <- bind_rows(
  weather.1,weather.2,weather.3
  ,weather.4[weather.4$date >= '2012-01-01',!colnames(weather.4) %in% c("date")]
  ,weather.5,weather.6,weather.7
)

rm(weather.1);rm(weather.2);rm(weather.3);rm(weather.4);rm(weather.5)
rm(weather.6);rm(weather.7)

weather.all$date <- as.Date(weather.all$DATE)
weather.all$yr <- lubridate::year(weather.all$date)
weather.all$mo <- lubridate::month(weather.all$date)
weather.all$date2 <- as.POSIXct(weather.all$DATE)

# str(weather.all)

# Remove columns that are completely missing for all observations
weather.all <- weather.all[, unlist(lapply(weather.all, function(x) !all(is.na(x))))]



################################################################################
## Create a station-level summary
################################################################################


stations <- weather.all %>% group_by(STATION,NAME,LATITUDE,LONGITUDE) %>%
  summarise(nrows = n())

station.yr <- weather.all %>% group_by(STATION,NAME,LATITUDE,LONGITUDE,yr) %>%
  summarise(nrows = n())

station.yr2 <- station.yr %>% group_by(STATION,NAME,LATITUDE,LONGITUDE) %>%
  spread(yr,nrows)
rm(station.yr)

stations <- inner_join(stations,station.yr2) 
rm(station.yr2)
stations <- stations %>% arrange(-nrows)



################################################################################
## Reshape weather data
################################################################################

# str(weather.all)

weather.all$location <- paste('(',weather.all$LATITUDE,',',weather.all$LONGITUDE,')',sep='')

weather.stations <- weather.all[,c("STATION","NAME","LATITUDE","LONGITUDE","ELEVATION","location")] %>% 
  group_by(STATION) %>% summarise_all(n_distinct)
summary(weather.stations)
# All of these columns are unique for given stations.  Good.

weather.stations.1 <- 
  weather.all[,regexpr('ATTRIBUTE',colnames(weather.all)) < 0] %>% 
  group_by(STATION,NAME,location,LATITUDE,LONGITUDE,ELEVATION) %>% 
  summarise(nrows=n(),min.date = min(date),max.date = max(date))
weather.stations.2 <- 
  weather.all[,regexpr('ATTRIBUTE',colnames(weather.all)) < 0
              & !colnames(weather.all) %in% c("DATE","date","date2","yr","mo")] %>% 
  group_by(STATION,NAME,location,LATITUDE,LONGITUDE,ELEVATION) %>% 
  summarise_all(funs(sum(!is.na(.)))) 

# Calculate percentage of daily observations that are non-missing
calc.pct <- function(x,denom) {100*x/denom}
weather.stations.3 <- cbind(  as.data.frame(weather.all[,regexpr('ATTRIBUTE',colnames(weather.all)) < 0
                                               & !colnames(weather.all) %in% c("DATE","date","date2","yr","mo")] %>% 
                                     group_by(STATION,NAME,location,LATITUDE,LONGITUDE,ELEVATION) %>%
                                     summarise(nrows=n()) %>% ungroup() %>% summarise(nrows = max(nrows)))
                                     ,as.data.frame(weather.stations.2)) %>%
  group_by(STATION,NAME,location,LATITUDE,LONGITUDE,ELEVATION,nrows) %>%
  summarise_all(funs(calc.pct(.,nrows)))

is.above.95.pct <- function(x) {x >= 0.95}
weather.stations.4 <- weather.stations.3 %>% 
  group_by(STATION,NAME,location,LATITUDE,LONGITUDE,ELEVATION,nrows) %>%
  summarise_all(funs(is.above.95.pct(.)))
summary(weather.stations.4)


# AWND -> 3 stations
# DAPR -> days included in multiday precipitation - skip
# DASF multiday snowfall
# EVAP - evaporation of water from pan -- maYYYYYbe; n=1
# FMTM - time of fastest wind (n=1)
# MDPR - multi-day precipitation? n=1
# MDSF n=0
# MPNP evap pan min temp n=1
# MXPN evap pan max temp n=1
# PGTM peak gust time n = 3
# PRCP, n=302
# SN32, soil temp bare ground... n=2
# SNOW n = 281
# SNWD

weather.stations <- inner_join(weather.stations.1,weather.stations.2)
summary(weather.stations)

dim(weather.stations)
colnames(weather.stations) <- c(
  colnames(weather.stations[1:9])
  , paste(colnames(weather.stations[10:61]),'n',sep='.')
  )

# str(weather.stations)
noaa.weather.stations <- weather.stations


################################################################################
## Save R dataset with minimal modifications
################################################################################

noaa.weather.top20stn <- weather.all[weather.all$STATION %in% 
                              stations$STATION[1:20],] 

getwd()
setwd(weather.path)
getwd()


# save(westnile.unedited, file="pump.unedited.RData", compress = FALSE)
# save(westnile.all, file="westnile.all.RData", compress = FALSE)
# save(westnile.eda, file="westnile.eda.RData", compress = FALSE)

save(noaa.weather.top20stn, file="noaa.weather.top20stn.RData", compress = FALSE)

write.csv(noaa.weather.top20stn, paste(my.path,'noaa.weather.top20stn.csv',sep='\\'),row.names = FALSE)



save(noaa.weather.stations, file="noaa.weather.stations.RData", compress = FALSE)

write.csv(noaa.weather.stations, paste(weather.path,'noaa.weather.stations.csv',sep='\\'),row.names = FALSE)



################################################################################
## Convert coordinates to a shapefile in ESRI ArcGIS then to a layer file
## then perform spatial join (intersection) with block group shapefile, then 
## convert resulting shapefile's tabular component (DBF) to CSV.  Finally, 
## read back into R.  
## Repeat for Chicago community area and Zip Code Tabulation Area (ZCTA).
################################################################################

stn.zcta <- read.csv(paste(weather.path,'GIS','noaa_weather_stations_zcta.csv',sep="\\"),stringsAsFactors = FALSE)
stn.blkgrp <- read.csv(paste(weather.path,'GIS','noaa_weather_stations_BG2018.csv',sep="\\"),stringsAsFactors = FALSE)
stn.community.area <- read.csv(paste(weather.path,'GIS','noaa_weather_stations_CommArea.csv',sep="\\"),stringsAsFactors = FALSE)

str(stn.zcta)
summary(stn.zcta$ZCTA5CE10)

str(stn.blkgrp)
summary(stn.blkgrp$STATEFP)
summary(stn.blkgrp$COUNTYFP)
summary(stn.blkgrp$TRACTCE)
summary(stn.blkgrp$BLKGRPCE)
summary(stn.blkgrp$GEOID)

str(stn.community.area)
summary(stn.community.area$community)
ftable(stn.community.area$community)

wnv.weather.stations.geo <- left_join(noaa.weather.stations,stn.zcta[,c("STATION","ZCTA5CE10")]
                           ,by=c("STATION"))
wnv.weather.stations.geo2 <- left_join(wnv.weather.stations.geo,stn.blkgrp[,c("STATION","STATEFP"
                                                       ,"COUNTYFP"
                                                       ,"TRACTCE"
                                                       ,"BLKGRPCE"
                                                       ,"GEOID")]
                            ,by=c("STATION"))
wnv.weather.stations.geo3 <- left_join(wnv.weather.stations.geo2,stn.community.area[,c("STATION","community")]
                            ,by=c("STATION"))

noaa.weather.stations <- wnv.weather.stations.geo3

summary(noaa.weather.stations)


################################################################################
## Save updated station data
################################################################################


save(noaa.weather.stations, file="noaa.weather.stations.RData", compress = FALSE)

write.csv(noaa.weather.stations, paste(weather.path,'noaa.weather.stations.csv',sep='\\'),row.names = FALSE)



