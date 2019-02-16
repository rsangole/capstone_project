

################################################################################
## Identify files to load

# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\github\\capstone_project"
weather.path <- paste(base.path,'data','raw','chi_weather_frontier',sep='\\')

filename.list <- list.files(path = weather.path)           
# filename.list
weather.files <- filename.list[grep('Chicago area weather',filename.list)]



################################################################################
## Load NOAA weather files


# weather.other.1 <- read.csv(paste(weather.path,"CDO6172217830671.txt",sep="\\"),stringsAsFactors = FALSE)
# weather.other.2 <- read.csv(paste(weather.path,"CDODiv1391317830653.txt",sep="\\"),stringsAsFactors = FALSE)

frontier.weather.1 <- read.csv(paste(weather.path,"KMDW_daily.txt",sep="\\"),stringsAsFactors = FALSE)
frontier.weather.2 <- read.csv(paste(weather.path,"KORD_daily.txt",sep="\\"),stringsAsFactors = FALSE)

# Midway and O'Hare weather data from Frontier Weather Inc look pretty solid.

# str(frontier.weather.1)
# str(frontier.weather.2)

frontier.weather <- rbind(frontier.weather.1,frontier.weather.2)

rm(frontier.weather.1); rm(frontier.weather.2)


################################################################################
## Calculate average temp


frontier.weather$Avg.Temp2 <- (frontier.weather$Min.Temp + frontier.weather$Max.Temp) / 2

kdepairs(frontier.weather[!(is.na(frontier.weather$Min.Temp) | 
                         is.na(frontier.weather$Max.Temp) | 
                         is.na(frontier.weather$Avg.Temp))
                     ,colnames(frontier.weather) %in% c("Min.Temp","Max.Temp","Avg.Temp","Avg.Temp2")])

# Avg.Temp is not average hourly temperature.  It's simply average 
# of min and max daily temperature.  


str(frontier.weather)


################################################################################
## Save R dataset with minimal modifications

getwd()
setwd(my.path)
getwd()


save(frontier.weather, file="frontier.weather.RData", compress = FALSE)

write.csv(frontier.weather, paste(my.path,'frontier.weather.csv',sep='\\'),row.names = FALSE)



################################################################################
## Just get Heating Degree Days (HDDs) and Cooling Degree Days (CDDs)

frontier_weather <- frontier.weather %>%
  setNames(tolower(paste(gsub('\\.','_',names(.))
                         ,sep='_'
  ))) %>% 
  rename(t_date = date) %>%
  mutate(station = ifelse(site4 == 'KMDW'
                              ,"midway"
                              ,ifelse(site4 == "KORD",'ohare','other'))
  ) %>% dplyr::select(-site4) 
# ftable(frontier_weather$wea_site4)
# ftable(frontier_weather$wea_station)



################################################################################
## Long to wide
## Also, convert character to numeric

frontier_weather_wide <- frontier_weather %>% 
  gather(variable,value,-c(station,t_date)) %>%
  unite(station.variable, station, variable) %>%
  spread(station.variable,value) %>%
  setNames(tolower(paste('wea'
                         ,names(.)
                         ,sep='_'
  ))) %>%
  rename(t_date = wea_t_date) %>%
  arrange(t_date) %>% 
  mutate(t_date = as.Date(t_date,format='%m/%d/%Y %t')) %>%
  group_by(t_date) %>%
  mutate_all(funs(as.numeric(.))) 

# Subset to our date range and variables of interest
frontier_vars <- frontier_weather_wide %>% 
  dplyr::select(c(t_date,contains('hdd'),matches('cdd'))) %>%
  right_join(.,ref.dates,by=c("t_date"="date")) %>%
  dplyr::select(t_date,wea_midway_hdds,wea_midway_cdds
                ,wea_ohare_hdds,wea_ohare_cdds) 

summary(frontier_vars)
summary(frontier_vars %>% filter(t_date < '2018-11-01'))


################################################################################
## Impute missing data if need be:  not needed, all missing data are after
## October 2018


################################################################################
## Save data 

wea_frontier <- frontier_vars

save(wea_frontier, file="wea_frontier.RData", compress = FALSE)
write.csv(wea_frontier, paste(my.path,'wea_frontier.csv',sep='\\'),row.names = FALSE)






