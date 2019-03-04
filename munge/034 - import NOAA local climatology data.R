

################################################################################
## Identify files to load
################################################################################

# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\github\\capstone_project\\data\\raw"
weather.path <- paste(base.path,'chi_weather',sep='\\')

filename.list <- list.files(path = weather.path)           
# filename.list
weather.files <- filename.list[grep('1617',filename.list)]
weather.files


################################################################################
## Load NOAA weather files (Local Climatological Data [LCD])
################################################################################

lcd.1 <- read.csv(paste(weather.path,"1617130_2007to10.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.2 <- read.csv(paste(weather.path,"1617146_2010to14.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.3 <- read.csv(paste(weather.path,"1617148_2015to19.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.4 <- read.csv(paste(weather.path,"1617149_2011to14.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.5 <- read.csv(paste(weather.path,"1617195_2006.csv",sep="\\"),stringsAsFactors = FALSE)


local.climatological.data <- rbind(lcd.1,lcd.3,lcd.4,lcd.5)

rm(lcd.1); rm(lcd.2); rm(lcd.3); rm(lcd.4); rm(lcd.5)

# These data may need a lot of work to pull out variables we want.  
# Rows are hourly or something like that.  Some columns are populated 
# only daily or at other intervals.  


################################################################################
## Pull out the daily data elements

# summary(local.climatological.data)

# summary(local.climatological.data$DAILYWeather)
# table(local.climatological.data$DAILYWeather)
# summary(local.climatological.data$DAILYMaximumDryBulbTemp)
# table(local.climatological.data$DAILYMaximumDryBulbTemp)
# table(local.climatological.data$DAILYAverageDryBulbTemp)
# table(local.climatological.data$DAILYAverageDryBulbTemp)
# table(local.climatological.data$DAILYDeptFromNormalAverageTemp)

daily <- local.climatological.data %>% 
  dplyr::select(-matches("HOURLY")) %>%
  dplyr::select(-matches("MONTHLY")) %>%
  mutate(date = as.Date(DATE)) %>%
  dplyr::select(-DATE,-DAILYWeather,-DAILYMaximumDryBulbTemp
                ,-DAILYAverageDryBulbTemp) %>%
  arrange(date,STATION_NAME) %>%
  group_by(date,STATION_NAME,STATION,LATITUDE,LONGITUDE,REPORTTPYE) %>%
  mutate_all(funs(as.numeric(.))) 
# warnings()

summary(daily)

daily2 <- daily %>% 
  ungroup() %>%
  mutate(nonmiss = rowSums(!is.na(dplyr::select(., contains("DAILY"))))) %>%
  mutate(keep = (nonmiss - 2) > 0) %>%
  filter(keep) %>%
  arrange(date,STATION_NAME,-nonmiss) %>%
  group_by(date,STATION_NAME) %>%
  mutate(seqn=row_number())

summary(daily2$nonmiss)
summary(daily2$keep)

daily2 %>% group_by(STATION,STATION_NAME) %>% summarise(n=n(),n.dates=n_distinct(date))
daily3 %>% group_by(STATION,STATION_NAME) %>% summarise(n=n(),n.dates=n_distinct(date))

daily3 <- daily2 %>% filter(seqn==1) %>%
  dplyr::select(-ELEVATION,-LATITUDE,-LONGITUDE,-REPORTTPYE) %>%
  rename(station = STATION, station_name = STATION_NAME) %>%
  setNames(gsub("^\\_"
                ,''
                ,gsub('[_]+','_'
                ,gsub(' ','_'
                      ,tolower(paste(gsub('([[:upper:]])', ' \\1'
                                           ,gsub('Minimum','min',
                                                 gsub('Maximum','max',
                                                      gsub('Average','Avg',
                                                           gsub('DAILY',''
                                                                ,names(.)
                                                                # ,gsub('([[:upper:]])', ' \\1', names(.))
                                                           )))))
                         ,sep='_')))))) %>%
  mutate(station_label = ifelse(station_name == 'CHICAGO MIDWAY AIRPORT IL US'
                                ,"midway"
                                ,ifelse(station_name == "CHICAGO OHARE INTERNATIONAL AIRPORT IL US"
                                        ,'ohare','other')))
         

daily_wide <- daily3 %>%
  ungroup() %>% group_by(date,station_label) %>%
  dplyr::select(-station,-station_name,-nonmiss,-keep,-seqn) %>%
  gather(variable,value,-c(station_label,t_date)) %>%
  unite(station.variable, station_label, variable) %>%
  spread(station.variable,value) %>%
  setNames(tolower(paste('wea'
                         ,names(.)
                         ,sep='_'
  ))) %>%
  rename(t_date = wea_date) 


# Remove columns that are completely missing for all observations
daily_wide <- daily_wide[, unlist(lapply(daily_wide, function(x) !all(is.na(x))))]

summary(daily_wide)

# daily_wide %>% dplyr::select(matches('dew'))

  
################################################################################
## Impute missing values



################################################################################
## Save data

wea_local_clim <- daily_wide

save(wea_local_clim, file="wea_local_clim.RData", compress = FALSE)
write.csv(wea_local_clim, paste(my.path,'wea_local_clim.csv',sep='\\'),row.names = FALSE)














