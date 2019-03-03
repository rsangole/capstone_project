





################################################################################
## Create abbreviations for each station name


# noaa.weather.stations
noaa.weather.stations

# rm('weather.station.names')
noaa.weather.stations <- noaa.weather.stations %>% 
  mutate(station_name_loc = gsub('\\.','',gsub(' ','_',trimws(tolower(
    substring(NAME,1,regexpr('[,]',NAME) - 1)))))) %>%
  # mutate(station_name_loc = rev(station_name_loc)) %>%
  mutate(loc_idx = regexpr('[0-9]',substring(station_name_loc,5))) %>%
  mutate(station_loc = ifelse(loc_idx > 0
                              ,substring(station_name_loc,5+loc_idx-1)
                              ,NA)) %>%
  mutate(station_name = ifelse(loc_idx > 0 
                               ,substring(station_name_loc,1,nchar(station_name_loc)-nchar(station_loc)-1)
                               ,station_name_loc)) %>%
  # mutate(station_abbrev = station_name_loc) %>%
  group_by(station_name) %>% 
  mutate(seqn = row_number(),station_cnt = max(seqn)) %>%
  ungroup() %>%
  mutate(station_abbrev=ifelse(station_cnt>1 & !is.na(station_loc),paste(station_name,gsub('_','',station_loc),sep='_'),station_name)) %>% 
  mutate(station_abbrev=gsub('heights','hts',station_abbrev)) %>%
  mutate(station_abbrev=gsub('grove','grv',station_abbrev)) %>%
  mutate(station_abbrev=gsub('estates','est',station_abbrev)) %>%
  mutate(station_abbrev=gsub('park','pk',station_abbrev)) %>%
  mutate(station_abbrev=gsub('arlington','arlng',station_abbrev)) %>%
  mutate(station_abbrev=gsub('bloomingdale','blmgdale',station_abbrev)) %>%
  mutate(station_abbrev=gsub('burnham_hegewisch','brnm_hege',station_abbrev)) %>%
  mutate(station_abbrev=gsub('burr_ridge','brrdg',station_abbrev)) %>%
  mutate(station_abbrev=gsub('chicago_ohare_international_airport','ohare',station_abbrev)) %>%
  mutate(station_abbrev=gsub('chicago_midway_airport','midway',station_abbrev)) %>%
  mutate(station_abbrev=gsub('chicago_palwaukee_airport','palwaukee',station_abbrev)) %>%
  mutate(station_abbrev=gsub('elmwood','elmwd',station_abbrev)) %>%
  mutate(station_abbrev=gsub('chicago','chgo',station_abbrev)) %>%
  mutate(station_abbrev=gsub('romeoville_weather_forecast_office','romeoville',station_abbrev)) %>%
  dplyr::select(-loc_idx,-seqn,-station_cnt) %>%
  group_by(station_abbrev) %>% 
  mutate(seqn = row_number(),station_cnt = max(seqn)) %>%
  ungroup() %>% 
  mutate(station_abbrev=ifelse(station_cnt>1 ,paste(station_abbrev,seqn,sep=''),station_abbrev)) %>% 
  dplyr::select(-seqn,-station_cnt) %>%
  arrange(station_abbrev) 

# noaa.weather.stations %>% ungroup() %>% 
#   summarise(a=n_distinct(station_abbrev),b=n_distinct(STATION),c=n_distinct(NAME))
# chk <- noaa.weather.stations %>% group_by(station_abbrev) %>% mutate(x=n_distinct(STATION)) %>%
#   arrange(-x)
  

# str(weather.all)

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

################################################################################
## EDA preparatory to restructuring


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
## Check correlations for all precipitation data

prcp.wide0 <- weather.all[,c("date","STATION","NAME","PRCP")] %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::select(-STATION,-NAME) %>% 
  gather(variable,value,-c(station_abbrev,date)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value)

prcp.wide <- ref.dates %>% left_join(prcp.wide0,by=c("date","date")) %>%
  .[,!colnames(.) %in% colnames(ref.dates)[-1]]

ggplot(aes(x=prcp.nonmissing.pct),data=data.frame(prcp.nonmissing.pct = colSums(!is.na(prcp.wide))/dim(prcp.wide)[1])) + 
  geom_histogram(binwidth=0.02) + 
  geom_vline(xintercept=0.95,linetype=4,color="black",alpha=0.5) + 
  geom_text(aes(x=0.95, label="95% complete",y=30), angle=90, vjust = 1.2, size=3.5) +
  geom_vline(xintercept=(1/7),linetype=4,color="black",alpha=0.5) +
  geom_text(aes(x=(1/7),label="~weekly",y=30), angle=90, vjust = 1.2, size=3.5) +
  geom_vline(xintercept=(1/30),linetype=4,color="black",alpha=0.5) + 
  geom_text(aes(x=(1/30),label="~monthly",y=30), angle=90, vjust = 1.2, size=3.5) +
  ggtitle('Completeness of daily precipitation data among 303 Chicago region weather stations') +
  xlab("Completeness of daily precipitation data, 2007-18 (%)")
ggsave(paste(plot.path,"Precipitation data pct daily non-missing.png",sep='\\'),width=10,height=8)


prcp.wide %>%
  .[,colSums(!is.na(prcp.wide))/dim(prcp.wide)[1] >= 0.95] %>%
  .[,-1] %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between daily precipitation data (limited to non-missing pairs for stations with >=95% complete daily data)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"Precipitation correlations - 95 pct complete stations.png",sep='\\'),width=10,height=8)



prcp.wide %>%
  .[,colSums(!is.na(prcp.wide))/dim(prcp.wide)[1] >= (1/7)] %>%
  .[,-1] %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>% 
  # hack for NA due to lack of variability 
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = F,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between daily precipitation data (limited to non-missing pairs for stations with approx. weekly data)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"Precipitation correlations - 14 pct complete stations.png",sep='\\'),width=10,height=8)



prcp.wide %>%
  .[,colSums(!is.na(prcp.wide))/dim(prcp.wide)[1] >= (1/2)] %>%
  .[,-1] %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>% 
  # hack for NA due to lack of variability 
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = F,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between daily precipitation data (limited to non-missing pairs for stations with >= 50% daily data)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"Precipitation correlations - 50 pct complete stations.png",sep='\\'),width=10,height=8)





################################################################################
## Check correlations for average temperature data

tavg2.wide0 <- weather.all[,c("date","STATION","NAME","tavg2")] %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::select(-STATION,-NAME) %>% 
  gather(variable,value,-c(station_abbrev,date)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value)

tavg2.wide <- ref.dates %>% left_join(tavg2.wide0,by=c("date","date")) %>%
  .[,!colnames(.) %in% colnames(ref.dates)[-1]]

ggplot(aes(x=tavg2.nonmissing.pct),data=data.frame(tavg2.nonmissing.pct = colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1])) + 
  geom_histogram(binwidth=0.02) + 
  geom_vline(xintercept=0.95,linetype=4,color="black",alpha=0.5) + 
  geom_text(aes(x=0.95, label="95% complete",y=30), angle=90, vjust = 1.2, size=3.5) +
  geom_vline(xintercept=(1/7),linetype=4,color="black",alpha=0.5) +
  geom_text(aes(x=(1/7),label="~weekly",y=30), angle=90, vjust = 1.2, size=3.5) +
  geom_vline(xintercept=(1/30),linetype=4,color="black",alpha=0.5) + 
  geom_text(aes(x=(1/30),label="~monthly",y=30), angle=90, vjust = 1.2, size=3.5) +
  ggtitle('Completeness of daily avg temperature data among 303 Chicago region weather stations') +
  xlab("Completeness of daily avg temperature data, 2007-18 (%)")
ggsave(paste(plot.path,"Daily avg temperature data pct daily non-missing.png",sep='\\'),width=10,height=8)


tavg2.wide %>%
  .[,colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1] >= 0.95] %>%
  .[,-1] %>%
  setNames(gsub('_tavg2','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between daily avg temperature data (limited to non-missing pairs for stations with >=95% complete daily data)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"Avg temperature correlations - 95 pct complete stations.png",sep='\\'),width=10,height=8)






################################################################################
## Just get Midway & O'Hare and see how variables compare


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

chgo.airports.wide <- weather.all %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::filter(station_abbrev %in% c('midway','ohare'))  %>%
  dplyr::select(station_abbrev,date,PRCP,TMAX,TMIN,tavg2) %>% 
  gather(variable,value,-c(station_abbrev,date)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value)

summary(chgo.airports.wide)


chk.miss <- anti_join(ref.dates,chgo.airports.wide,by=c("date")) # only 12/31/2018 missing from weather
chk.miss2 <- anti_join(chgo.airports.wide,ref.dates,by=c("date")) # no missing data

# chgo.airports.wide <- inner_join(ref.dates
#                                  ,chgo.airports.wide
#                                  ,by=c("date"))
# --n=4747

summary(chgo.airports.wide)


chk.miss3 <- chgo.airports.wide[rowSums(is.na(chgo.airports.wide)) > 0,] 
## n=38 rows.  Mostly Nov 2018 and later data, but a few points missing earlier.


chgo.airports.wide %>%
  # .[,colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1] >= 0.95] %>%
  .[,!colnames(.) %in% colnames(ref.dates)] %>%
  cor(.,use = "pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between Chicago airport daily weather data') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Chgo airport daily weather correlations.png",sep='\\'),width=10,height=8)



weather.all %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::filter(station_abbrev %in% c('midway','ohare'))  %>%
  .[,!colnames(.) %in% colnames(ref.dates)] %>%
  dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
  # mutate(tavg2 = coalesce(TAVG,(TMAX + TMIN)/2)) %>%
  dplyr::select(-matches('_ATTRIBUTES')) %>%
  gather(variable,value,-c(station_abbrev,date2)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value) %>%
  dplyr::select(-date2) %>%
  # .[,colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1] >= 0.95] %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = F,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between Chicago airport daily weather data') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Chgo airport daily weather correlations - extended.png",sep='\\'),width=10,height=8)



# colnames(weather.all)

weather.all %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::filter(station_abbrev %in% c('ohare'))  %>%
  .[,!colnames(.) %in% colnames(ref.dates)] %>%
  dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
  # mutate(tavg2 = coalesce(TAVG,(TMAX + TMIN)/2)) %>%
  dplyr::select(-matches('_ATTRIBUTES')) %>%
  gather(variable,value,-c(station_abbrev,date2)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value) %>%
  dplyr::select(-date2) %>%
  # .[,colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1] >= 0.95] %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = F,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between Chicago airport daily weather data (useful columns)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Chgo airport daily weather correlations - extended and trimmed.png",sep='\\'),width=10,height=8)

## This shows us that the temperature variables are highly correlated, 
## that there is little relationship between precipitation and other variables
## except snowfall (which is correlated with temperature and day of year 
## anyway).  There are clusters of variables related to wind speed and direction
## that may contain additional information, and also FMTM and PGTM (time of 
## fastest winds and peak gust time -- not expected to be useful for 
## mosquito prediction anyway). 
## So, what we CAN expect is temperature, precipitation, wind speed





################################################################################
## Data.frame of wide daily weather data

wea_daily_wide0 <- 
  weather.all %>% 
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  # dplyr::filter(station_abbrev %in% c('midway','ohare'))  %>%
  .[,!colnames(.) %in% colnames(ref.dates)] %>%
  dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
  # mutate(tavg2 = coalesce(TAVG,(TMAX + TMIN)/2)) %>%
  dplyr::select(-matches('_ATTRIBUTES')) %>%
  gather(variable,value,-c(station_abbrev,date2)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value)

dim(wea_daily_wide0) #4748x15454

colnames(wea_daily_wide0 %>% dplyr::select(matches('ohare')))
# 51 variables per station

# remove columns with all NA 
wea_daily_wide0 <- wea_daily_wide0[, unlist(lapply(wea_daily_wide0, function(x) !all(is.na(x))))]

dim(wea_daily_wide0) #4748x1373
colnames(wea_daily_wide0)


# remove columns with less than 95% complete daily data
wea_daily_wide1 <- 
  wea_daily_wide0[,colSums(!is.na(wea_daily_wide0))/dim(wea_daily_wide0)[1] >= 0.95]

dim(wea_daily_wide1) #4748x68
colnames(wea_daily_wide1)

  

################################################################################
## EDA on wide data 

wea_daily_wide1 %>% 
  dplyr::select(-date2) %>%
  # .[,colSums(!is.na(tavg2.wide))/dim(tavg2.wide)[1] >= 0.95] %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = F,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between Chicago daily weather data (station variables >= 95% complete)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Chgo daily weather correlations - limited to stations and vars 95 pct complete.png",sep='\\'),width=12,height=12)


wea_daily_wide1 %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() + 
  scale_x_date(date_breaks ="2 years"
               ,date_minor_breaks  = "1 years"
               , labels=date_format("%Y")
               ,limits=c(as.Date("2007-01-01"),as.Date("2014-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily weather time series - limited to stations and vars 95 pct complete.png",sep='\\'),width=18,height=16)


wea_daily_wide1 %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('PRCP')) %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() + 
  scale_x_date(date_breaks ="1 years"
               ,date_minor_breaks  = "3 months"
               , labels=date_format("%Y")
               ,limits=c(as.Date("2007-01-01"),as.Date("2014-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily precipitation - limited to stations and vars 95 pct complete.png",sep='\\'),width=12,height=10)

wea_daily_wide1 %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('tavg2')) %>%
  setNames(gsub('_tavg2','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() + 
  scale_x_date(date_breaks ="1 years"
               ,date_minor_breaks  = "3 months"
               , labels=date_format("%Y")
               ,limits=c(as.Date("2007-01-01"),as.Date("2014-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily max temp - limited to stations and vars 95 pct complete.png",sep='\\'),width=12,height=10)


wea_daily_wide1 %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('_WS')) %>%
  # setNames(gsub('_tavg2','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() + 
  scale_x_date(date_breaks ="1 years"
               ,date_minor_breaks  = "3 months"
               , labels=date_format("%Y")
               ,limits=c(as.Date("2007-01-01"),as.Date("2014-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily max wind speed - limited to stations and vars 95 pct complete.png",sep='\\'),width=12,height=10)

# str(wea_daily_wide1)


################################################################################
## Look at variables we're omitting

wea_daily_wide3 <-
  weather.all %>%
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  .[,!colnames(.) %in% colnames(ref.dates)] %>%
  dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
  dplyr::select(-matches('_ATTRIBUTES')) %>%
  gather(variable,value,-c(station_abbrev,date2)) %>%
  unite(station.variable, station_abbrev, variable) %>%
  spread(station.variable,value)

# remove columns with all NA
wea_daily_wide3 <- wea_daily_wide3[, unlist(lapply(wea_daily_wide3, function(x) !all(is.na(x))))]

# remove columns with less than 95% complete daily data
wea_daily_wide3b <-
  wea_daily_wide3[,colSums(!is.na(wea_daily_wide3))/dim(wea_daily_wide3)[1] >= 0.95]
# wea_daily_wide3b <- cbind(date2=wea_daily_wide3$date2,wea_daily_wide3b)
dim(wea_daily_wide3b)
dim(wea_daily_wide3)

# remove columns with more than 95% complete daily data
wea_daily_wide3c <-
  wea_daily_wide3[,colSums(!is.na(wea_daily_wide3))/dim(wea_daily_wide3)[1] < 0.95]
wea_daily_wide3c <- cbind(date2=wea_daily_wide3$date2,wea_daily_wide3c)
dim(wea_daily_wide3c)


wea_daily_wide3b %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('PRCP')) %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() +
  scale_x_date(date_breaks ="4 years"
               ,date_minor_breaks  = "2 years"
               ,labels=date_format("%Y")
               ,limits=c(as.Date("2006-01-01"),as.Date("2018-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily precipitation ts - patterns of missingness for those lt 95 pct complete.png",sep='\\'),width=30,height=30)

## It's possible that some of these could be useable with a bit of work to 
## determine why they have less than 95% completeness.  If they're just not 
## reporting data on days with zero precipitation, that could easily be resolved.


wea_daily_wide3b %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('tavg2')) %>%
  setNames(gsub('_tavg2','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() +
  scale_x_date(date_breaks ="4 years"
               ,date_minor_breaks  = "2 years"
               ,labels=date_format("%Y")
               ,limits=c(as.Date("2006-01-01"),as.Date("2018-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily max temp ts - patterns of missingness for those lt 95 pct complete.png",sep='\\'),width=30,height=30)


wea_daily_wide3b %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  dplyr::select(t_date,matches('_WS')) %>%
  # setNames(gsub('_tavg2','',names(.))) %>%
  gather(attribute, value , -t_date, factor_key=FALSE) %>%
  ggplot(aes(y=value,x=t_date,group=attribute)) +
  facet_wrap(~attribute,scales = "free") +
  geom_line() +
  scale_x_date(date_breaks ="4 years"
               ,date_minor_breaks  = "2 years"
               ,labels=date_format("%Y")
               ,limits=c(as.Date("2006-01-01"),as.Date("2018-12-31"))
  ) +
  theme(axis.title.x=element_blank())
ggsave(paste(plot.path,'Weather EDA',"Chgo daily wind speed ts - patterns of missingness for those lt 95 pct complete.png",sep='\\'),width=30,height=30)



################################################################################
## Look at correlations between weather data and outcomes

# mosq_wide <- df_rev %>% 
#   # dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
#   # mutate(tavg2 = coalesce(TAVG,(TMAX + TMIN)/2)) %>%
#   # dplyr::select(-matches('_ATTRIBUTES')) %>%
#   dplyr::select(trap_trap_name,t_date,mos_tot_num_mosquitos,mos_any_wnv_present) %>% 
#   mutate(mos_tot_num_mosquitos = as.numeric(mos_tot_num_mosquitos)
#          ,mos_any_wnv_present = as.numeric(mos_any_wnv_present)) %>%
#   gather(variable,value,-c(trap_trap_name,t_date)) %>%
#   unite(trap.variable, trap_trap_name, variable) %>%
#   spread(trap.variable,value)


# mosq_wide2 <- wea_daily_wide0 %>% dplyr::select(date2,matches('ohare')) %>% 
#   mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
#   inner_join(mosq_wide,by=c("t_date")) 
# 
# 
# mosq_wide2_cor <- 
# mosq_wide2 %>% 
#   dplyr::select(-t_date) %>%
#   cor(.,use = "pairwise.complete.obs") %>%
#   as.data.frame(.) %>% 
#   mutate_all(funs(coalesce),0) %>%
#   .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
#   `rownames<-`(names(.)) 
# 
# mosq_wide2_cor %>% 
#   as.matrix(.) %>%
#   ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
#                          lab = F,ggtheme = "minimal",lab_size = 3,digits=3
#                          # ,tl.srt=0
#   ) +
#   ggtitle('Correlations between Chicago airport daily weather data (useful columns)') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ggsave(paste(plot.path,'Weather EDA',"Chgo airport daily weather correlations - extended and trimmed.png",sep='\\'),width=10,height=8)

## This is just too hard to do.  Let's repeat using a weekly time series for 
## the whole city. 


city_wkly_long2

mosq_wide <- df_rev %>%
  # dplyr::select(-STATION,-NAME,-LATITUDE,-LONGITUDE,-ELEVATION,-DATE,-location,-TAVG) %>%
  # mutate(tavg2 = coalesce(TAVG,(TMAX + TMIN)/2)) %>%
  # dplyr::select(-matches('_ATTRIBUTES')) %>%
  dplyr::select(trap_trap_name,t_date,mos_tot_num_mosquitos,mos_any_wnv_present) %>%
  mutate(mos_tot_num_mosquitos = as.numeric(mos_tot_num_mosquitos)
         ,mos_any_wnv_present = as.numeric(mos_any_wnv_present)) %>%
  gather(variable,value,-c(trap_trap_name,t_date)) %>%
  unite(trap.variable, trap_trap_name, variable) %>%
  spread(trap.variable,value)


mosq_wide2 <- wea_daily_wide0 %>% dplyr::select(date2,matches('ohare')) %>%
  mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
  inner_join(mosq_wide,by=c("t_date"))


# wea_daily_wide0

wea_wkly <- wea_daily_wide0 %>% mutate(t_date=as.Date(date2)) %>%
  dplyr::select(-date2) %>% 
  # mutate_at(vars(-one_of('t_date')),funs(ma7=make.ma),y=0:6) 
  inner_join(ref.dates %>% dplyr::select(date,eval.wk)
             ,by=c("t_date"="date")) %>%
  rename(t_eval_wk = eval.wk) %>%
  dplyr::select(-t_date) %>%
  mutate(t_eval_wk = as.integer(t_eval_wk)) %>%
  group_by(t_eval_wk) %>% 
  summarise_all(sum) 
  # mutate_at(vars(-one_of('t_date')),sum)

# dim(wea_wkly) 
# dim(city_wkly_long2) 

city_wkly_long2 <- left_join(eval_wks %>% filter(t_train)
                             ,city_wkly_long
                             ,by=("t_eval_wk")) %>%
  mutate(t_eval_wk = as.integer(t_eval_wk)) %>% 
  dplyr::select(-c(t_mo,t_wk_end)) 

city_wkly_long3 <- city_wkly_long2 %>%
  left_join(wea_wkly)

# summary(city_wkly_long3 %>% dplyr::select(starts_with('mos_'),starts_with('ohare_')))

  
city_wkly_long3 %>%
  dplyr::select(-starts_with('t_'),-starts_with('n_')) %>%
  dplyr::select(starts_with('mos_'),starts_with('any_'),starts_with('ohare_')) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>%
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>%
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = T,ggtheme = "minimal",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('Correlations between Chicago airport daily weather data (useful columns)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste(plot.path,'Weather EDA',"Chgo airport daily weather correlations - extended and trimmed.png",sep='\\'),width=10,height=8)





################################################################################
## Look at station variability 

wea_daily_wide3b %>% 
  dplyr::select(-date2) %>%
  dplyr::select(matches('PRCP')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Precipitation correlations for larger set of stations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Daily data - correlations - precipitation.png",sep='\\'),width=12,height=12)


wea_daily_wide3b %>% 
  dplyr::select(-date2) %>%
  dplyr::select(matches('AWND'),matches('WSF')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Wind speed correlations for larger set of stations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Daily data - correlations - wind.png",sep='\\'),width=12,height=12)



wea_daily_wide3b %>% 
  dplyr::select(-date2) %>%
  dplyr::select(matches('AWND'),matches('WSF')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Wind speed correlations for larger set of stations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Daily data - correlations - wind.png",sep='\\'),width=12,height=12)


wea_daily_wide3b %>% 
  dplyr::select(-date2) %>%
  dplyr::select(matches('TOBS'),matches('avg2'),matches('TMIN'),matches('TMAX')
                ,matches('deg_day')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Temperature correlations for larger set of stations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Daily data - correlations - temp.png",sep='\\'),width=12,height=12)


colnames(wea_daily_wide3b)


################################################################################
## Impute missing values... via "Multiple Imputation by Chained Equations"
## using mice package in R
## Run 5 iterations and take mean of those five

require(mice)

miss.data.pat <- md.pattern(chgo.airports.wide)

# imputed.via.predmean <- mice(chgo.airports.wide, m=5, maxit = 30, method = 'pmm', seed = 500)
imputed.via.predmean <- mice(chgo.airports.wide, m=5, maxit = 50, method = 'pmm', seed = 500)

# summary(imputed.via.predmean)
# summary(imputed.via.predmean$imp)
# summary(imputed.via.predmean$imp$USW00014819_PRCP)
# str(imputed.via.predmean$imp$USW00014819_PRCP)
# chk <- imputed.via.predmean$imp$USW00014819_PRCP


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
## Impute missing data for selected variables
################################################################################

require(mice)
wide.imputations <- mice(weather.wide, m=10, maxit = 10, method = 'pmm', seed = 500)

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

weather.subset.wide.imp <- weather.subset.wide2

for(col in names(wide.imputations$imp)) {
  weather.subset.wide.imp <- update.values(col,wide.imputations)
}

rowSums(is.na(weather.wide[,-1]))
rowSums(is.na(weather.wide.imp[,-1]))

rowSums(!is.na(weather.wide[,-1]))
rowSums(!is.na(weather.wide.imp[,-1]))


str(weather.wide.imp)

# summary(weather.wide.imp %>% dplyr::select(matches('tavg2')) %>% 
#           select(matches())
#         .[,1:20])

################################################################################
## Daily temp/precip weather data -- all stations
################################################################################

weather.all$tavg2 <- ifelse(is.na(weather.all$TAVG)
                            ,(weather.all$TMIN + weather.all$TMAX)/2
                            ,weather.all$TAVG)

precip.wide <- weather.all %>% 
  dplyr::select(-ends_with('ATTRIBUTES')) %>%
  left_join(noaa.weather.stations %>% dplyr::select(STATION,NAME,station_abbrev)) %>%
  dplyr::select(-c(STATION,NAME,LATITUDE,LONGITUDE,ELEVATION,date2,DATE,location,yr,mo)) %>%
  rename(t_date = date) %>% 
  # dplyr::select(c(starts_with('t'),contains('prcp'))) %>% 
  dplyr::select(station_abbrev,t_date,PRCP) %>%
  gather(variable,value,-c(station_abbrev,t_date)) %>%
  unite(station.variable, station_abbrev, variable) %>%
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

# weather.wide.imp[1:10,2:11]
# as.vector(as.list(weather.wide.imp[1,]))

colnames(weather.wide)
colnames(weather.wide.imp)
str(weather.wide.imp)
summary(as.data.frame(weather.wide.imp[,-1]))

# summary(weather.wide.imp %>% dplyr::select(matches('tavg2')) %>% 
#           select(matches())
#           .[,1:20])


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

nearest.stations.x <- wea2trap.2 %>% 
  group_by(trap.name) %>% 
  dplyr::filter(wea.seqn <= 10) %>%
  mutate(wea.dist.mi.cumavg = cumsum(wea.dist.mi)/wea.seqn) %>%
  mutate(new.name = paste('wea.dist.mi.top',wea.seqn,sep='')) 
# %>%
#   dplyr::select(trap.name,wea.dist.mi.cumavg,new.name,wea.seqn) 
# %>%
#   spread(new.name,wea.dist.mi.cumavg)

nearest.stations.x %>% ggplot(aes(x=wea.dist.mi,group=wea.seqn)) +
  geom_histogram() +
  facet_wrap(~wea.seqn) + 
  ggtitle("Distance to nearest nth station")
ggsave(paste(plot.path,'Weather EDA',"Distance to nearest stations.png",sep='\\'),width=10,height=8)

nearest.stations.x %>% ggplot(aes(x=wea.dist.mi.cumavg,group=wea.seqn)) +
  geom_histogram() +
  facet_wrap(~wea.seqn) + 
  ggtitle("Mean distance to nearest nth stations")
ggsave(paste(plot.path,'Weather EDA',"Distance to nearest stations (mean of nth).png",sep='\\'),width=10,height=8)



## I could repeat this only among stations used for precipitation which is the 
## variable with most station variability....





################################################################################
## Rank weather stations in order of closest station to each trap
## Limit to stations with nearly-complete temp/precipitation data
################################################################################


PRCP.stns <- noaa.weather.stations[noaa.weather.stations$PRCP.n >= 4500,]


wea2trap.2 <- wea2trap %>%
  semi_join(PRCP.stns) %>%
  arrange(trap.name,wea.dist.mi) %>%
  group_by(trap.name) %>%
  mutate(wea.seqn = row_number()) 
# summary(wea2trap.2$wea.dist.ft)


# head(wea2trap)

nearest.stations <- wea2trap.2 %>% filter(wea.seqn==1) %>% dplyr::select(-wea.seqn)

nearest.stations.x <- wea2trap.2 %>% 
  group_by(trap.name) %>% 
  dplyr::filter(wea.seqn <= 10) %>%
  mutate(wea.dist.mi.cumavg = cumsum(wea.dist.mi)/wea.seqn) %>%
  mutate(new.name = paste('wea.dist.mi.top',wea.seqn,sep='')) 
# %>%
#   dplyr::select(trap.name,wea.dist.mi.cumavg,new.name,wea.seqn) 
# %>%
#   spread(new.name,wea.dist.mi.cumavg)

nearest.stations.x %>% ggplot(aes(x=wea.dist.mi,group=wea.seqn)) +
  geom_histogram() +
  facet_wrap(~wea.seqn) + 
  ggtitle("Distance to nearest nth precipitation station")
ggsave(paste(plot.path,'Weather EDA',"Distance to nearest precipitation stations.png",sep='\\'),width=10,height=8)

nearest.stations.x %>% ggplot(aes(x=wea.dist.mi.cumavg,group=wea.seqn)) +
  geom_histogram() +
  facet_wrap(~wea.seqn) + 
  ggtitle("Mean distance to nearest nth precipitation stations")
ggsave(paste(plot.path,'Weather EDA',"Distance to nearest precipitation stations (mean of nth).png",sep='\\'),width=10,height=8)


## We could also try to pick stations that are dispersed directionally and
## thus do a better job of triangulating the trap, but that would require a 
## lot of effort and possibly little payoff.  
## Let's just work with nearest preciptation station to see if it adds value
## above using OHare/Midway outright.


################################################################################
## Rank weather stations in order of closest station to each trap
## Limit to stations with nearly-complete temp/precipitation data
################################################################################

# summary(noaa.weather.stations)


PRCP.stns <- noaa.weather.stations[noaa.weather.stations$PRCP.n >= 4500,]
tavg2.stns <- noaa.weather.stations[noaa.weather.stations$tavg2.n >= 4500,]
wind.stns <- noaa.weather.stations[noaa.weather.stations$WSF5.n >= 4500,]

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

nearest.WIND <- semi_join(wea2trap,wind.stns,by=c("STATION")) %>%
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


nearest_wind <- inner_join(wnv.traps,nearest.WIND) %>%
  inner_join(weather.all,by=c("STATION")) %>% 
  dplyr::select(-ends_with('ATTRIBUTES')) %>%
  group_by(trap.name,date) %>%
  dplyr::select(STATION,tavg2,wea.dist.mi) %>%
  dplyr::rename(nearest_wind = tavg2
                ,nearest_wind_station = STATION
                ,nearest_wind_dist_mi = wea.dist.mi)

nearest_wind %>% ungroup() %>% summarise(n=n(),n.traps = n_distinct(trap.name)
                                          ,n.stations = n_distinct(nearest_wind_station)
                                          ,n.dates = n_distinct(date)
                                          ,min.dist = min(nearest_wind_dist_mi)
                                          ,max.dist = max(nearest_wind_dist_mi)
                                          ,mean.dist = mean(nearest_wind_dist_mi))


# nearest_precips_imp 
summary(nearest_precips)
summary(nearest_temps)
summary(nearest_wind)


################################################################################
## Add this station data to our dataset
################################################################################

# df$t_date <- as.Date(df$t_date) 
# 
# df2 <- left_join(df,nearest_temps,by=c("trap_trap_name" = 'trap.name',"t_date" = "date"))
# df2 <- left_join(df2,nearest_precips,by=c("trap_trap_name" = "trap.name","t_date" = "date"))
# 
# 
# # kdepairs(df %>% dplyr::select(nearest_prcp,wea_midway_prcp,wea_ohare_prcp))
# df2 %>% 
#   dplyr::select(nearest_prcp,wea_midway_prcp,wea_ohare_prcp) %>%
#   dplyr::filter(rowSums(is.na(.)) == 0) %>%
#   cor() %>%
#   ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
#                          lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
#                          # ,tl.srt=0
#   ) +
#   ggtitle('NOAA OHare local climatological data') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # ....
# 
# 
# getwd()
# setwd(paste("D:","ajc188","github","capstone_project","data","processed",sep='\\'))
# getwd() 
# 
# save(df, file="df.RData", compress = FALSE)
# write.csv(df, 'df.csv',row.names = FALSE)


################################################################################
## Quick EDA - precipitation

## Look at correlations between different station's precipitation (limiting
## to rows with non-missing values for stations with ~ 95% complete data.)


weather.wide %>%
  ungroup() %>%
  dplyr::select(matches('PRCP')) %>%
  # dplyr::select(matches(paste(noaa.weather.stations[noaa.weather.stations$PRCP.n >= 4500,]$STATION,collapse='|'))) %>% 
  # dplyr::select(matches(paste(PRCP.stns$STATION,collapse='|'))) %>% 
  dplyr::select(matches(paste(nearest.PRCP %>% group_by(STATION) %>% summarise(n=n()) %>% .$STATION,collapse='|'))) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  setNames(gsub('_PRCP','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('NOAA daily weather data - precipitation') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Map them

# summary(as.factor(nearest.PRCP$STATION))
# summary(as.factor(nearest.PRCP$NAME))
# summary(as.factor(PRCP.stns$STATION))


nearest.PRCP2 <- nearest.PRCP %>% 
  left_join(wnv.traps %>% 
              mutate(trap.lat=as.numeric(rev.lat),trap.lng=as.numeric(rev.lng)) %>% 
              dplyr::select(trap.name,trap.lat,trap.lng)) %>%
  left_join(weather.stations.1 %>%  ungroup() %>%
              mutate(station.lat=LATITUDE,station.lng=LONGITUDE) %>%
              dplyr::select(STATION,NAME,station.lat,station.lng)) %>%
  as.data.frame()

plot.comm.area + 
  geom_point(aes(x=station.lng,y=station.lat,color=NAME),shape='x',size=2,data=nearest.PRCP2) + 
  geom_point(aes(x=trap.lng,y=trap.lat,color=NAME),data=nearest.PRCP2,show_guide=F)

nearest.PRCP2 %>% group_by(STATION,NAME) %>% summarise(n=n_distinct(trap.name))



################################################################################
## Quick EDA - temperature

weather.wide %>%
  ungroup() %>%
  dplyr::select(matches('tavg2')) %>%
  dplyr::select(matches(paste(nearest_temps %>% mutate(STATION=nearest_temp_station) %>% group_by(STATION) %>% summarise(n=n()) %>% .$STATION,collapse='|'))) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  setNames(gsub('_tavg2','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('NOAA daily weather data - precipitation') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Very little variability in average daily temperature

weather.wide %>%
  ungroup() %>%
  dplyr::select(matches('tavg2')) %>%
  dplyr::select(matches(paste(tavg2.stns %>% group_by(STATION) %>% summarise(n=n()) %>% .$STATION,collapse='|'))) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  setNames(gsub('_tavg2','',names(.))) %>%
  # setNames(gsub(weather.station.names$STATION,weather.station.names$station_abbrev,names(.))) %>%
  # rename_(.dots = setNames(vt$old_varname, vt$new_varname))
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
  ) +
  ggtitle('NOAA daily weather data - precipitation') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Map nearest temp station

# summary(as.factor(nearest.tavg2$STATION))
# summary(as.factor(nearest_temps$nearest_temp_station))
# summary(as.factor(tavg2.stns$STATION))


nearest_temps2 <- nearest_temps %>% 
  mutate(STATION = nearest_temp_station) %>%
  left_join(wnv.traps %>% 
              mutate(trap.lat=as.numeric(rev.lat),trap.lng=as.numeric(rev.lng)) %>% 
              dplyr::select(trap.name,trap.lat,trap.lng)) %>%
  left_join(weather.stations.1 %>%  ungroup() %>%
              mutate(station.lat=LATITUDE,station.lng=LONGITUDE) %>%
              dplyr::select(STATION,NAME,station.lat,station.lng)) %>%
  as.data.frame()

plot.comm.area + 
  geom_point(aes(x=station.lng,y=station.lat,color=NAME),shape='x',size=2,data=nearest_temps2) + 
  geom_point(aes(x=trap.lng,y=trap.lat,color=NAME),data=nearest_temps2,show_guide=F)

nearest_temps2 %>% group_by(STATION,NAME) %>% summarise(n=n_distinct(trap.name))




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



## Pull in nearest precipitation station and nearest temp....




