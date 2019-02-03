

noaa.weather.stations
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


lapply(colnames(weather.all)[regexpr('ATTRIBUTES',colnames(weather.all)) > 0]
       ,FUN=function(x) {ftable(weather.all[,x])})


noaa.weather.stations
table(noaa.weather.stations$NAME)
table(noaa.weather.stations[
  regexpr('MIDWAY',noaa.weather.stations$NAME) > 0
  | regexpr('HARE',noaa.weather.stations$NAME) > 0,]$NAME)


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



