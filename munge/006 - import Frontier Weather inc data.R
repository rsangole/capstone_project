

################################################################################
## Identify files to load
################################################################################

base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
weather.path <- paste(base.path,'weather data',sep='\\')

filename.list <- list.files(path = weather.path)           
# filename.list
weather.files <- filename.list[grep('Chicago area weather',filename.list)]



################################################################################
## Load NOAA weather files
################################################################################

weather.other.1 <- read.csv(paste(weather.path,"CDO6172217830671.txt",sep="\\"),stringsAsFactors = FALSE)
weather.other.2 <- read.csv(paste(weather.path,"CDODiv1391317830653.txt",sep="\\"),stringsAsFactors = FALSE)

frontier.weather.1 <- read.csv(paste(weather.path,"KMDW_daily.txt",sep="\\"),stringsAsFactors = FALSE)
frontier.weather.2 <- read.csv(paste(weather.path,"KORD_daily.txt",sep="\\"),stringsAsFactors = FALSE)

# Midway and O'Hare weather data from Frontier Weather Inc look pretty solid.

str(frontier.weather.1)
str(frontier.weather.2)

frontier.weather <- rbind(frontier.weather.1,frontier.weather.2)

rm(frontier.weather.1); rm(frontier.weather.2)




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
################################################################################


getwd()
setwd(my.path)
getwd()



save(frontier.weather, file="frontier.weather.RData", compress = FALSE)

write.csv(frontier.weather, paste(my.path,'frontier.weather.csv',sep='\\'),row.names = FALSE)




