

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

lcd.1 <- read.csv(paste(weather.path,"1617130.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.2 <- read.csv(paste(weather.path,"1617146.csv",sep="\\"),stringsAsFactors = FALSE)
# lcd.3 <- read.csv(paste(weather.path,"1617130.csv",sep="\\"),stringsAsFactors = FALSE)
# lcd.4 <- read.csv(paste(weather.path,"1617130.csv",sep="\\"),stringsAsFactors = FALSE)
# lcd.5 <- read.csv(paste(weather.path,"1617130.csv",sep="\\"),stringsAsFactors = FALSE)


# These data may need a lot of work to pull out variables we want.  
# Rows are hourly or something like that.  Some columns are populated 
# only daily or at other intervals.  


