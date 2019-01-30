
################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"

my.path <- paste(base.path,"data",sep="\\")
src.path <- paste(base.path,"source data",sep="\\")
plot.path <- paste(base.path,"plots",sep="\\")
submission.path <- paste(base.path,"submissions",sep="\\")
output.path <- paste(base.path,"output",sep="\\")


filename.list <- list.files(path = src.path)           
filename.list



################################################################################
## Load WNV mosquito trap datasets from CDPH (Chicago data portal)
################################################################################

cdph.csv <- "West_Nile_Virus__WNV__Mosquito_Test_Results.csv"

cdph.wnv <- read.csv(paste(base.path,'cdph data',cdph.csv,sep="\\"),stringsAsFactors = FALSE)

# str(cdph.wnv)


################################################################################
## Load WNV mosquito trap datasets from CDPH (Chicago data portal)
################################################################################

colnames(cdph.wnv) <- tolower(colnames(cdph.wnv))

colnames(cdph.wnv)[colnames(cdph.wnv)=="season.year"] <- "yr"
# cdph.wnv$yr <- cdph.wnv$season.year
cdph.wnv$date <- as.Date(substring(cdph.wnv$test.date,1,10),"%m/%d/%Y")
# westnile.cdph$date2 <- as.POSIXlt(cdph.wnv$test.date, format="%m/%d/%Y %H:%M:%OS %p")
cdph.wnv$mo <- lubridate::month(cdph.wnv$date)
cdph.wnv$WnvPresent <- as.logical(cdph.wnv$result == 'positive')
colnames(cdph.wnv)[colnames(cdph.wnv)=="number.of.mosquitoes"] <- "NumMosquitos"

colnames(cdph.wnv)[colnames(cdph.wnv)=="latitude"] <- "lat.num"
colnames(cdph.wnv)[colnames(cdph.wnv)=="longitude"] <- "lng.num"

lat.lng <- trimws(stringr::str_split_fixed(gsub('[()]','',cdph.wnv$location), ",", 2))
cdph.wnv$lat <- lat.lng[,1]
cdph.wnv$lng <- lat.lng[,2]


################################################################################
## Train/validate/test partitions
################################################################################

ftable(cdph.wnv$yr)

cdph.wnv$train <- cdph.wnv$yr %in% c(2007,2008,2009,2010,2011,2012,2013)
cdph.wnv$validate <- cdph.wnv$yr %in% c(2014,2015,2016)
cdph.wnv$test <- cdph.wnv$yr %in% c(2017,2018)

cdph.wnv$partition <- ifelse(cdph.wnv$train,"train"
                             ,ifelse(cdph.wnv$validate,"validate"
                                    ,"test"))

summary(cdph.wnv$partition)
ftable(cdph.wnv$partition,cdph.wnv$yr)



################################################################################
## Make a trap-level summary
################################################################################

traps.1 <- my.cdph.aggr(c("trap"),df=cdph.wnv)
# There are two traps with differing locations.  These are effectively 
# different traps.  One way to handle this is to create a different unique ID.


traps.2 <- my.cdph.aggr(c("trap","lat","lng"),df=cdph.wnv)


################################################################################
## Check and fix traps with missing lat/lng pairs
################################################################################

traps.missing.lat.lng <- traps.2[is.na(traps.2$lat) | is.na(traps.2$lng)
                                 | traps.2$lat == '' | traps.2$lng == '',]
summary(traps.missing.lat.lng$block.unique)
traps.missing.lat.lng2 <- semi_join(cdph.wnv,traps.missing.lat.lng) %>%
  group_by(trap,location,lat,lng,block) %>% summarise(nrows=n()
                                                      ,mind.date=min(date)
                                                      ,max.date=max(date))

## Try kaggle-sourced dataset to see if this fixes the issues.
## Still some issues.  Probably not worth doing it this way.
traps.missing.lat.lng3 <- 
  left_join(traps.missing.lat.lng2
            ,westnile.traps[westnile.traps$Address != '5100 West 72nd Street, Chicago, IL 60638, USA'
                            ,c("Trap","Address","Latitude","Longitude")]
            ,by=c("trap" = "Trap"))

## Try just geocoding myself
traps.missing.lat.lng3$addr.to.geocode <- paste(gsub('XX'
                                                     ,'00'
                                                     ,traps.missing.lat.lng3$block)
                                                ,', Chicago IL',sep='')
traps.missing.lat.lng3$addr.to.geocode <- gsub(" ","+",traps.missing.lat.lng3$addr.to.geocode)
traps.missing.lat.lng3$google.call <-   ifelse(is.na(traps.missing.lat.lng3$addr.to.geocode) 
                                               | traps.missing.lat.lng3$addr.to.geocode == "","",
                                               paste(geocode.api.url,"address="
                                                     ,traps.missing.lat.lng3$addr.to.geocode
                                                     ,"&key=",geocode.api.key,sep=""))
library("httr")
library("jsonlite")

google.response <- sapply(traps.missing.lat.lng3$google.call,function(x) {
  if(!is.na(x) & x != "") { GET(x) }
})

## Process geocoding results

process.geocoding.api.result <- function(x) {
  x <- fromJSON(rawToChar(unlist(x)))
  result <- as.data.frame(as.character(x)[1])
  colnames(result) <- c("json")
  
  result$status <- x$status
  result$result.cnt <- dim(x$results)[1]
  
  # For now, use the first result if there are multiple results.
  if(result$result.cnt > 1) { result1 <- x$results[1] }
  else {result1 <- x$results}
  
  result$place.id <- result1$place_id
  result$formatted.address <- result1$formatted_address
  result$lat <- result1$geometry$location$lat
  result$lng <- result1$geometry$location$lng
  
  
  addr.components.labels = as.list(rep(NA,length(result1$address_components[[1]]$types)))
  for(i in 1:length(result1$address_components[[1]]$types)) {
    addr.components.labels[i] = unlist(result1$address_components[[1]]$types[i])[1]
  }
  
  addr.components.df <- as.data.frame(result1$address_components)
  
  addr.components.df2 <- transpose(addr.components.df[,c("long_name","short_name")])
  colnames(addr.components.df2) <- unlist(addr.components.labels)
  rownames(addr.components.df2) <- colnames(addr.components.df)[1:2]
  
  result$street.number = addr.components.df2["short_name",]$street_number
  result$route = addr.components.df2["short_name",]$route
  result$locality = addr.components.df2["short_name",]$locality
  result$admin_area_level_1 = addr.components.df2["short_name",]$administrative_area_level_1
  result$admin_area_level_2 = addr.components.df2["short_name",]$administrative_area_level_2
  result$admin_area_level_3 = addr.components.df2["short_name",]$administrative_area_level_3
  result$country = addr.components.df2["short_name",]$country
  result$postal_code = addr.components.df2["short_name",]$postal_code
  result$postal_code_suffix = addr.components.df2["short_name",]$postal_code_suffix
  
  return(result)
}

process.all.results <- function(response) {
  calls <- length(response)/10
# This is actually wrong but works.  We should be working with a 2-dimensional
# matrix rather than manually parsing into groups of 10.  
  for (i in 1:calls) {
    # print(i)
    idx <- 10*(i-1) + 6
    my.result <- process.geocoding.api.result(response[idx])
    if (i == 1) {
      my.results <- my.result
    }
    else {
      my.results <- bind_rows(my.results,my.result)
    }
  }
  return(my.results)
}


library(data.table)

address.results <- process.all.results(google.response)

traps.missing.lat.lng3$formatted.address <- address.results$formatted.address
traps.missing.lat.lng3$google.lat <- address.results$lat
traps.missing.lat.lng3$google.lng <- address.results$lng


traps.2b <- left_join(traps.2
                      ,traps.missing.lat.lng3
                      ,by=c("trap","lat","lng")
)

traps.2b$cdph.lat <- traps.2b$lat 
traps.2b$cdph.lng <- traps.2b$lng 

traps.2b[traps.2b$lat == '',]$lat <- NA
traps.2b[traps.2b$lng == '',]$lng <- NA

traps.2b$lat.lng.src = ifelse(!is.na(traps.2b$lat),'CDPH','Google')

traps.2b$rev.lat <- coalesce(traps.2b$lat,as.character(traps.2b$google.lat))
traps.2b$rev.lng <- coalesce(traps.2b$lng,as.character(traps.2b$google.lng))

traps.2c <- traps.2b[,c("trap","rev.lat","rev.lng","lat.lng.src"
                        ,"cdph.lat","cdph.lng"
                        ,"google.lat","google.lng")]

traps.2c$satellite.ind <- regexpr('[0-9]',substring(traps.2c$trap
                                    ,nchar(traps.2c$trap)
                                    ,nchar(traps.2c$trap)
                                    ) ) == -1
traps.2c$trap.name <- paste(
  ifelse(substring(traps.2c$trap,1,1) != 'T','T','')
  ,traps.2c$trap
  ,sep=''
)

# Give all traps a unique name assuming that addresses and lat/lng pairs accurately
#  differentiate between traps
traps.3 <- traps.2c %>% arrange(trap,rev.lat,rev.lng) %>%
  group_by(trap) %>% mutate(
    trap.name = ifelse(row_number() > 1,paste(trap.name,row_number(),sep='.'),trap.name)    
  )

# Manually recode one trap name since it's in the same location as 1+ other 
# traps but a different time point.  Likely a change in the trap of some 
# sort but I see no reason to treat it differently for our purposes
traps.3[traps.3$trap.name == 'T031',]$trap.name <- 'T167'

trap.crosswalk <- traps.3[,c("trap","cdph.lat","cdph.lng","trap.name","rev.lat","rev.lng","lat.lng.src","satellite.ind")]


rm(traps.2c);rm(traps.2b);rm(traps.2);rm(traps.1)


################################################################################
## Now use crosswalk to append my revised trap name to the full dataset
################################################################################

cdph.wnv2 <- left_join(cdph.wnv,trap.crosswalk,by=c("trap","lat"="cdph.lat"
                                                    ,"lng"="cdph.lng"))
cdph.wnv2$lat <- cdph.wnv2$rev.lat 
cdph.wnv2$lng <- cdph.wnv2$rev.lng 

cdph.wnv2$location <- ifelse(cdph.wnv2$location==''
                             ,paste('(',cdph.wnv2$lat,',',cdph.wnv2$lng,')',sep='')
                             ,cdph.wnv2$location)
cdph.wnv2$lat.num <- coalesce(cdph.wnv2$lat.num,as.numeric(cdph.wnv2$lat))
cdph.wnv2$lng.num <- coalesce(cdph.wnv2$lng.num,as.numeric(cdph.wnv2$lng))

cdph.wnv3 <- cdph.wnv2[,c("test.id","trap.name","location","lat","lng","lat.num","lng.num"
                       ,"satellite.ind"
                       ,"trap_type","test.date","date","yr","mo","week"
                       ,"species"
                       ,"NumMosquitos","WnvPresent"
                       ,"train","validate","test","partition")]
wnv.cdph.observed <- cdph.wnv3


################################################################################
## Now check uniqueness of data
################################################################################




cdph.yr <- my.cdph.aggr(c("yr"),df=wnv.cdph.observed)
cdph.yrmo <- my.cdph.aggr(c("yr","mo"),df=wnv.cdph.observed)
cdph.mo <- my.cdph.aggr(c("mo"),df=wnv.cdph.observed)
cdph.wk <- my.cdph.aggr(c("week"),df=wnv.cdph.observed)
cdph.species <- my.cdph.aggr(c("species"),df=wnv.cdph.observed)
cdph.trap_type <- my.cdph.aggr(c("trap_type"),df=wnv.cdph.observed)
cdph.trap <- my.cdph.aggr(c("trap.name"),df=wnv.cdph.observed)
cdph.location <- my.cdph.aggr(c("location"),df=wnv.cdph.observed)
cdph.trap.location <- my.cdph.aggr(c("trap.name","location"),df=wnv.cdph.observed)

chk.trp <- my.cdph.aggr(c("trap.name","location","trap_type"),df=
                          wnv.cdph.observed[wnv.cdph.observed$location %in% c('(41.87287286249572, -87.7647365320396)'),])


################################################################################
## Now aggregate to trap/date/species level
################################################################################

# first check to see if any data differ (e.g. multiple trap types on same
# date?)
wnv.trap.date.species.1 <- wnv.cdph.observed %>% group_by(trap.name
                                                        ,date
                                                        ,species) %>%
  summarise_all(n_distinct
    
  )
wnv.tds.1b <- summary(wnv.trap.date.species.1)
# wnv.tds.1b[6,] > 1
# Where are there multiple values?  test.id (of course), test.date (hmm), 
#  week (hmm), results

wnv.tds.chkdate <- inner_join(wnv.cdph.observed
                              ,wnv.trap.date.species.1[wnv.trap.date.species.1$test.date > 1
                                                       | wnv.trap.date.species.1$week > 1,]
                              ,by=c("trap.name","date","species")) %>%
  arrange(trap.name,date,species)
# Ok, so differences in time of day are minor.  Differences in week are odd --
# typically off-by-one.  Could be a data entry issue. I'm more inclined to 
# believe that than to think it's a resulted date instead of collection date.


wnv.trap.date.2 <- wnv.cdph.observed %>% group_by(trap.name
                                                          ,date
                                                          ) %>%
  summarise_all(n_distinct
  )
summary(wnv.trap.date.2)
# Good, the trap type never varies for a given date & trap_name.  


wnv.trap.date.species <- wnv.cdph.observed %>% group_by(trap.name
                                                        ,trap_type
                                                          ,date
                                                          ,species) %>%
  summarise(nrows = n(),NumMosquitos = sum(NumMosquitos)
            ,WnvPresent = max(as.integer(WnvPresent)) )

# wnv.trap.date.species[,c("nrows","NumMosquitos","WnvPresent")]  %>% 
#   group_by(nrows) %>% summarise_all(.funs=c(mean,sd,min,max) )


# ftable(as.integer(wnv.trap.date.species$WnvPresent)
# ,wnv.trap.date.species$WnvPresent)

  
################################################################################
## Add culex short name then reshape outcomes
################################################################################

wnv.trap.date.species$identifier <-
  paste(trimws(str_replace(str_replace(tolower(wnv.trap.date.species$species), '/', '_'), 'culex','')),'NumMosquitos',sep='.')

wnv.trap.date.1 <- spread(wnv.trap.date.species[,!colnames(wnv.trap.date.species) %in% c("nrows","WnvPresent","species")]
                          ,identifier,NumMosquitos, fill = NA, convert = FALSE) %>%
  group_by(trap.name,trap_type,date) %>% summarise_all(max)
wnv.trap.date.1[,4:11] <- sapply(wnv.trap.date.1[,4:11],function(x){return(coalesce(x,0))})
wnv.trap.date.1$tot.NumMosquitos <- rowSums(wnv.trap.date.1[,4:11])


wnv.trap.date.species$identifier <-
  paste(trimws(str_replace(str_replace(tolower(wnv.trap.date.species$species), '/', '_'), 'culex','')),'WnvPresent',sep='.')

wnv.trap.date.2 <- spread(wnv.trap.date.species[,!colnames(wnv.trap.date.species) %in% c("nrows","NumMosquitos","species")]
                          ,identifier,WnvPresent, fill = NA, convert = FALSE) %>%
  group_by(trap.name,trap_type,date) %>% summarise_all(max) 
wnv.trap.date.2$any.WnvPresent <- (rowMeans(wnv.trap.date.2[,4:11],na.rm=TRUE) > 0)
wnv.trap.date.2[,4:11] <- sapply(wnv.trap.date.2[,4:11],as.logical)
# If there are no mosquitos, we can code WNV as false.
wnv.trap.date.2[,4:11] <- sapply(wnv.trap.date.2[,4:11],function(x){return(coalesce(x,FALSE))})

wnv.trap.date <- inner_join(wnv.trap.date.1,wnv.trap.date.2)
# as.logical(1)
# as.logical(2)
# as.integer(TRUE)
# as.integer(FALSE)
# ftable(wnv.trap.date.species$WnvPresent)
# 
# str(wnv.cdph.observed$WnvPresent)
  
summary(wnv.trap.date)

  
################################################################################
## Go back and add location and date variables and partition info
################################################################################

summary(traps)
summary(trap.crosswalk)
wnv.traps <- distinct(trap.crosswalk[,c("trap.name","rev.lat","rev.lng","lat.lng.src"
                               ,"satellite.ind")])
wnv.trap.date2 <- inner_join(wnv.traps
                             ,wnv.trap.date,by=c("trap.name"))
colnames(wnv.trap.date2)[colnames(wnv.trap.date2)=="rev.lat"] <- "lat"
colnames(wnv.trap.date2)[colnames(wnv.trap.date2)=="rev.lng"] <- "lng"
wnv.trap.date2$location <- paste('(',wnv.trap.date2$lat,',',wnv.trap.date2$lng,')',sep='')


wnv.trap.date2$yr <- lubridate::year(wnv.trap.date2$date)
wnv.trap.date2$mo <- lubridate::month(wnv.trap.date2$date)
wnv.trap.date2$wk <- lubridate::week(wnv.trap.date2$date)
wnv.trap.date2$day.of.wk <- wday(wnv.trap.date2$date)
wnv.trap.date2$day.of.wk.name <- weekdays(as.POSIXct(wnv.trap.date2$date), abbreviate = T)

wnv.trap.date <- wnv.trap.date2


################################################################################
## Summarize
################################################################################

wnv.td.dayofwk <- wnv.trap.date.aggr(c("day.of.wk","day.of.wk.name"),df=wnv.trap.date2)
wnv.td.yr <- wnv.trap.date.aggr(c("yr"),df=wnv.trap.date2)
wnv.td.mo <- wnv.trap.date.aggr(c("mo"),df=wnv.trap.date2)
wnv.td.trap <- wnv.trap.date.aggr(c("trap.name"),df=wnv.trap.date2)
wnv.td.satellite.ind<- wnv.trap.date.aggr(c("satellite.ind"),df=wnv.trap.date2)


################################################################################
## Wide back to long (no longer sparse)
################################################################################


str(wnv.trap.date2)

wnv.trap.date.species2.1 <- gather(wnv.trap.date2, key = "species", value = "WnvPresent"
                                   , erraticus.WnvPresent
                                   , pipiens.WnvPresent
                                   , pipiens_restuans.WnvPresent
                                   , restuans.WnvPresent 
                                   , salinarius.WnvPresent 
                                   , tarsalis.WnvPresent
                                   , territans.WnvPresent
                                   , unspecified.WnvPresent
                                   , any.WnvPresent)
wnv.trap.date.species2.1$species <- gsub('.WnvPresent','',wnv.trap.date.species2.1$species)
wnv.trap.date.species2.1[wnv.trap.date.species2.1$species=='any',]$species <- 'All'

wnv.trap.date.species2.2 <- gather(wnv.trap.date2, key = "species", value = "NumMosquitos"
                                   , erraticus.NumMosquitos
                                   , pipiens.NumMosquitos
                                   , pipiens_restuans.NumMosquitos
                                   , restuans.NumMosquitos 
                                   , salinarius.NumMosquitos 
                                   , tarsalis.NumMosquitos
                                   , territans.NumMosquitos
                                   , unspecified.NumMosquitos
                                   , tot.NumMosquitos)
wnv.trap.date.species2.2$species <- gsub('.NumMosquitos','',wnv.trap.date.species2.2$species)
wnv.trap.date.species2.2[wnv.trap.date.species2.2$species=='tot',]$species <- 'All'

wnv.trap.date.species2.3 <- inner_join(
  wnv.trap.date.species2.1[,!regexpr('.NumMosquitos',colnames(wnv.trap.date.species2.1)) > -1]
  ,wnv.trap.date.species2.2[,!regexpr('.WnvPresent',colnames(wnv.trap.date.species2.1)) > -1]

)
# 
# , erraticus.WnvPresent
# , pipiens.WnvPresent
# , pipiens_restuans.WnvPresent
# , restuans.WnvPresent 
# , salinarius.WnvPresent 
# , tarsalis.WnvPresent
# , territans.WnvPresent
# , unspecified.WnvPresent
# , any.WnvPresent
# 
# , erraticus.NumMosquitos
# , pipiens.NumMosquitos
# , pipiens_restuans.NumMosquitos
# , restuans.NumMosquitos 
# , salinarius.NumMosquitos 
# , tarsalis.NumMosquitos
# , territans.NumMosquitos
# , unspecified.NumMosquitos
# , any.NumMosquitos



wnv.trap.date.species <- wnv.trap.date.species2.3



################################################################################
## Check tabulations
################################################################################

my.cdph.aggr(c("species"),df=wnv.trap.date.species)
my.cdph.aggr(c("yr"),df=wnv.trap.date.species)


################################################################################
## Save R dataset with minimal modifications
################################################################################


getwd()
setwd(my.path)
getwd()


# save(westnile.unedited, file="pump.unedited.RData", compress = FALSE)
# save(westnile.all, file="westnile.all.RData", compress = FALSE)
# save(westnile.eda, file="westnile.eda.RData", compress = FALSE)

save(wnv.trap.date, file="wnv.trap.date.RData", compress = FALSE)
save(wnv.trap.date.species, file="wnv.trap.date.species.RData", compress = FALSE)

write.csv(wnv.trap.date, paste(my.path,'wnv.trap.date.csv',sep='\\'))
write.csv(wnv.trap.date.species, paste(my.path,'wnv.trap.date.species.csv',sep='\\'))



################################################################################
## Clean up more unnecessary objects
################################################################################








