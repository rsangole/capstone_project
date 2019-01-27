
################################################################################
## Libraries
################################################################################


# install.packages("zoo")
require("zoo")
# require("RODBC")
# install.packages("sqldf")
# require("sqldf")


# install.packages("plyr")
require(plyr)

# install.packages("dplyr")
library('dplyr')
# install.packages("dplyr")
require("tidyr")

# install.packages("ggplot2")
library('ggplot2')
# install.packages("robustbase")
# install.packages("caret")
library("caret")
# install.packages("randomForest")
require(randomForest)

require(foreign)
require(nnet)

# install.packages("ngram")
library(ngram)
# install.packages("hunspell")
library(hunspell)


# install.packages("gbm")
require(gbm)
# install.packages("xgboost")
require(xgboost)
# install.packages("e1071")
require(e1071)

library(reshape2)

# install.packages("ResourceSelection")
require("ResourceSelection")

require("MASS")


# install.packages("forecast")
require("forecast")

# install.packages("pscl")
require("pscl")

# install.packages("glmnet")
require(glmnet)

# install.packages("tibble")
require(tibble)

require("scales")


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
## Load Kaggle datasets
################################################################################

westnile.spray <- read.csv(paste(src.path,"spray.csv",sep="\\"),stringsAsFactors = FALSE)
westnile.test <- read.csv(paste(src.path,"test.csv",sep="\\"),stringsAsFactors = FALSE)
westnile.train <- read.csv(paste(src.path,"train.csv",sep="\\"),stringsAsFactors = FALSE)
westnile.weather <- read.csv(paste(src.path,"weather.csv",sep="\\"),stringsAsFactors = FALSE)
westnile.sample <- read.csv(paste(src.path,"sampleSubmission.csv",sep="\\"),stringsAsFactors = FALSE)


################################################################################
## Load other datasets
################################################################################

cdph.csv <- "West_Nile_Virus__WNV__Mosquito_Test_Results.csv"

westnile.cdph <- read.csv(paste(base.path,'cdph data',cdph.csv,sep="\\"),stringsAsFactors = FALSE)

str(westnile.cdph)
str(westnile.train)



################################################################################
## Data manipulation
################################################################################

# Create blank variables for the outcomes for test observations
westnile.test$NumMosquitos <- rep(NA,dim(westnile.test)[1])
westnile.test$WnvPresent <- rep(NA,dim(westnile.test)[1])

westnile.train <- cbind(data.frame(Id=(max(westnile.test$Id)+1):(max(westnile.test$Id)+dim(westnile.train)[1]))
                        ,westnile.train)

westnile.train$train <- TRUE
westnile.test$train <- FALSE 


westnile.both <- rbind(westnile.train,westnile.test) 

# str(westnile.train)
# str(westnile.test)
# 
# str(westnile.both)


westnile.both$date <- as.Date(westnile.both$Date)
westnile.both$yr <- lubridate::year(westnile.both$date)
westnile.both$mo <- lubridate::month(westnile.both$date)
westnile.both$date2 <- as.POSIXct(westnile.both$date, format="%Y-%m-%d")

westnile.both <- westnile.both %>% arrange(date)


westnile.spray$date <- as.Date(westnile.spray$Date)
westnile.spray$yr <- lubridate::year(westnile.spray$date)
westnile.spray$mo <- lubridate::month(westnile.spray$date)
westnile.spray$date2 <- as.POSIXct(westnile.spray$date, format="%Y-%m-%d")

westnile.weather$date <- as.Date(westnile.weather$Date)
westnile.weather$yr <- lubridate::year(westnile.weather$date)
westnile.weather$mo <- lubridate::month(westnile.weather$date)
westnile.weather$date2 <- as.POSIXct(westnile.weather$date, format="%Y-%m-%d")


################################################################################
## Aggregation function
################################################################################

## Example
# westnile.both.trap.dt <- westnile.both %>% group_by(Trap,date) %>%
#   summarise(nrows = n()
#             ,address.cnt = n_distinct(Address)
#             ,train = sum(train)
#             ,train.pct = round(100*sum(train)/n(),1)
#             ,wnv.present = sum(WnvPresent==1,na.rm=TRUE)
#             ,wnv.present.pct = round(100*sum(WnvPresent==1,na.rm=TRUE)/n(),1)
#             ,wnv.absent = sum(WnvPresent==0,na.rm=TRUE)
#             ,wnv.absent.pct = round(100*sum(WnvPresent==0,na.rm=TRUE)/n(),1)
#             ,wnv.na = sum(is.na(WnvPresent))
#             ,fst.date = min(date)
#             ,lst.date = max(date)
#   )

## Function
my.aggr <- function(cols,df=westnile.both,limit.vars = 150) {
  
  # Add an id variable if it doesn't exist on the data.frame  
  if(!("id" %in% colnames(df))) {
    df$id <- 1:dim(df)[1]
  }
  
  # If the outcome variables don't exist, make them.  Assume 0 cases.
  # if(!("any_cases" %in% colnames(df))) {
  #   df$any_cases <- df$total_cases > 0 
  # }
  
  # If we're limiting to the first X variables, then subset columns as needed.
  if(!is.na(limit.vars)) {
    limit.vars <- min(limit.vars,dim(df)[2])
    df2 <- df[,1:limit.vars]
    
    # May need rewriting to accommodate lists
    for (col in cols) {
      if(!col %in% colnames(df2)) {
        df2b <- df[,c("id",cols)]
        df2 <- inner_join(df2b,df2,by="id")
      }
    }
  }
  else df2 <- df
  
  # Group by one or more columns specified in the cols parameter
  # One thing we're doing is counting unique values of each variable.
  df2 <- df2 %>% group_by(!!! syms(cols)) %>%
    summarise_all(n_distinct)
  # print(summary(df2))
  df2.names.old <- colnames(df2)[!colnames(df2) %in% cols]
  df2.names.new <- paste(df2.names.old,'unique',sep='.')
  df2 <- df2 %>% rename_at(vars(df2.names.old), ~ df2.names.new)
  # print(summary(df2))
  
  # Another thing we're doing is running some metrics (aggregation)
  df3 <- df 
  df3$missing.ind = FALSE
  df3 <- df3 %>% group_by(!!! syms(cols)) %>%
    summarise(nrows = n()
              ,address.cnt = n_distinct(Address)
              ,train.n = sum(train)
              ,train.pct = round(100*sum(train)/n(),1)
              ,test.n = sum(!train)
              ,test.pct = round(100*sum(!train)/n(),1)
              ,wnv.present.n = sum(WnvPresent==1,na.rm=TRUE)
              ,wnv.present.pct = round(100*sum(WnvPresent==1,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.absent.n = sum(WnvPresent==0,na.rm=TRUE)
              ,wnv.absent.pct = round(100*sum(WnvPresent==0,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.na.n = sum(is.na(WnvPresent))
              ,wnv.na.pct = round(100*sum(is.na(WnvPresent))/n(),1)
              ,fst.date = min(date)
              ,lst.date = max(date)
              ,tot.Mosquitos = sum(NumMosquitos) 
              ,mean.Mosquitos = mean(NumMosquitos) 
              ,sd.Mosquitos = sd(NumMosquitos) 
              ,seqn = row_number()
    )
  
  return(inner_join(df3,df2,by=cols))
}



################################################################################
## EDA: tabular aggregation 
################################################################################

aggr.trap <- my.aggr(c("Trap"))
aggr.trap.dt <- my.aggr(c("Trap","date"))
aggr.yr <- my.aggr(c("yr"))
aggr.mo <- my.aggr(c("mo"))
aggr.yrmo <- my.aggr(c("yr","mo"))

aggr.train <- my.aggr("train")
aggr.WnvPresent <- my.aggr("WnvPresent")
aggr.Species <- my.aggr("Species")

# aggr.trap.train.dt <- my.aggr(c("Trap","date","train"))
aggr.trap.species.dt <- my.aggr(c("Trap","date","Species"))

ftable(aggr.trap$address.cnt)
hist(aggr.trap.dt$nrows)
ftable(aggr.trap.dt$nrows)
# Check out data where we have lots of rows for a given trap date
# mult.same.date <- westnile.both[westnile.both$Trap %in% c(
#   aggr.trap.dt[aggr.trap.dt$nrows > 20,]$Trap
# ),] %>% arrange(Trap,date)
mult.same.date <- semi_join(westnile.both,aggr.trap.dt[aggr.trap.dt$nrows > 20,]
                            ,c("Trap","date")) %>% arrange(Trap,date)

# df = data.frame(date = c(20121201, 20121220, 20130101, 20130115, 20130201),
#                 val  = c(10, 5, 8, 20, 4))
# plot(cumsum(rowsum(df$val, df$date)), type = "l")
# ggplot(df, aes(x=1:5, y=cumsum(val))) + geom_line() + geom_point()


################################################################################
## Make a lookup table for species (plus some aggregate data) 
################################################################################

species <- my.aggr("Species")
require(stringr)
# require(purrr)
species <- cbind(species.id = 1:dim(species)[1]
                 ,culex = trimws(str_replace(str_replace(tolower(species$Species), '/', '_'), 'culex',''))
                 ,species)

################################################################################
## Make a lookup table for trap (plus some aggregate data) 
################################################################################

traps <- my.aggr(c("Trap","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy"))
traps2 <- my.aggr(c("Trap")) 
# Note that Traps T009 and T035 have two different addresses each.  The former 
# are probably just municipality/avenue-vs-road issues but the same trap and same location.  
#  I would presume the Chicago location is correct.  Or they could be different traps.
# The latter are probably different traps.  
# Also, Block is probably not very useful.  It's the block number going north, 
#  south, east or west from the intersection of State & Madison.  Not useful by
#  itself.  
install.packages("rowid")
require(rowid)
rowid(traps)

# Give all traps a unique name assuming that addresses and lat/lng pairs accurately
#  differentiate between traps
traps <- traps %>% arrange(Trap,Address,Block,Street,AddressNumberAndStreet,Latitude,Longitude,AddressAccuracy) %>%
    group_by(Trap) %>% mutate(
      trap2 = ifelse(row_number() > 1,paste(Trap,row_number(),sep='.'),Trap)
    ) 
# Give all traps a unique number
traps$trap.id <- 1:dim(traps)[1]


westnile.both$culex <- trimws(str_replace(str_replace(tolower(westnile.both$Species), '/', '_'), 'culex',''))
# Append unique trap.id and trap2 names to the data
westnile.both2 <- left_join(westnile.both
                            ,traps[,c("Trap","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy"
                                      ,"trap.id","trap2")]
)

################################################################################
## Check uniqueness of rows in test set
################################################################################

westnile.test2 <- westnile.both[!westnile.both$train,]
westnile.test2b <- westnile.test2 %>% arrange(trap2,date,Species) %>%
    group_by(trap2,date,Species) %>% summarise(nrows = n())
ftable(westnile.test2b$nrows)
westnile.test2c <- inner_join(westnile.test2,westnile.test2b[westnile.test2b$nrows>1,])
# So, combinations of trap/date/species are not unique.  
# This is a minor issue.  In effect, kaggle is weighting results for some 
# traps/date/species combinations -- or giving people the chance to hedge 
# their bets.  But for modeling purposes, we can ignore this.  It is probably
# safe to reduce this to the trap/date/species level and maintain a mapping
# from that to competition Id so that submissions can be constructed with the
# proper number of rows.  
# (All of this assumes that we don't really have multiple test values at 
#  different times during the day, which might be the case -- but still no 
#  easy way to handle for test set predictions.)

# Uniqueness of training data is a bit more complicated.  Should we assume 
#  there are multiple tests per day?  Keep all versions for modeling purposes?
#  Or reduce to one result per trap/date/species combination?  If the latter 
#  do we sum or max the number of mosquitos?  

# Ahh, I forgot that data were organized in batches of 50 mosquitos.  
# So, for training data, we could potentially sum the mosquito counts.  
# But for test data, well, it's a little weird.  Do we assume that if there
#  is more than one row, it means there were more than 50 mosquitos?  But then
#  we're still left with the challenge of guessing how many batches of 50 have
#  one or more positive WNV.  


################################################################################
## Check uniqueness of rows in training set
################################################################################

westnile.train2 <- westnile.both[westnile.both$train,]
westnile.train2b <- westnile.train2 %>% arrange(trap2,date,Species) %>%
  group_by(trap2,date,Species) %>% summarise(nrows = n())
ftable(westnile.train2b$nrows)
westnile.train2c <- inner_join(westnile.train2,westnile.train2b[westnile.train2b$nrows==2,])
westnile.train2d <- inner_join(westnile.train2,westnile.train2b[westnile.train2b$nrows>2,])


################################################################################
## Add culex short name to westnile.both then reshape outcomes
################################################################################


westnile.both$trap.id <- westnile.both2$trap.id 
westnile.both$trap2 <- westnile.both2$trap2 
rm(westnile.both2) 

westnile.reshape1 <- cbind(identifier = paste(trimws(str_replace(str_replace(tolower(westnile.both$Species), '/', '_'), 'culex','')),'WnvPresent',sep='.')
                             ,westnile.both[,!colnames(westnile.both) %in% c("Trap","Species","culex","Id","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy")])
westnile.reshape2 <- distinct(westnile.reshape1)

westnile.reshape3 <- spread(westnile.reshape2,identifier,WnvPresent, fill = NA, convert = FALSE)


# this fails because i haven't properly accounted for having multiple rows in a given
# location/time (due to batches of 50)

# westnile.reshape1 <- westnile.both[,!colnames(westnile.both) %in% c("Trap","Species","Id","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy")]
# 
# westnile.reshape1 <- cbind(culex = paste(trimws(str_replace(str_replace(tolower(westnile.both$Species), '/', '_'), 'culex','')),'WnvPresent',sep='.')
#                            ,westnile.both[,!colnames(westnile.both) %in% c("Species","culex","Id","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy")])
# 
# 
# westnile.trap.date.culex <- spread(
#   cbind(culex = paste(trimws(str_replace(str_replace(tolower(westnile.both$Species), '/', '_'), 'culex','')),'WnvPresent',sep='.')
#         ,westnile.both[,!colnames(westnile.both) %in% c("Species","culex","Id","Address","Block","Street","AddressNumberAndStreet","Latitude","Longitude","AddressAccuracy")])
#   ,culex
#   ,WnvPresent, fill = NA, convert = FALSE)
# 
# westnile.both2 <- spread(westnile.reshape1, paste(culex,'WnvPresent',sep='.'), WnvPresent, fill = NA, convert = FALSE)
# westnile.both2 <- spread(westnile.both2, paste(culex,'NumMosquitos',sep='.'), NumMosquitos, fill = NA, convert = FALSE)



################################################################################
## Restructure CDPH data to see how it compares with Kaggle set
################################################################################

str(westnile.cdph)



ftable(westnile.cdph$NUMBER.OF.MOSQUITOES)
ftable(westnile.both$NumMosquitos)

ftable(westnile.cdph$SPECIES)
ftable(westnile.cdph$BLOCK)
ftable(westnile.cdph$RESULT)
ftable(westnile.cdph$TRAP_TYPE)

westnile.cdph$yr <- westnile.cdph$SEASON.YEAR
westnile.cdph$date <- as.Date(substring(westnile.cdph$TEST.DATE,1,10),"%m/%d/%Y")
# westnile.cdph$date2 <- as.POSIXlt(westnile.cdph$TEST.DATE, format="%m/%d/%Y %H:%M:%OS %p")
westnile.cdph$mo <- lubridate::month(westnile.cdph$date)
westnile.cdph$WnvPresent <- as.factor(westnile.cdph$RESULT == 'positive')
westnile.cdph$NumMosquitos <- westnile.cdph$NUMBER.OF.MOSQUITOES

# ftable(westnile.cdph$WnvPresent,westnile.cdph$RESULT)

ftable(westnile.cdph$yr ,westnile.cdph$SEASON.YEAR )

# westnile.cdph <- westnile.cdph[,!colnames(westnile.cdph) %in% c("date2")]



# westnile.cdph %>% group_by(SEASON.YEAR) %>% summarise(nrows=n(),species = n_distinct(SPECIES)
#                                                        ,NumMosquitos = sum(NUMBER.OF.MOSQUITOES)
#                                                        ,WnvPresent = sum(RESULT=='positive'))


my.cdph.aggr <- function(cols,df=westnile.cdph,limit.vars = 150) {
  
  # Add an id variable if it doesn't exist on the data.frame  
  if(!("id" %in% colnames(df))) {
    df$id <- 1:dim(df)[1]
  }
  
  # If the outcome variables don't exist, make them.  Assume 0 cases.
  # if(!("any_cases" %in% colnames(df))) {
  #   df$any_cases <- df$total_cases > 0 
  # }
  
  # If we're limiting to the first X variables, then subset columns as needed.
  if(!is.na(limit.vars)) {
    limit.vars <- min(limit.vars,dim(df)[2])
    df2 <- df[,1:limit.vars]
    
    # May need rewriting to accommodate lists
    for (col in cols) {
      if(!col %in% colnames(df2)) {
        df2b <- df[,c("id",cols)]
        df2 <- inner_join(df2b,df2,by="id")
      }
    }
  }
  else df2 <- df
  
  # Group by one or more columns specified in the cols parameter
  # One thing we're doing is counting unique values of each variable.
  df2 <- df2[,!colnames(df2) %in% c("date2")] %>% group_by(!!! syms(cols)) %>%
    summarise_all(n_distinct)
  # print(summary(df2))
  df2.names.old <- colnames(df2)[!colnames(df2) %in% cols]
  df2.names.new <- paste(df2.names.old,'unique',sep='.')
  df2 <- df2 %>% rename_at(vars(df2.names.old), ~ df2.names.new)
  # print(summary(df2))
  
  # Another thing we're doing is running some metrics (aggregation)
  df3 <- df 
  df3$missing.ind = FALSE
  df3 <- df3 %>% group_by(!!! syms(cols)) %>%
    summarise(nrows = n()
              # ,address.cnt = n_distinct(Address)
              ,latlng.cnt = n_distinct(LOCATION)
              # ,train.n = sum(train)
              # ,train.pct = round(100*sum(train)/n(),1)
              # ,test.n = sum(!train)
              # ,test.pct = round(100*sum(!train)/n(),1)
              ,wnv.present.n = sum(WnvPresent == TRUE,na.rm=TRUE)
              ,wnv.present.pct = round(100*sum(WnvPresent == TRUE,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.absent.n = sum(WnvPresent == FALSE,na.rm=TRUE)
              ,wnv.absent.pct = round(100*sum(WnvPresent == FALSE,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.na.n = sum(is.na(WnvPresent))
              ,wnv.na.pct = round(100*sum(is.na(WnvPresent))/n(),1)
              ,fst.date = min(date)
              ,lst.date = max(date)
              ,tot.Mosquitos = sum(NumMosquitos) 
              ,mean.Mosquitos = mean(NumMosquitos) 
              ,sd.Mosquitos = sd(NumMosquitos) 
              ,seqn = row_number()
    )
  
  return(inner_join(df3,df2,by=cols))
}



cdph.yr <- my.cdph.aggr(c("yr"))
cdph.yrmo <- my.cdph.aggr(c("yr","mo"))
cdph.mo <- my.cdph.aggr(c("mo"))
cdph.wk <- my.cdph.aggr(c("WEEK"))
cdph.species <- my.cdph.aggr(c("SPECIES"))
cdph.trap_type <- my.cdph.aggr(c("TRAP_TYPE"))
cdph.trap <- my.cdph.aggr(c("TRAP"))
cdph.location <- my.cdph.aggr(c("LOCATION"))
cdph.trap.location <- my.cdph.aggr(c("TRAP","LOCATION"))

ftable(cdph.location$TRAP.unique) # A lot of issues with one location.  Let's see why.
ftable(cdph.location$BLOCK.unique) # Blocks are all unique.
location.mult.trap <- semi_join(cdph.trap.location,cdph.location[cdph.location$TRAP.unique > 1,]
                           ,c("LOCATION")) %>% arrange(LOCATION,TRAP)
location.mult.trap2 <- inner_join(westnile.cdph,location.mult.trap,by=c("TRAP","LOCATION")) %>%
  arrange(LOCATION,date,TRAP)

## There are two issues.  First, T031 and T167 have same location.  
## Second, there are a lot of missing locations.
## So, we have a couple options.  We could geocode the block.  We could use 
## the Kaggle dataset where block was geocoded to give us a slightly lower 
## quality lat/lng pair anyway.  
## Anyway, we can do this data clean-up later.  (Very soon, but not immediately.)


ftable(cdph.trap$LOCATION.unique) # A couple non-unique locations for a given trap.  
ftable(cdph.trap$BLOCK.unique) # Blocks are almost all unique; exceptions same as non-unique locations for a given trap.
trap.mult.loc <- semi_join(cdph.trap.location,cdph.trap[cdph.trap$LOCATION.unique > 1,]
                            ,c("TRAP")) %>% arrange(TRAP,LOCATION)
trap.mult.loc2 <- inner_join(westnile.cdph,trap.mult.loc,by=c("TRAP","LOCATION")) %>%
  arrange(TRAP,date,LOCATION)
  
## T035 missing locations can probably be assumed same as T035 non-missing? 
##  No, on further examination, the block has changed.  Should probably just 
##  approximate the lat/lng via the block.  
## T002B has moved locations but not sure if it's a real move?  Yes.  


cdph.trap.date.species <- my.cdph.aggr(c("TRAP","date","SPECIES"))
ftable(cdph.trap.date.species$nrows)
# Still need to do data cleanup!  For a given combination of trap/date/species,
# we sometims have many observations.  Let's spot check a few.

westnile.cdph.mults <- inner_join(westnile.cdph
                                  ,cdph.trap.date.species[cdph.trap.date.species$nrows %in% c(2,3,4),]
                                  ,by=c("TRAP","date","SPECIES")) %>%
  arrange(TRAP,date,SPECIES)
## Ok, this is actually exactly what we see in the kaggle data.  They cap a 
## given row at 50 then start a new row for each additional batch of mosquitos
## up to 50.  

## For predictive purposes, we might choose to ignore this and simply sum 
## mosquitos and check for any POSITIVE for the TRAP/date/SPECIES combination.
## Alternatively, we could possibly  calculate a weighted version of the positive
## outcome.  Not sure it's worth it.  



################################################################################
## Cleanup
################################################################################

rm(westnile.both.trap)
rm(westnile.both.trap.mo)
rm(westnile.both.trap.dt)
rm(westnile.both.mo)
rm(westnile.both.yr)
rm(westnile.both.trap.yr)
rm(westnile.both.trap.yrmo)
rm(westnile.both.trap.train)
rm(westnile.both.yrmo)
rm(aggr.dt)
rm(aggr.mo)
rm(aggr.Species)
rm(aggr.train)
rm(aggr.trap)
rm(aggr.trap.train.dt)
rm(aggr.trap.dt)
rm(aggr.trap.species.dt)
rm(aggr.WnvPresent)
rm(aggr.yr)
rm(aggr.yrmo)



################################################################################
## EDA
################################################################################

summary(westnile.spray) ## just date, time, lat, lng
summary(westnile.weather) ## station, date, lots of other data
summary(westnile.test) ## id, date, address, species, block, street, trap, address # and street, lt, lng, address accuracy
summary(westnile.train) ## same, plus NumMosquitos, WnvPresent
summary(westnile.sample) ## id, WnvPresent

hist(westnile.train$NumMosquitos)
ftable(westnile.train$WnvPresent)

## So, predicting # of mosquitos is an intermediate step. Real goal is WNV Y/N.  

str(westnile.spray)
str(westnile.weather)
str(westnile.test)
str(westnile.train)
str(westnile.sample)


# ftable(westnile.weather$Station) 
# str(westnile.weather)


westnile.weather %>% group_by(Station) %>% 
  summarise(nrows=n(),min.date = min(date),max.date=max(date))
# There are two stations.  They're not identified, but I'd bet they're 
# O'Hare and Midway weather stations.  

  
str(westnile.test)


westnile.spray %>% summarise(nrows=n(),min.date = min(date),max.date=max(date))
westnile.train %>% summarise(nrows=n(),min.date = min(date),max.date=max(date))
westnile.test %>% summarise(nrows=n(),min.date = min(date),max.date=max(date))


# hist(westnile.train$date)
# hist(westnile.test$date)



# summary(westnile.test$Id)

# westnile.train$Id <- (max(westnile.test$Id)+1):(max(westnile.test$Id)+dim(westnile.train)[1])




# westnile.test <- read.csv(paste(src.path,"westnile_features_test.csv",sep="\\"))
# westnile.train <- read.csv(paste(src.path,"westnile_features_train.csv",sep="\\"))
# westnile.labels <- read.csv(paste(src.path,"westnile_labels_train.csv",sep="\\"))
# westnile.sample.submission <- read.csv(paste(src.path,"submission_Format.csv",sep="\\"))


dim(westnile.test)
dim(westnile.train)
dim(westnile.labels)
dim(westnile.sample.submission)

# test.u <- anti_join(pump.labels,pump.data)
# test.u <- inner_join(pump.data2,sample.competition.u)

summary(westnile.test)
summary(westnile.train)
summary(westnile.labels)
summary(westnile.sample.submission)


################################################################################
## EDA
################################################################################

yrs.of.data <- westnile.both %>% group_by(yr=lubridate::year(date),mo=lubridate::month(date)) %>%
    summarise(nrows=n(),n.train = sum(train),n.test = n() - sum(train))

yrs.of.spray.data <- westnile.spray %>% group_by(yr=lubridate::year(date),mo=lubridate::month(date)) %>%
  summarise(nrows=n())

yrs.of.weather.data <- westnile.weather %>% group_by(yr=lubridate::year(date),mo=lubridate::month(date)) %>%
  summarise(nrows=n())



str(westnile.both$date)

westnile.both$date2 <- as.POSIXct(westnile.both$date, format="%Y-%m-%d")

ggplot(westnile.both,aes(x=date)) + 
  geom_histogram(data=subset(westnile.both,!train),fill = "yellow", alpha = 0.5,binwidth=30) +
  geom_histogram(data=subset(westnile.both,train),fill = "red", alpha = 0.5,binwidth=30) +
  scale_x_date(date_breaks ="12 months"
               # , date_breaks_minor = "3 months"
               , labels=date_format("%Y %b")
               ,limits=c(as.Date("2007-01-01"),as.Date("2014-12-31"))
               ) +
  theme(axis.title.x=element_blank())

  ggtitle(paste('Histogram of mean percent with each status for levels of ',paste(cols, collapse=","),sep='')) 
# coord_cartesian(ylim=c(0,100))




################################################################################
## Try a really basic model
################################################################################
  
  
# my.train <- westnile.both[westnile.both$train & westnile.both$yr %in% c(2007,2009,2011),] 
# my.validate <- westnile.both[westnile.both$train & westnile.both$yr %in% c(2013),] 

my.partitions <- westnile.both[westnile.both$train,]
my.partitions$validate <- my.partitions$yr %in% c(2013)

my.partitions$WnvPresent <- as.factor(my.partitions$WnvPresent)
my.partitions$Species <- as.factor(my.partitions$Species)
my.partitions$trap2 <- as.factor(my.partitions$trap2)

rf.predictors.01 <- colnames(my.partitions[,!colnames(my.partitions) %in% c("train","Address","Date",'Block'
                                                                           ,'Street','Trap','trap.id'
                                                                           ,"AddressNumberAndStreet"
                                                                           ,'AddressAccuracy'
                                                                           ,'Id','culex','date2'
                                                                           ,'validate','WnvPresent','trap2')])
chk <- my.partitions[,colnames(my.partitions) %in% c(rf.predictors.01,"WnvPresent")]
summary(chk)
rf.formula.01 <- as.formula(paste("WnvPresent~"
                                        ,gsub('\n','',paste(rf.predictors.01,collapse='+'))
                                        ,sep=""))

set.seed(2019)
rf.model01 <- randomForest(rf.formula.01,data = my.partitions
                             , subset = !validate
                             , ntree = 500
                             # , mtry = 26
)
rf.predictions01 <- predict(rf.model01,my.partitions)

# score.model(pump.rf2a2c3.predict,actual = pump.eda3x6$status_group
#             ,part.name = pump.eda3x6$part.name)

varImpPlot(rf.model01,type=2)


  


################################################################################
## Save R dataset with minimal modifications
################################################################################


getwd()
setwd(my.path)
getwd()


# save(westnile.unedited, file="pump.unedited.RData", compress = FALSE)
# save(westnile.all, file="westnile.all.RData", compress = FALSE)
# save(westnile.eda, file="westnile.eda.RData", compress = FALSE)




################################################################################
## Clean up more unnecessary objects
################################################################################








