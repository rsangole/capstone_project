

?transform()


transform(D8, PL8.lag = c(NA, head(PL8, -1)))

dim(head(wnv.trap.date.rev2$USW00014819_PRCP,-1))

# test <- transform(wnv.trap.date.rev2
#                   ,USW00014819_PRCP.lag = USW00014819_PRCP )

# ?transform


####################################
# Add lagged terms
####################################

cols.to.lag <-  c("erraticus.NumMosquitos","pipiens.NumMosquitos"
                  ,"pipiens_restuans.NumMosquitos"
                  ,"restuans.NumMosquitos","salinarius.NumMosquitos","tarsalis.NumMosquitos"        
                  ,"territans.NumMosquitos","unspecified.NumMosquitos","tot.NumMosquitos"             
                  ,"erraticus.WnvPresent","pipiens.WnvPresent","pipiens_restuans.WnvPresent"  
                  ,"restuans.WnvPresent","salinarius.WnvPresent","tarsalis.WnvPresent"          
                  ,"territans.WnvPresent","unspecified.WnvPresent","any.WnvPresent"               
                  ,"USW00014819_PRCP","USW00014819_tavg2","USW00014819_TMAX"             
                  ,"USW00014819_TMIN","USW00094846_PRCP","USW00094846_tavg2"            
                  ,"USW00094846_TMAX","USW00094846_TMIN")
cols.to.lag <-  c(
  # "tot.NumMosquitos","any.WnvPresent"               
  #                 ,
                  "USW00014819_PRCP","USW00014819_tavg2","USW00014819_TMAX"             
                  ,"USW00014819_TMIN","USW00094846_PRCP","USW00094846_tavg2"            
                  ,"USW00094846_TMAX","USW00094846_TMIN")
cols.to.lag

# summary()

make.lag.col.names <- function(col
                               ,lag.terms=c(1)
                               ,suffix=".lag"
                               ,df=wnv.trap.date.rev2) {
  lagged.col <- paste(col,suffix,lag.terms,sep='')
  return(lagged.col) 
}

lagsum <- function(col,x) {
  seqn <- 1:length(col)
  lagsum <- rep(NA,length(col))
  for(i in seqn) {
    if(i>x) {
      lagged.set <- col[(i-x):(i-1)]
      lagsum[i] <- sum(lagged.set)
    }
  }
  return(lagsum)
}

# Create a set of lagged values (1 week prior, 2 weeks, 4, 13, 52)
calc.lag.terms <- function(cols.to.lag
                           ,lag.terms = c(1:30)
                           ,df=wnv.trap.date.rev2) {
  df.out <- NA
  for(col in cols.to.lag) {
    lag.col.values <- sapply(lag.terms,function(x) {lag(df[,col],x)})
    colnames(lag.col.values) <- make.lag.col.names(col,lag=lag.terms)
    df.out <- cbind(df.out,lag.col.values)
  }
  
  return(as.data.frame(df.out[,!colnames(df.out) %in% "df.out"]))
}


wnv.trap.date.rev3 <- calc.lag.terms(cols.to.lag = cols.to.lag)



# install.packages("smooth")
# install.packages("Mcomp")
# 
# require(smooth)
# require(Mcomp)
# 
# ?sma()
# 
# summary(wnv.trap.date.rev2$USW00014819_tavg2)
# sma(wnv.trap.date.rev2$USW00014819_tavg2)
# 
# a <- as.double(rep(1:10,2))
# b <- sma(a,order=3,h=0)
# b$actuals 
# as.vector(b$fitted)
# data.frame(a=a,b=as.vector(b$fitted))
# 
# 
# 
# sma(wnv.trap.date.rev2$USW00014819_tavg2)
# 
# ?stats::filter

wnv.trap.date.rev2 %>% arrange(wnv.trap.date.rev2)

wnv.trap.date.rev2$USW00014819_tavg2
wnv.trap.date.rev2$USW00014819_tavg2.ma7 <- 
  lagsum(wnv.trap.date.rev2$USW00014819_tavg2,7)/7
wnv.trap.date.rev2$USW00014819_tavg2.ma7[1:100]

wnv.trap.date.rev2[1:10000,c('date','USW00014819_TMAX','USW00014819_TMIN'
                            ,'USW00014819_tavg2','USW00014819_tavg2.ma7')]

# chi.weather.v1[1:10000,c('date','USW00014819_TMAX','USW00014819_TMIN'
#                              ,'USW00014819_tavg2','USW00014819_tavg2.ma7')]




## 7-day moving average of average daily temperature for each station

chi.weather.v1$USW00014819_tavg2.ma7 <- 
  lagsum(chi.weather.v1$USW00014819_tavg2,7)/7
chi.weather.v1$USW00094846_tavg2.ma7 <- 
  lagsum(chi.weather.v1$USW00094846_tavg2,7)/7

## 60-day moving average of average daily temperature for each station

chi.weather.v1$USW00014819_tavg2.ma60 <- 
  lagsum(chi.weather.v1$USW00014819_tavg2,60)/60
chi.weather.v1$USW00094846_tavg2.ma60 <- 
  lagsum(chi.weather.v1$USW00094846_tavg2,60)/60

# Count of zero-precicipation days in past week

# chi.weather.v1$USW00014819_norain7 <- 
#   lagsum(chi.weather.v1$USW00014819_PRCP == 0,7)
# chi.weather.v1$$USW00094846_norain7 <- 
#   lagsum(chi.weather.v1$$USW00094846_PRCP == 0,7)

# Total volume of rain in past 7 days

chi.weather.v1$USW00014819_PRCP.sum7 <- 
  lagsum(chi.weather.v1$USW00014819_PRCP,7)
chi.weather.v1$USW00094846_PRCP.sum7 <- 
  lagsum(chi.weather.v1$USW00094846_PRCP,7)

# Total volume of rain in past 60 days

chi.weather.v1$USW00014819_PRCP.sum60 <- 
  lagsum(chi.weather.v1$USW00014819_PRCP,60)
chi.weather.v1$USW00094846_PRCP.sum60 <- 
  lagsum(chi.weather.v1$USW00094846_PRCP,60)

# Add 7, 14, 21, 28, 45, 60, 90, 120 day lagged values of each of the above

chi.weather.v1$USW00014819_PRCP.sum60.lag7 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,7)
chi.weather.v1$USW00014819_PRCP.sum60.lag14 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,14)
chi.weather.v1$USW00014819_PRCP.sum60.lag21 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,21)
chi.weather.v1$USW00014819_PRCP.sum60.lag28 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,28)
chi.weather.v1$USW00014819_PRCP.sum60.lag35 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,35)
chi.weather.v1$USW00014819_PRCP.sum60.lag42 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,42)
chi.weather.v1$USW00014819_PRCP.sum60.lag49 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,49)
chi.weather.v1$USW00014819_PRCP.sum60.lag56 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,56)
chi.weather.v1$USW00014819_PRCP.sum60.lag63 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,63)
chi.weather.v1$USW00014819_PRCP.sum60.lag90 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,90)
chi.weather.v1$USW00014819_PRCP.sum60.lag120 <- lag(chi.weather.v1$USW00014819_PRCP.sum60,120)

chi.weather.v1$USW00014819_tavg2.ma60.lag7 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,7)
chi.weather.v1$USW00014819_tavg2.ma60.lag14 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,14)
chi.weather.v1$USW00014819_tavg2.ma60.lag21 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,21)
chi.weather.v1$USW00014819_tavg2.ma60.lag28 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,28)
chi.weather.v1$USW00014819_tavg2.ma60.lag35 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,35)
chi.weather.v1$USW00014819_tavg2.ma60.lag42 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,42)
chi.weather.v1$USW00014819_tavg2.ma60.lag49 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,49)
chi.weather.v1$USW00014819_tavg2.ma60.lag56 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,56)
chi.weather.v1$USW00014819_tavg2.ma60.lag63 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,63)
chi.weather.v1$USW00014819_tavg2.ma60.lag90 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,90)
chi.weather.v1$USW00014819_tavg2.ma60.lag120 <- lag(chi.weather.v1$USW00014819_tavg2.ma60,120)

chi.weather.v1$USW00094846_PRCP.sum60.lag7 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,7)
chi.weather.v1$USW00094846_PRCP.sum60.lag14 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,14)
chi.weather.v1$USW00094846_PRCP.sum60.lag21 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,21)
chi.weather.v1$USW00094846_PRCP.sum60.lag28 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,28)
chi.weather.v1$USW00094846_PRCP.sum60.lag35 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,35)
chi.weather.v1$USW00094846_PRCP.sum60.lag42 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,42)
chi.weather.v1$USW00094846_PRCP.sum60.lag49 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,49)
chi.weather.v1$USW00094846_PRCP.sum60.lag56 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,56)
chi.weather.v1$USW00094846_PRCP.sum60.lag63 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,63)
chi.weather.v1$USW00094846_PRCP.sum60.lag90 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,90)
chi.weather.v1$USW00094846_PRCP.sum60.lag120 <- lag(chi.weather.v1$USW00094846_PRCP.sum60,120)

chi.weather.v1$USW00094846_tavg2.ma60.lag7 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,7)
chi.weather.v1$USW00094846_tavg2.ma60.lag14 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,14)
chi.weather.v1$USW00094846_tavg2.ma60.lag21 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,21)
chi.weather.v1$USW00094846_tavg2.ma60.lag28 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,28)
chi.weather.v1$USW00094846_tavg2.ma60.lag35 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,35)
chi.weather.v1$USW00094846_tavg2.ma60.lag42 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,42)
chi.weather.v1$USW00094846_tavg2.ma60.lag49 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,49)
chi.weather.v1$USW00094846_tavg2.ma60.lag56 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,56)
chi.weather.v1$USW00094846_tavg2.ma60.lag63 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,63)
chi.weather.v1$USW00094846_tavg2.ma60.lag90 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,90)
chi.weather.v1$USW00094846_tavg2.ma60.lag120 <- lag(chi.weather.v1$USW00094846_tavg2.ma60,120)

# summary(chi.weather.v1)
# kdepairs(chi.weather.v1)


str(chi.weather.v1)


################################################################################
## Merge new variables into trap results dataset
################################################################################


wnv.trap.date.rev3 <- left_join(
  wnv.trap.date.rev2
  ,cbind(date=chi.weather.v1$date
         ,chi.weather.v1[,!colnames(chi.weather.v1) %in% colnames(wnv.trap.date.rev2)])
  ,by=c("date")
)


################################################################################
## Save trap/date pairs with moving average and lagged weather variables
################################################################################

# subset to rows with trap results
wnv.trap.date.rev3b <- wnv.trap.date.rev3[!is.na(wnv.trap.date.rev3$any.WnvPresent),]

base.path <- "D:\\ajc188\\github\\capstone_project"

my.path <- paste(base.path,"data","processed",sep="\\")


getwd()
setwd(my.path)
getwd()
list.files()


save(wnv.trap.date.rev3b, file="wnv.trap.date.rev3b.RData", compress = FALSE)
write.csv(wnv.trap.date.rev3b, paste(my.path,'wnv.trap.date.rev3b.csv',sep='\\'),row.names = FALSE)






