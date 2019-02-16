

# * 2wk_precip - For each day, the preceding 2-week average for precip
# * [ ]  4wk_precip
# * [ ]  90day_precip
# * [ ]  2wk_tavg
# * [ ]  4wk_tavg
# * [ ]  2wk_mintemp
# * [ ]  4wk_mintemp



################################################################################
# Quick-and-dirty functions for lagged terms

make.lag.col.names <- function(col
                               ,lag.terms=c(1)
                               ,suffix=".lag"
                               ,df=wnv.trap.date.rev2) {
  lagged.col <- paste(col,suffix,lag.terms,sep='')
  return(lagged.col) 
}


# lagsum <- function(col,x) {
#   seqn <- 1:length(col)
#   lagsum <- rep(NA,length(col))
#   for(i in seqn) {
#     if(i>x) {
#       lagged.set <- col[(i-x):(i-1)]
#       lagsum[i] <- sum(lagged.set)
#     }
#   }
#   return(lagsum)
# }


lagfun <- function(col,x,fun=sum) {
  seqn <- 1:length(col)
  lagfun <- rep(NA,length(col))
  for(i in seqn) {
    if(i>x) {
      lagged.set <- col[(i-x):(i-1)]
      lagfun[i] <- fun(lagged.set)
    }
  }
  return(lagfun)
}



# Create a set of lagged values (e.g. 1 week prior, 2 weeks, 4, 13, 52)
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


################################################################################
# Add lagged terms

# * 2wk_precip - For each day, the preceding 2-week average for precip
# * [ ]  4wk_precip
# * [ ]  90day_precip
# * [ ]  2wk_tavg
# * [ ]  4wk_tavg
# * [ ]  2wk_mintemp
# * [ ]  4wk_mintemp


# chgo.airports.wide.imp ## Use the imputed version of daily weather data 
# (2 Chicago airports) since that version has no missing data

chi.weather.v2 <- data.frame(dummy=1,date=chgo.airports.wide.imp[,"date"])

chi.weather.v2$USW00014819_2wk_precip <- lagfun(chgo.airports.wide.imp$USW00014819_PRCP,14,fun=mean)
chi.weather.v2$USW00094846_2wk_precip <- lagfun(chgo.airports.wide.imp$USW00094846_PRCP,14,fun=mean)

chi.weather.v2$USW00014819_4wk_precip <- lagfun(chgo.airports.wide.imp$USW00014819_PRCP,28,fun=mean)
chi.weather.v2$USW00094846_4wk_precip <- lagfun(chgo.airports.wide.imp$USW00094846_PRCP,28,fun=mean)

chi.weather.v2$USW00014819_4wk_precip <- lagfun(chgo.airports.wide.imp$USW00014819_PRCP,90,fun=mean)
chi.weather.v2$USW00094846_4wk_precip <- lagfun(chgo.airports.wide.imp$USW00094846_PRCP,90,fun=mean)

chi.weather.v2$USW00014819_2wk_tavg <- lagfun(chgo.airports.wide.imp$USW00014819_tavg2,14,fun=mean)
chi.weather.v2$USW00094846_2wk_tavg <- lagfun(chgo.airports.wide.imp$USW00094846_tavg2,14,fun=mean)

chi.weather.v2$USW00014819_4wk_tavg <- lagfun(chgo.airports.wide.imp$USW00014819_tavg2,28,fun=mean)
chi.weather.v2$USW00094846_4wk_tavg <- lagfun(chgo.airports.wide.imp$USW00094846_tavg2,28,fun=mean)

chi.weather.v2$USW00014819_2wk_mintemp <- lagfun(chgo.airports.wide.imp$USW00014819_TMIN,14,fun=min)
chi.weather.v2$USW00094846_2wk_mintemp <- lagfun(chgo.airports.wide.imp$USW00094846_TMIN,14,fun=min)

chi.weather.v2$USW00014819_4wk_mintemp <- lagfun(chgo.airports.wide.imp$USW00014819_TMIN,28,fun=min)
chi.weather.v2$USW00094846_4wk_mintemp <- lagfun(chgo.airports.wide.imp$USW00094846_TMIN,28,fun=min)

chi.weather.v2 <- chi.weather.v2 %>% dplyr::select(-dummy)

# summary(chi.weather.v2)


################################################################################
# Rename columns using Rahul's naming conventions

chi.weather.v2 <- chi.weather.v2 %>% 
  setNames(tolower(paste('wea'
    ,gsub('USW00094846','ohare',gsub("USW00014819","midway",names(.)))
    ,sep='_'
    ))) %>% 
  rename(t_date = wea_date)


################################################################################
# Subset to dates used in trap result dataset

chi.weather.v3 <- semi_join(chi.weather.v2 
                            ,df %>% group_by(t_date) %>% summarise(n=n())
                            ,by="t_date")

################################################################################
# Append to trap result data

df_rev <- left_join(df_rev,chi.weather.v2,by="t_date")

# colnames(df2[!colnames(df2) %in% colnames(df)])


################################################################################
# Save RData and CSV

chi_weather_lagged <- chi.weather.v2

save(chi_weather_lagged, file="chi_weather_lagged.RData", compress = FALSE)
write.csv(chi_weather_lagged,paste("D:\\ajc188\\github\\capstone_project","data\\processed","chi_weather_lagged.csv",sep='\\'),row.names=FALSE)

save(df_rev, file="df_rev.RData", compress = FALSE)
write.csv(df_rev,paste("D:\\ajc188\\github\\capstone_project","data\\processed","df_rev.csv",sep='\\'),row.names=FALSE)







