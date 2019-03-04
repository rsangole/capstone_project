
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


require("stringr")


# https://github.com/cttobin/ggthemr


# install.packages("ggthemr")
# install.packages("https://github.com/cttobin/ggthemr")
# install.packages("devtools"); require("devtools")
devtools::install_github("cttobin/ggthemr")
require("ggthemr")


install.packages("janitor")
# require("janitor")



################################################################################
## Functions
################################################################################








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
              ,latlng.cnt = n_distinct(paste(lng,lat,collapse=','))
              # ,train.n = sum(train)
              # ,train.pct = round(100*sum(train)/n(),1)
              # ,validate.n = sum(validate)
              # ,validate.pct = round(100*sum(validate)/n(),1)
              # ,test.n = sum(!train)
              # ,test.pct = round(100*sum(!train)/n(),1)
              ,wnv.present.n = sum(WnvPresent == TRUE,na.rm=TRUE)
              ,wnv.present.pct = round(100*sum(WnvPresent == TRUE,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.absent.n = sum(WnvPresent == FALSE,na.rm=TRUE)
              ,wnv.absent.pct = round(100*sum(WnvPresent == FALSE,na.rm=TRUE)/sum(!is.na(WnvPresent)),1)
              ,wnv.na.n = sum(is.na(WnvPresent))
              ,wnv.na.pct = round(100*sum(is.na(any.WnvPresent))/n(),1)
              ,fst.date = min(date)
              ,lst.date = max(date)
              ,tot.Mosquitos = sum(NumMosquitos) 
              ,mean.Mosquitos = mean(NumMosquitos) 
              ,sd.Mosquitos = sd(NumMosquitos) 
              ,seqn = row_number()
    )
  
  return(inner_join(df3,df2,by=cols))
}




wnv.trap.date.aggr <- function(cols,df=wnv.trap.date.rev3b,limit.vars = 150) {
  
  # Add an id variable if it doesn't exist on the data.frame  
  # if(!("id" %in% colnames(df))) {
  #   df$id <- 1:dim(df)[1]
  # }
  
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
              ,latlng.cnt = n_distinct(paste(lng,lat,collapse=','))
              # ,train.n = sum(train)
              # ,train.pct = round(100*sum(train)/n(),1)
              # ,validate.n = sum(validate)
              # ,validate.pct = round(100*sum(validate)/n(),1)
              # ,test.n = sum(!train)
              # ,test.pct = round(100*sum(!train)/n(),1)
              ,wnv.present.n = sum(any.WnvPresent == TRUE,na.rm=TRUE)
              ,wnv.present.pct = round(100*sum(any.WnvPresent == TRUE,na.rm=TRUE)/sum(!is.na(any.WnvPresent)),1)
              ,wnv.absent.n = sum(any.WnvPresent == FALSE,na.rm=TRUE)
              ,wnv.absent.pct = round(100*sum(any.WnvPresent == FALSE,na.rm=TRUE)/sum(!is.na(any.WnvPresent)),1)
              ,wnv.na.n = sum(is.na(any.WnvPresent))
              ,wnv.na.pct = round(100*sum(is.na(any.WnvPresent))/n(),1)
              ,fst.date = min(date)
              ,lst.date = max(date)
              ,tot.Mosquitos = sum(tot.NumMosquitos) 
              ,mean.Mosquitos = mean(tot.NumMosquitos) 
              ,sd.Mosquitos = sd(tot.NumMosquitos) 
              # ,seqn = row_number()
    )
  
  return(inner_join(df3,df2,by=cols))
}






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
              # ,latlng.cnt = n_distinct(paste(lng,lat,collapse=','))
              ,latlng.cnt = n_distinct(location)
              # ,train.n = sum(train)
              # ,train.pct = round(100*sum(train)/n(),1)
              # ,validate.n = sum(validate)
              # ,validate.pct = round(100*sum(validate)/n(),1)
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



