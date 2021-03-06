## ----message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------
library("tidyverse")
library("xts")
library("dygraphs")
library("janitor")

## ------------------------------------------------------------------------
kag_train <- read_csv("../data/raw/kaggle/train.csv") %>% 
    clean_names()
head(kag_train)

## ------------------------------------------------------------------------
mosq_by_date = kag_train %>% group_by(date,trap) %>% summarise(total_mosq=sum(num_mosquitos),total_wnv = sum(wnv_present)) %>% ungroup()

trap_metadata = kag_train %>% select(trap, latitude, longitude, address_accuracy) %>% distinct()

## ------------------------------------------------------------------------
head(mosq_by_date)

## ------------------------------------------------------------------------
head(trap_metadata)

## ------------------------------------------------------------------------
ohare = c(41.99,-87.933,662)
midway = c(41.786,-87.752,612)

## ------------------------------------------------------------------------
hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}

## ------------------------------------------------------------------------
trap_metadata %>% 
    mutate(ohare_dist = hav.dist(longitude,ohare[2],latitude,ohare[1]),
           midway_dist = hav.dist(longitude,midway[2],latitude,midway[1])) -> trap_metadata

## ------------------------------------------------------------------------
trap_counts = mosq_by_date %>% count(trap)
trap_metadata %>% 
    left_join(trap_counts) %>% 
    ggplot(aes(longitude,latitude,color=n,size=n),alpha=0.4)+
    geom_point()+
    coord_fixed(ratio = 1)

## ------------------------------------------------------------------------
weather_data <- read_csv("../data/raw/kaggle/weather.csv")
head(weather_data)

## ------------------------------------------------------------------------
date_vec = weather_data$Date
int_cols = weather_data %>% select_if(is_numeric) %>% select(-Date)
chr_cols = weather_data %>% select_if(is_character)

## ------------------------------------------------------------------------
replace_m_with_na <- function(x){
    x[x=="M"] = NA
    x[x=="-"] = NA
    x
}
replace_t_with_na <- function(x){
    x[x=="T"] = NA
    x
}

## ------------------------------------------------------------------------
int_cols = int_cols %>% purrr::map_df(~replace_m_with_na(.x)) %>% purrr::map_df(~replace_t_with_na(.x))
chr_cols = chr_cols %>% purrr::map_df(~replace_m_with_na(.x)) %>% purrr::map_df(~replace_t_with_na(.x))

## ------------------------------------------------------------------------
weather_data = tibble(date = date_vec) %>% bind_cols(int_cols, chr_cols)
head(weather_data)

## ------------------------------------------------------------------------
weather_data <- weather_data %>% 
    mutate(
        Sunset = lubridate::fast_strptime(as.character(Sunset),format = "%H%M",lt = FALSE),
        Sunrise = lubridate::fast_strptime(as.character(Sunrise),format = "%H%M",lt = FALSE),
        day_time = as.numeric(Sunset-Sunrise)
    ) %>% 
    select(-Sunset,-Sunrise)

## ------------------------------------------------------------------------
weather_data <- weather_data %>% select(-CodeSum)

## ------------------------------------------------------------------------
weather_data[,-1] <- weather_data[,-1] %>% purrr::map_df(~as.numeric(.x))
head(weather_data)

## ------------------------------------------------------------------------
library(missForest)
library(doParallel)
library(foreach)
#cl <- parallel::makeCluster(18)
registerDoParallel(cores = 15)
mfFit <- missForest(xmis = as.matrix(weather_data[,-1]),ntree = 150)

## ------------------------------------------------------------------------
weather_impute = mfFit$ximp
weather_impute = tibble(date = weather_data$date) %>% bind_cols(as_tibble(weather_impute))

## ------------------------------------------------------------------------
head(weather_impute)

## ------------------------------------------------------------------------
train_X = mosq_by_date %>% 
    left_join(trap_metadata) %>% 
    left_join(weather_impute) %>% 
    select(-latitude, -longitude) %>% 
    separate(date,c("year","month","date")) %>% 
    mutate(SnowFall = round(SnowFall,2)) %>% 
    mutate_at(c("year","month","date"),as.numeric) %>% 
    mutate(trap=as.factor(trap))

## ------------------------------------------------------------------------
head(train_X)

## ------------------------------------------------------------------------
dim(train_X)

## ------------------------------------------------------------------------
library(randomForest)
?randomForest
randomForest(x=train_X[,c(-4,-6)],y = train_X[[6]],ntree = 150)

