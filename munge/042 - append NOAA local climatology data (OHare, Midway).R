


################################################################################
## Append more NOAA weather data from Local Climatological Data (LCD) datasets



################################################################################
## Load data

base.path <- "D:\\ajc188\\github\\capstone_project"
weather.path <- paste(base.path,"data","raw",'chi_weather',sep='\\')

lcd.1 <- read.csv(paste(weather.path,"1617130_2007to10.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.2 <- read.csv(paste(weather.path,"1617146_2010to14.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.3 <- read.csv(paste(weather.path,"1617148_2015to19.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.4 <- read.csv(paste(weather.path,"1617149_2011to14.csv",sep="\\"),stringsAsFactors = FALSE)
lcd.5 <- read.csv(paste(weather.path,"1617195_2006.csv",sep="\\"),stringsAsFactors = FALSE)

local.climatological.data <- rbind(lcd.1,lcd.3,lcd.4,lcd.5)

rm(list=c('lcd.1','lcd.2','lcd.3','lcd.4','lcd.5'))


################################################################################
## Preliminary checks

summary(local.climatological.data)
# summary(as.factor(local.climatological.data$DAILYMaximumDryBulbTemp))



################################################################################
## Load data

# huh <- local.climatological.data$DAILYMaximumDryBulbTemp
# huh <- huh[huh != '']
# ftable(huh)
# Per documentation, when "s" is at end of value, it's suspect.  
# I might just delete those and impute.

# huh <- local.climatological.data$DAILYWeather
# huh <- huh[huh != '']
# summary(as.factor(huh))

# lcd.chars <- sapply(local.climatological.data,function (x) {regexpr('[a-zA-Z]',x)})
# str(lcd.chars)
# lcd.chars2 <- colSums(lcd.chars > 0)


# summary(as.factor(local.climatological.data[lcd.chars[,'HOURLYPrecip'] > 0,'HOURLYPrecip']))
# summary(as.factor(local.climatological.data[lcd.chars[,'HOURLYSKYCONDITIONS'] > 0,'HOURLYSKYCONDITIONS']))
# summary(as.factor(local.climatological.data[lcd.chars[,'REPORTTPYE'] > 0,'REPORTTPYE']))
# summary(as.factor(local.climatological.data[lcd.chars[,'HOURLYPRSENTWEATHERTYPE'] > 0,'HOURLYPRSENTWEATHERTYPE']))
# summary(as.factor(local.climatological.data[lcd.chars[,'HOURLYDRYBULBTEMPF'] > 0,'HOURLYDRYBULBTEMPF']))
# summary(as.factor(local.climatological.data[lcd.chars[,'HOURLYDRYBULBTEMPC'] > 0,'HOURLYDRYBULBTEMPC']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYMinimumDryBulbTemp'] > 0,'DAILYMinimumDryBulbTemp']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYAverageDryBulbTemp'] > 0,'DAILYAverageDryBulbTemp']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYDeptFromNormalAverageTemp'] > 0,'DAILYDeptFromNormalAverageTemp']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYHeatingDegreeDays'] > 0,'DAILYHeatingDegreeDays']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYCoolingDegreeDays'] > 0,'DAILYCoolingDegreeDays']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYWeather'] > 0,'DAILYWeather']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYPrecip'] > 0,'DAILYPrecip']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYSnowfall'] > 0,'DAILYSnowfall']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYSnowDepth'] > 0,'DAILYSnowDepth']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYPeakWindSpeed'] > 0,'DAILYPeakWindSpeed']))
# summary(as.factor(local.climatological.data[lcd.chars[,'PeakWindDirection'] > 0,'PeakWindDirection']))
# summary(as.factor(local.climatological.data[lcd.chars[,'MonthlyTotalSnowfall'] > 0,'MonthlyTotalSnowfall']))
# summary(as.factor(local.climatological.data[lcd.chars[,'DAILYPrecip'] > 0,'DAILYPrecip']))
# summary(as.factor(local.climatological.data[,'DAILYPrecip']))


drop.suspect <- function(x) {
  return(ifelse(regexpr('s',x)<0,x,''))
}
replace.trace <- function(x) {
  return(ifelse(x=='T','0.001',x))
}

daily1 <- local.climatological.data %>% 
  dplyr::select(-matches("HOURLY")) %>%
  dplyr::select(-matches("MONTHLY")) %>%
  mutate(date = as.Date(DATE)) %>%
  dplyr::select(-DATE,-DAILYWeather
                # ,-DAILYMaximumDryBulbTemp,-DAILYAverageDryBulbTemp
  ) %>%
  arrange(date,STATION_NAME) %>%
  group_by(date,STATION_NAME,STATION,LATITUDE,LONGITUDE,REPORTTPYE) %>%
  mutate_all(funs(drop.suspect(.))) %>%
  mutate_all(funs(replace.trace(.))) %>%
  mutate_all(funs(as.numeric(.))) 
# warnings()
# summary(daily1)

daily2 <- daily1 %>% 
  ungroup() %>%
  mutate(nonmiss = rowSums(!is.na(dplyr::select(., contains("DAILY"))))) %>%
# Daily sunrise and daily sunset are always nonmissing, so ignore them
  mutate(keep = (nonmiss - 2) > 0) %>%
  filter(keep) %>%
  arrange(date,STATION_NAME,-nonmiss) %>%
  group_by(date,STATION_NAME) %>%
  mutate(seqn=row_number())
# summary(daily2$nonmiss)
# summary(daily2$keep)
# summary(daily2)


# daily2 %>% group_by(STATION,STATION_NAME) %>% summarise(n=n(),n.dates=n_distinct(date))
# daily3 %>% group_by(STATION,STATION_NAME) %>% summarise(n=n(),n.dates=n_distinct(date))

daily3 <- daily2 %>% filter(seqn==1) %>%
  dplyr::select(-ELEVATION,-LATITUDE,-LONGITUDE,-REPORTTPYE) %>%
  rename(station = STATION, station_name = STATION_NAME) %>%
  setNames(gsub("^\\_"
                ,''
                ,gsub('[_]+','_'
                      ,gsub(' ','_'
                            ,tolower(paste(gsub('([[:upper:]])', ' \\1'
                                                ,gsub('Minimum','min',
                                                      gsub('Maximum','max',
                                                           gsub('Average','Avg',
                                                                gsub('DAILY',''
                                                                     ,names(.)
                                                                     # ,gsub('([[:upper:]])', ' \\1', names(.))
                                                                )))))
                                           ,sep='_')))))) %>%
  mutate(station_label = ifelse(station_name == 'CHICAGO MIDWAY AIRPORT IL US'
                                ,"midway"
                                ,ifelse(station_name == "CHICAGO OHARE INTERNATIONAL AIRPORT IL US"
                                        ,'ohare','other')))

daily_wide <- daily3 %>%
  ungroup() %>% group_by(date,station_label) %>%
  dplyr::select(-station,-station_name,-nonmiss,-keep,-seqn) %>%
  gather(variable,value,-c(station_label,t_date)) %>%
  unite(station.variable, station_label, variable) %>%
  spread(station.variable,value) %>%
  setNames(tolower(paste('wea'
                         ,names(.)
                         ,sep='_'
  ))) %>%
  rename(t_date = wea_date) 

# Remove columns that are completely missing for all observations
daily_wide <- daily_wide[, unlist(lapply(daily_wide, function(x) !all(is.na(x))))]

# summary(daily_wide)


################################################################################
##Check if this is the same as what I got from Frontier weather.all

daily_wide_chk <- daily_wide %>% left_join(.,wea_frontier,by="t_date")

daily_wide_chk %>% dplyr::select(matches('degree_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% kdepairs() + title('Heating/Cooling Degree Days - NOAA vs Weather Frontier')

## Based on the above, there may be very minor differences between Weather
## Frontier and NOAA HDDs/CDDs, but they're close enough to omit the former
## version.
## There's also very little difference between O'Hare and Midway HDDs/CDDs, 
## especially HDDs, but we might still include both.  

# ggthemr::ggthemr("fresh")
# options(repr.plot.width=12, repr.plot.height=5)
ggthemr::ggthemr("fresh")
options(repr.plot.width=12, repr.plot.height=5)

# daily_wide %>% dplyr::select(matches('degree_days|hdds|cdds')) %>%
#   filter(rowSums(is.na(.)) == 0) %>% kdepairs() + 
#   title('NOAA heating/cooling degree days') 

daily_wide_chk %>% dplyr::select(matches('degree_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digit=3) +
  ggtitle('Weather Frontier and NOAA heating/cooling degree days') 


# install.packages("ggcorrplot")
# remove.packages("ggcorrplot")
# devtools::install_github("taiyun/corrplot")
# ?ggcorrplot
# ??ggcorrplot
# ?corrplot

daily_wide %>% dplyr::select(matches('degree_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA heating/cooling degree days') 


daily_wide_chk %>% dplyr::select(matches('degree_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather Frontier and NOAA heating/cooling degree days') 

# colnames(daily_wide)


################################################################################
## Quick EDA

summary(daily_wide)

daily_wide %>% dplyr::select(matches('ohare')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_ohare_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA OHare local climatological data') 
ggsave(paste(plot.path,"OHare_LCD_corr.png",sep='\\'),width=10,height=8)

daily_wide %>% dplyr::select(matches('midway')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_midway_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Midway local climatological data') 
ggsave(paste(plot.path,"Midway_LCD_corr.png",sep='\\'),width=10,height=8)



################################################################################
## Before imputing, create some lag terms (prev, next values)

# daily_wide2 <- daily_wide %>% mutate_all(Hmisc::Lag(.,1)) %>% setNames(paste('lag1',names(.),sep='_'))

# calc.lag.col.names <- function(col
#                                ,lag=c(1)
#                                ,suffix="_lag") {
#   lag <- ifelse(lag<0,paste('p',abs(lag),sep=''),lag)
#   lagged.col <- paste(col,suffix,lag,sep='')
#   return(lagged.col) 
# }
# 
# # Create a set of lagged values (1 week prior, 2 weeks, 4, 13, 52)
# calc.lag.terms <- function(cols.to.lag
#                            ,lag.terms = c(1,2,4,13,52)
#                            ,df=dengue.eda) {
#   df.out <- NA
#   for(col in cols.to.lag) {
#     lag.col.values <- sapply(lag.terms,function(x) {Hmisc::Lag(df[,col],x)})
#     colnames(lag.col.values) <- calc.lag.col.names(col,lag=lag.terms)
#     df.out <- cbind(df.out,lag.col.values)
#   }
#   
#   return(as.data.frame(df.out[,!colnames(df.out) %in% "df.out"]))
# }
# 
# # install.packages("Hmisc")
# 
# daily_wide2 <- calc.lag.terms(
#   colnames(daily_wide %>% dplyr::select(-t_date))
#   ,lag.terms = c(1,-1,4)
#   ,df=daily_wide)
# 
# lag_values <- function(x,lag.term=1) {
#   Hmisc::Lag(x,shift=1)
# }
# 
# daily_wide %>% mutate_all(funs(Hmisc::Lag(.,shift=1))) %>% set
# 
# daily_wide %>% dplyr::select(-t_date) %>% 
#   mutate_all(funs(Hmisc::Lag(.,shift=-1))) %>%
#   setNames(paste('prev1',colnames(.),sep='_'))  

daily_wide2 <- daily_wide %>%
  cbind(.
        ,daily_wide %>% dplyr::select(-t_date) %>% 
          mutate_all(funs(Hmisc::Lag(.,shift=1))) %>%
          setNames(paste(colnames(.),'prev1',sep='_'))  
  ) %>%
  cbind(.
        ,daily_wide %>% dplyr::select(-t_date) %>% 
          mutate_all(funs(Hmisc::Lag(.,shift=-1))) %>%
          setNames(paste(colnames(.),'next1',sep='_'))  
  ) 
  
# daily_wide2 %>% dplyr::select(contains('sunrise'))




################################################################################
## Impute missing data
## This code is very clunky but should work.

require('mice')

daily_wide2_imp0 <- mice(daily_wide2, m=10, maxit = 10, method = 'pmm', seed = 500)

get_means <- function(x,imp.obj) {
  imputations <- imp.obj$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values.daily_wide <- function(x,imp.obj) {
  means <- get_means(x,imp.obj)
  daily_wide2_imp[as.integer(names(means)),x] <- means
  return(daily_wide2_imp)
}

daily_wide2_imp <- daily_wide2

for(col in names(daily_wide2_imp0$imp)) {
  if(dim(daily_wide2_imp0$imp[col][[1]])[1] > 0 ) {
    daily_wide2_imp <- update.values.daily_wide(col,daily_wide2_imp0)
  }
}

summary(daily_wide2)
summary(daily_wide2_imp)

chi_daily_wide_imp <- daily_wide2_imp %>% 
  dplyr::select(-matches('_prev1')) %>%
  dplyr::select(-matches('_next1'))

summary(daily_wide2)
summary(chi_daily_wide_imp)

summary(chi_daily_wide_imp[,-(1)])
still.missing <- chi_daily_wide_imp[rowSums(is.na(chi_daily_wide_imp))>0,]
# We're still missing data for sunrise/sunset at Midway for two time points. 
# Must be some explanation during imputation, but it's probably not important.
# Let's just drop these variables.

chi_daily_wide_imp <- chi_daily_wide_imp %>% 
  dplyr::select(-c(wea_midway_sunrise,wea_midway_sunset)) 

chi_daily_wide_imp <- chi_daily_wide_imp %>% 
  mutate(wea_ohare_sunrise = floor(wea_ohare_sunrise/100) + (wea_ohare_sunrise %% 100)/60
         ,wea_ohare_sunset = floor(wea_ohare_sunset/100) + (wea_ohare_sunset %% 100)/60)

save(daily_wide2_imp0
     ,file=paste(weather.path,"daily_wide2_imp0.RData",sep='\\')
     , compress = FALSE)
save(daily_wide2_imp
     ,file=paste(weather.path,"daily_wide2_imp.RData",sep='\\')
     , compress = FALSE)
save(chi_daily_wide_imp
     ,file=paste(weather.path,"chi_daily_wide_imp.RData",sep='\\')
     , compress = FALSE)


################################################################################
## Repeat quick EDA

summary(chi_daily_wide_imp)

chi_daily_wide_imp %>% dplyr::select(matches('ohare')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_ohare_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3
                         # ,tl.srt=0
                         ) +
  ggtitle('NOAA OHare local climatological data') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"OHare_LCD_corr_imp.png",sep='\\'),width=10,height=8)

chi_daily_wide_imp %>% dplyr::select(matches('midway')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_midway_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Midway local climatological data') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,"Midway_LCD_corr_imp.png",sep='\\'),width=10,height=8)



chi_daily_wide_imp %>% 
  dplyr::select(matches('ohare|t_date')) %>% 
  setNames(gsub('wea_ohare_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=value,group=attribute),ggtheme = "fresh") +  
  geom_histogram() + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA OHare local climatological data - histograms') 
ggsave(paste(plot.path,"OHare_LCD_hist_imp.png",sep='\\'),width=12,height=8)

chi_daily_wide_imp %>% 
  dplyr::select(matches('midway|t_date')) %>% 
  setNames(gsub('wea_midway_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=value,group=attribute),ggtheme = "fresh") +  
  geom_histogram() + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Midway local climatological data - histograms') 
ggsave(paste(plot.path,"Midway_LCD_hist_imp.png",sep='\\'),width=12,height=8)

# Should rescale sunrise/sunset.  Should think about which of these terms
# work.

chi_daily_wide_imp %>% 
  dplyr::select(matches('ohare|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_ohare_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA OHare local climatological data - time series') 
ggsave(paste(plot.path,"OHare_LCD_imp_timeseries.png",sep='\\'),width=12,height=8)

chi_daily_wide_imp %>% 
  dplyr::select(matches('midway|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_midway_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Midway local climatological data - time series') 
ggsave(paste(plot.path,"Midway_LCD_imp_timeseries.png",sep='\\'),width=12,height=8)



################################################################################
## PCA


wea_pca <- prcomp(chi_daily_wide_imp[,-(1)], scale = TRUE)
summary(wea_pca)
biplot(wea_pca , scale = 0)

wea_pca_var <- wea_pca$sdev ^2
wea_pca_var_explained <- wea_pca_var / sum(wea_pca_var)
wea_pca_var_explained_cum <- cumsum(wea_pca_var_explained)

plot(wea_pca_var_explained 
     , xlab="Principal Component"
     , ylab=" Proportion of Variance Explained "
     , ylim=c(0,1) 
     ,type="b")
plot(wea_pca_var_explained_cum
     , xlab="Principal Component"
     , ylab ="Cumulative Proportion of Variance Explained"
     , ylim=c(0,1) 
     , type="b")

wea_pca_x <- as.data.frame(cbind(t_date=chi_daily_wide_imp[,1],wea_pca$x)) %>%
  mutate(t_date = as.Date(t_date))
chi_daily_wide_imp_pca <- wea_pca_x

save(chi_daily_wide_imp_pca
     ,file=paste(weather.path,"chi_daily_wide_imp_pca.RData",sep='\\')
     , compress = FALSE)

# I'm disinclined to use the PCA version because it makes it much more 
# confusing to work with moving averages and lag terms, but we certainly could.


chi_daily_wide_imp_pca2 <- chi_daily_wide_imp %>%
  inner_join(chi_daily_wide_imp_pca)

chi_daily_wide_imp_pca2 %>% dplyr::select(matches('ohare|PC')) %>%
  setNames(gsub('wea_ohare_','',names(.))) %>%
  cor() %>%
  as.data.frame() %>%
  .[colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC'))
             %>% setNames(gsub('wea_ohare_','',names(.))))
    ,colnames(chi_daily_wide_imp_pca[,-1])] %>%
  mutate(raw = rownames(.)) %>%
  # dplyr::select(colnames(chi_daily_wide_imp_pca[,-1])) %>%
  gather(.,pc, corr, -raw) %>% 
  ggplot(aes(raw, pc)) + 
  geom_tile(aes(fill = corr), colour = "white") + 
  geom_text(aes(label=sprintf("%0.2f", round(corr, digits = 2)),alpha=abs(corr))
            ,size=3, show_legend = F) +
  # scale_color_gradient(low="blue", high="red", mid="white") +  
  # scale_colour_distiller(palette="BlGrWh") + 
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0) +  
  xlim(as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC')) %>% setNames(gsub('wea_ohare_','',names(.)))) ) )) +
  ylim(as.character(rev(as.factor(colnames(chi_daily_wide_imp_pca[,-1]))))) +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('NOAA OHare local climatological data - correlations between raw and PCA') 

ggsave(paste(plot.path,"OHare_LCD_imp_corr_with_pca.png",sep='\\'),width=12,height=8)


chi_daily_wide_imp_pca2 %>% dplyr::select(matches('midway|ohare|PC')) %>%
  cor() %>%
  as.data.frame() %>%
  .[colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('midway|ohare|PC')))
    ,colnames(chi_daily_wide_imp_pca[,-1])] %>%
  mutate(raw = rownames(.)) %>%
  gather(.,pc, corr, -raw) %>% 
  ggplot(aes(raw, pc)) + 
  geom_tile(aes(fill = corr), colour = "white") + 
  geom_text(aes(label=sprintf("%0.2f", round(corr, digits = 2)),alpha=abs(corr))
            ,size=3, show_guide = F) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0) +  
  xlim(c(as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC')))))),
       as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('midway|PC')) )))) +
  ylim(as.character(rev(as.factor(colnames(chi_daily_wide_imp_pca[,-1]))))) +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('NOAA local climatological data - correlations between raw and PCA') 
ggsave(paste(plot.path,"LCD_imp_corr_with_pca.png",sep='\\'),width=16,height=12)






################################################################################
## Create moving averages and sums
## In this case, make some subjective decisions about which vars to drop.

## Probably change my approach.  Calculate 7-day moving average for key 
## variables (or PCA results from groups of related variables). 
## Then create lag terms for moving average (0 weeks, 1, 2, 3, ..., 12 weeks
## maybe out to 16 weeks).  This could result in 16xTBD variables.  



################################################################################
## Create lag terms







