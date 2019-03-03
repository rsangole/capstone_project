


################################################################################
## Append more NOAA weather data from Local Climatological Data (LCD) datasets



################################################################################
## Load data

base.path <- "D:\\ajc188\\github\\capstone_project"
# weather.path <- paste(base.path,"data","raw",'chi_weather',sep='\\')
weather.path <- paste(base.path,"data","raw",'chi_weather','lcd (multiple stations)',sep='\\')

wea.files <- list.files(path = weather.path)           

lcd.files <- wea.files[ regexpr('163[0-9a-zA-Z\\.]*\\.csv',wea.files) >0]

local.climatological.data <- 
  do.call("rbind",lapply(lcd.files,
               function(x) {
                 read.csv(paste(weather.path,x,sep="\\"),stringsAsFactors = FALSE)
               }))


################################################################################
## Initial restructuring, e.g. subset to daily values


dim(local.climatological.data) # 949245 x 124
str(local.climatological.data)
summary(local.climatological.data)
colnames(local.climatological.data)


## Look at structure of some of the variables

summary(local.climatological.data %>% dplyr::select(starts_with('Backup')))
summary(local.climatological.data %>% dplyr::select(starts_with('REPORT_T')) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(starts_with('SOURCE')) %>% mutate_all(as.factor))
ftable(local.climatological.data %>% dplyr::select(SOURCE,REPORT_TYPE) %>% mutate_all(as.factor))
ftable(local.climatological.data %>% dplyr::select(STATION,REPORT_TYPE) %>% mutate_all(as.factor))
ftable(local.climatological.data %>% dplyr::select(STATION,SOURCE) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(HeavyFog) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(starts_with('Normal')) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(starts_with('REM')) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(starts_with('DailyWeather')) %>% mutate_all(as.factor))
summary(local.climatological.data %>% dplyr::select(starts_with('STATION')) %>% mutate_all(as.factor))


# Limit to numeric daily variables & clean them up as needed

drop.suspect <- function(x) {
  return(ifelse(regexpr('s',x)<0,x,''))
}
replace.trace <- function(x) {
  return(ifelse(x=='T','0.001',x))
}

daily1 <- local.climatological.data %>% 
  dplyr::select(-starts_with("Hourly")) %>%
  dplyr::select(-starts_with("Monthly")) %>%
  dplyr::select(-starts_with("Backup")) %>%
  dplyr::select(-starts_with("ShortDuration")) %>%
  dplyr::select(-starts_with("Normals")) %>%
  dplyr::select(-c(REPORT_TYPE.1,SOURCE.1)) %>%
  dplyr::select(-c(REPORT_TYPE,SOURCE)) %>%
  dplyr::select(-c(WindEquipmentChangeDate,REM,DailyWeather)) %>%
  mutate(date = as.Date(DATE)) %>%
  dplyr::select(-DATE) %>%
  arrange(date,STATION) %>%
  group_by(date,STATION) %>%
  mutate_all(funs(drop.suspect(.))) %>%
  mutate_all(funs(replace.trace(.))) %>%
  mutate_all(funs(as.numeric(.))) %>%
  mutate(Sunrise = floor(Sunrise/100) + (Sunrise %% 100)/60
         ,Sunset = floor(Sunset/100) + (Sunset %% 100)/60)

# warnings()

dim(daily1) # 949245 x 31
summary(daily1)


daily2 <- daily1 %>% 
  ungroup() %>%
  mutate(nonmiss = rowSums(!is.na(dplyr::select(.,-c(date,STATION))))) %>%
  filter(nonmiss > 0) %>%
  arrange(date,STATION,-nonmiss) %>%
  group_by(date,STATION) %>%
  mutate(seqn=row_number()) 

# ftable(daily2$nonmiss)
# chk <-
# daily2 %>% group_by(STATION,date) %>% summarise(nrows=n()) %>% ungroup() %>% as.data.frame() %>% ftable(.$nrows)
# ftable(chk$nrows)
# chk <- chk[chk$nrows > 1,]
# chk2 <- inner_join(daily2,chk)

daily3 <- daily2 %>%
  dplyr::select(-c(nonmiss,seqn)) %>%
  mutate_at(vars(-one_of('date','STATION')),funs(max)) %>%
  distinct() %>% 
  setNames(ifelse(
    nchar(names(.)) < 5 | names(.) == 'STATION'
    ,tolower(names(.))
    ,gsub("^\\_"
                ,''
                ,gsub('[_]+','_'
                      ,gsub(' ','_'
                            ,tolower(paste(gsub('([[:upper:]])', ' \\1'
                                                ,gsub('Temperature','Temp'
                                                      ,gsub('Direction','Dir'
                                                            ,gsub('Speed','Spd'
                                                                  ,gsub('Minimum','min',
                                                                        gsub('Degree','Deg',
                                                                             gsub('Maximum','max',
                                                                                  gsub('Average','Avg',
                                                                                       gsub('Daily',''
                                                                                            ,names(.)
                                                                                            # ,gsub('([[:upper:]])', ' \\1', names(.))
                                                                                       )))))))))
                                                            ,sep='_'))))))) %>% 
# Remove columns that are NA for all observations
  .[, unlist(lapply(., function(x) !all(is.na(x))))] %>%
  mutate(station=as.factor(station)) %>% 
  mutate(station_abbrev=tolower(ifelse(
    regexpr('[a-zA-Z0-9]*14819',station) > 0,'Midway'
    ,ifelse(regexpr('[a-zA-Z0-9]*04807',station) > 0,'Gary'
            ,ifelse(regexpr('[a-zA-Z0-9]*94846',station) > 0,'OHare'
                    ,ifelse(regexpr('[a-zA-Z0-9]*04879',station) > 0,'Lansing'
                            ,ifelse(regexpr('[a-zA-Z0-9]*04838',station) > 0,'Palwaukee'
                                    ,'Unknown'
                            )))))))

dim(daily2) # 20200 x 33
dim(daily3) # 19740 x 23

summary(daily3)
ftable(daily3 %>% dplyr::select(starts_with('station')) %>% mutate_all(as.factor))


################################################################################
## Reshape long-to-wide

daily_wide <- daily3 %>%
  ungroup() %>% group_by(date,station_abbrev) %>%
  dplyr::select(-station) %>%
  gather(variable,value,-c(station_abbrev,t_date)) %>%
  unite(station.variable, station_abbrev, variable) %>%
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

daily_wide_chk %>% dplyr::select(matches('deg_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% kdepairs() + title('Heating/Cooling Degree Days - NOAA vs Weather Frontier')



# install.packages("ggcorrplot")
# remove.packages("ggcorrplot")
# devtools::install_github("taiyun/corrplot")
# ?ggcorrplot
# ??ggcorrplot
# ?corrplot


daily_wide_chk %>% dplyr::select(matches('deg_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digit=3) +
  ggtitle('Weather Frontier and NOAA heating/cooling degree days')  +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 

## Based on the above, there may be very minor differences between Weather
## Frontier and NOAA HDDs/CDDs, but they're close enough to omit the former
## version.
## There's also very little difference between O'Hare and Midway HDDs/CDDs, 
## especially HDDs, but we might still include both.  

ggthemr::ggthemr("fresh")
options(repr.plot.width=12, repr.plot.height=5)



################################################################################
## Look at correlations between weather stations using NOAA LCD data for 
## heating/cooling degree days


daily_wide %>% dplyr::select(matches('deg_days|hdds|cdds')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digit=3) +
  ggtitle('NOAA heating/cooling degree days')  +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"Heating and cooling degree days - corr.png",sep='\\'),width=10,height=8)



################################################################################
## Quick EDA for each station - CORRELATIONS

# summary(as.factor(daily3$station_abbrev))

daily_wide %>% dplyr::select(matches('ohare')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_ohare_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA OHare local climatological data') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"LCD_corr_OHare.png",sep='\\'),width=10,height=8)

daily_wide %>% dplyr::select(matches('midway')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_midway_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Midway local climatological data') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"LCD_corr_Midway.png",sep='\\'),width=10,height=8)

daily_wide %>% dplyr::select(matches('palwaukee')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_palwaukee_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Palwaukee local climatological data') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"LCD_corr_Palwaukee.png",sep='\\'),width=10,height=8)

daily_wide %>% dplyr::select(matches('gary')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_gary_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Gary local climatological data') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"LCD_corr_Gary.png",sep='\\'),width=10,height=8)

daily_wide %>% dplyr::select(matches('lansing')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_lansing_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('NOAA Lansing local climatological data') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave(paste(plot.path,'Weather EDA',"LCD_corr_Lansing.png",sep='\\'),width=10,height=8)


# Lansing and Gary are worthless.  Nothing but sunrise/sunset.  


################################################################################
## Quick EDA for each station - TIME SERIES PLOTS

daily_wide %>% 
  dplyr::select(matches('ohare|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_ohare_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA OHare local climatological data - time series') 
ggsave(paste(plot.path,'Weather EDA',"LCD_timeseries_OHare.png",sep='\\'),width=12,height=8)


daily_wide %>% 
  dplyr::select(matches('midway|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_midway_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Midway local climatological data - time series') 
ggsave(paste(plot.path,'Weather EDA',"LCD_timeseries_Midway.png",sep='\\'),width=12,height=8)



daily_wide %>% 
  dplyr::select(matches('palwaukee|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_palwaukee_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Palwaukee local climatological data - time series') 
ggsave(paste(plot.path,'Weather EDA',"LCD_timeseries_Palwaukee.png",sep='\\'),width=12,height=8)



daily_wide %>% 
  dplyr::select(matches('lansing|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_lansing_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Lansing local climatological data - time series') 
ggsave(paste(plot.path,'Weather EDA',"LCD_timeseries_Lansing.png",sep='\\'),width=12,height=8)



daily_wide %>% 
  dplyr::select(matches('gary|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_gary_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('NOAA Gary local climatological data - time series') 
ggsave(paste(plot.path,'Weather EDA',"LCD_timeseries_Gary.png",sep='\\'),width=12,height=8)







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


daily_wide2 <-  
  daily_wide[,colSums(!is.na(daily_wide))/dim(daily_wide)[1] >= 0.95]
## this reduced us from 57 variables to 48


# daily_wide3 <- daily_wide2 %>%
#   cbind(.
#         ,daily_wide2 %>% dplyr::select(-t_date) %>% 
#           mutate_all(funs(Hmisc::Lag(.,shift=1))) %>%
#           setNames(paste(colnames(.),'prev1',sep='_'))  
#   ) %>%
#   cbind(.
#         ,daily_wide2 %>% dplyr::select(-t_date) %>% 
#           mutate_all(funs(Hmisc::Lag(.,shift=-1))) %>%
#           setNames(paste(colnames(.),'next1',sep='_'))  
#   ) 
  
# daily_wide2 %>% dplyr::select(contains('sunrise'))




################################################################################
## Now combine with our other set of daily weather data

daily_wide3 <- 
  inner_join(daily_wide2,
             wea_daily_wide3b %>% 
               mutate(t_date=as.Date(date2)) %>% dplyr::select(-date2) %>%
               dplyr::select(starts_with('t_'),starts_with('ohare'),starts_with('midway'))
             )

# colnames(daily_wide2)
# colnames(wea_daily_wide3b)
# colnames(wea_daily_wide3)
# colnames(daily_wide3)

dim(daily_wide3)




################################################################################
## EDA (again)

daily_wide3 %>% 
  dplyr::select(matches('ohare|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_ohare_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "3 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('Weather (LCD/Daily) - time series - OHare') 
ggsave(paste(plot.path,'Weather EDA',"Weather_timeseries_OHare.png",sep='\\'),width=18,height=18)



daily_wide3 %>% dplyr::select(matches('ohare')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('ohare_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('OHare weather (LCD/Daily) correlations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations OHare.png",sep='\\'),width=18,height=18)

# summary(daily_wide3)


daily_wide3 %>% 
  dplyr::select(matches('midway|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_midway_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "3 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('Weather (LCD/Daily) - time series - Midway') 
ggsave(paste(plot.path,'Weather EDA',"Weather_timeseries_Midway.png",sep='\\'),width=18,height=18)



daily_wide3 %>% dplyr::select(matches('midway')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('midway_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Midway weather (LCD/Daily) correlations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations Midway.png",sep='\\'),width=18,height=18)


daily_wide3 %>% 
  dplyr::select(matches('palwaukee|t_date')) %>% 
  dplyr::filter(lubridate::year(t_date) >= 2007 & lubridate::year(t_date) <= 2016) %>%
  setNames(gsub('wea_palwaukee_','',names(.))) %>% 
  gather(.,attribute, value, -t_date) %>% 
  ggplot(aes(x=t_date,y=value,group=attribute),ggtheme = "fresh") +  
  geom_line() + 
  scale_x_date(labels = date_format("%Y"),date_breaks = "3 years", date_labels = "%y") + 
  facet_wrap(~attribute,ncol=5, scales = "free") +
  ggtitle('Weather (LCD/Daily) - time series - Palwaukee') 
ggsave(paste(plot.path,'Weather EDA',"Weather_timeseries_Palwaukee.png",sep='\\'),width=18,height=18)



daily_wide3 %>% dplyr::select(matches('palwaukee')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('palwaukee_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Palwaukee weather (LCD/Daily) correlations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations Palwaukee.png",sep='\\'),width=18,height=18)

daily_wide4 <- daily_wide3 %>% 
  dplyr::select(-matches('sunrise|sunset')) %>% 
  dplyr::select(-ends_with('AWND')) %>% 
  dplyr::select(-ends_with('WSF2')) %>% 
  dplyr::select(-ends_with('WSF5')) %>% 
  dplyr::select(-ends_with('SNOW')) %>% 
  dplyr::select(-ends_with('SNWD')) %>% 
  dplyr::select(-ends_with('TMIN')) %>% 
  dplyr::select(-ends_with('TMAX')) %>% 
  dplyr::select(-ends_with('PRCP')) %>% 
  dplyr::select(-ends_with('WDF2')) %>% 
  dplyr::select(-ends_with('WDF5')) 
  

daily_wide4 %>% dplyr::select(matches('ohare')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('ohare_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('OHare weather (LCD/Daily) correlations condensed') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations OHare - condensed.png",sep='\\'),width=18,height=18)

daily_wide4 %>% dplyr::select(matches('midway')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('midway_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Midway weather (LCD/Daily) correlations condensed') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations Midway - condensed.png",sep='\\'),width=18,height=18)


colnames(daily_wide4)
str(daily_wide4)
summary(daily_wide4)



daily_wide4 %>% 
  dplyr::select(matches('wind')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations - wind') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations wind.png",sep='\\'),width=18,height=18)



daily_wide4 %>% 
  dplyr::select(matches('precip'),matches('snow')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations - precipitation') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations precipitation.png",sep='\\'),width=18,height=18)



daily_wide4 %>% 
  dplyr::select(matches('cool'),matches('heat'),matches('temp')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations - temperature') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations temperature.png",sep='\\'),width=18,height=18)



daily_wide4 %>% 
  dplyr::select(matches('press')) %>%
  filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations - pressure') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations pressure.png",sep='\\'),width=18,height=18)




daily_wide5 <- daily_wide4 %>% 
  dplyr::select(-matches('palwaukee')) %>% 
  dplyr::select(-matches('snowfall')) %>% 
  setNames(paste('wea',names(.),sep='_')) %>%
  setNames(gsub('wea_wea_','wea_',names(.))) %>%
  rename(t_date = wea_t_date)
  
dim(daily_wide5) #4747 x 35
colnames(daily_wide5)

# 35*16=560

daily_wide5 %>% 
  dplyr::select(-t_date) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations reduced.png",sep='\\'),width=18,height=18)


# Try correlations in our moquito season to see if they look different
key.period <- wnv.trap.date.rev3b %>% ungroup() %>%
  summarise(min.day=min(day.of.yr),max.day=max(day.of.yr))

daily_wide5 %>% 
  dplyr::filter(lubridate::yday(t_date) >= key.period$min.day & 
                  lubridate::yday(t_date) <= key.period$max.day ) %>% 
  dplyr::select(-t_date) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations during mosquito season') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations during mosquito season.png",sep='\\'),width=18,height=18)
## Not a lot of obvious differences here.



daily_wide6 <- daily_wide5 %>%
  left_join(wea_daily_wide3b %>% 
              mutate(t_date=as.Date(date2)) %>%
              dplyr::select(t_date,matches('PRCP')) %>%
              setNames(paste('wea',names(.),sep='_')) %>%
              setNames(gsub('PRCP','precipitation',names(.))) %>%
              .[,!colnames(.) %in% colnames(daily_wide5)] %>%
              rename(t_date=wea_t_date) 
  )


daily_wide6 %>% 
  dplyr::select(-t_date) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations - final variables.png",sep='\\'),width=18,height=18)


# plot(daily_wide6 %>% dplyr::select(matches('avg_station_pressure')))

summary(daily_wide6)



################################################################################
## Impute missing data
## This code is very clunky but should work.

require('mice')

daily_wide6_imp0 <- mice(daily_wide6, m=5, maxit = 10, method = 'pmm', seed = 500)

get_means <- function(x,imp.obj) {
  imputations <- imp.obj$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values.daily_wide <- function(x,imp.obj) {
  means <- get_means(x,imp.obj)
  daily_wide6_imp[as.integer(names(means)),x] <- means
  return(daily_wide6_imp)
}

daily_wide6_imp <- daily_wide6

for(col in names(daily_wide6_imp0$imp)) {
  if(dim(daily_wide6_imp0$imp[col][[1]])[1] > 0 ) {
    daily_wide6_imp <- update.values.daily_wide(col,daily_wide6_imp0)
  }
}

summary(daily_wide6)
summary(daily_wide6_imp)


################################################################################
## Make sure we have no missing data

chk.missing <- colSums(is.na(daily_wide6_imp)) / dim(daily_wide6_imp)[1]
summary(chk.missing)
colnames(daily_wide6_imp[,chk.missing>0])

plot(daily_wide6_imp %>% dplyr::select(wea_midway_avg_dry_bulb_temp,wea_midway_tavg2))
plot(daily_wide6_imp %>% dplyr::select(wea_ohare_avg_dry_bulb_temp,wea_ohare_tavg2))

daily_wide6_imp.miss <- daily_wide6_imp[,colnames(daily_wide6_imp[,chk.missing>0])]



daily_wide6_imp$wea_midway_tavg2 <- 
  coalesce(
    daily_wide6_imp$wea_midway_tavg2
    ,daily_wide6_imp$wea_midway_avg_dry_bulb_temp
    ,(daily_wide6_imp$wea_midway_min_dry_bulb_temp + 
        daily_wide6_imp$wea_midway_max_dry_bulb_temp)/2
  )

daily_wide6_imp$wea_midway_avg_dry_bulb_temp <- 
  coalesce(
    daily_wide6_imp$wea_midway_avg_dry_bulb_temp
   ,(daily_wide6_imp$wea_midway_min_dry_bulb_temp + 
        daily_wide6_imp$wea_midway_max_dry_bulb_temp)/2
   ,daily_wide6_imp$wea_midway_tavg2
  )

summary(daily_wide6_imp)


################################################################################
## Repeat quick EDA

daily_wide6_imp %>% 
  dplyr::select(-t_date) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations with imputation') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(plot.path,'Weather EDA',"Weather correlations - final variables with imputation.png",sep='\\'),width=18,height=18)





################################################################################
## Create moving averages and sums
## In this case, make some subjective decisions about which vars to drop.

## Probably change my approach.  Calculate 7-day moving average for key 
## variables (or PCA results from groups of related variables). 
## Then create lag terms for moving average (0 weeks, 1, 2, 3, ..., 12 weeks
## maybe out to 16 weeks).  This could result in 16xTBD variables.  

make.mmax <- function(x,y=(0:6)) {
  return(apply(sapply(y,FUN=function(y) {dplyr::lag(x,y)}),1,max))
}
make.mmin <- function(x,y=(0:6)) {
  return(apply(sapply(y,FUN=function(y) {dplyr::lag(x,y)}),1,min))
}
make.ma <- function(x,y=(0:6)) {
  return(rowMeans(sapply(y,FUN=function(y) {dplyr::lag(x,y)})))
}
make.lag <- function(x,y=(0:6)) {
  return(sapply(y,FUN=function(y) {dplyr::lag(x,y)}))
}

# chk.x <- 1:100
# make.lag(chk.x)
# sapply(1:7,FUN=function(y) {dplyr::lag(chk.x,y)})
# rowMeans(sapply(0:6,FUN=function(y) {dplyr::lag(chk.x,y)}))

# chi_daily_wide_imp_ma <- chi_daily_wide_imp %>%
#   mutate_all(funs(ma=make.ma),y=0:6) %>%
#   dplyr::select(-t_date_ma)

# colnames(daily_wide6_imp)
# colnames(daily_wide6_imp %>% dplyr::select(matches('[a-zA-Z\\_]*min|max[a-zA-Z\\_]*temp')))
# colnames(daily_wide6_imp %>% dplyr::select(matches('[a-zA-Z\\_]*min[a-zA-Z\\_]*temp')))
# colnames(daily_wide6_imp %>% dplyr::select(matches('[a-zA-Z\\_]*max[a-zA-Z\\_]*temp')))
# colnames(daily_wide6_imp %>% dplyr::select(matches('[a-zA-Z\\_]*min[a-zA-Z\\_]')))
# colnames(daily_wide6_imp %>% dplyr::select(matches('[a-zA-Z\\_]*max[a-zA-Z\\_]')))
# colnames(daily_wide6_imp %>% dplyr::select(-one_of('t_date'),-matches('[a-zA-Z\\_]*min|max[a-zA-Z\\_]')))


# daily_wide6_imp_ma <- daily_wide6_imp %>%
#   mutate_at(vars(-one_of('t_date'),matches()),funs(ma7=make.ma),y=0:6) 
# 
# daily_wide6_imp_ma2 <- daily_wide6_imp_ma %>%
#   mutate_at(vars(-one_of('t_date')),funs(max7=make.mmax),y=0:6) 
# 
# # chi_daily_wide_imp_ma_lag %>% dplyr::select(contains('wea_ohare_sunset'))
# 
# daily_wide6_imp_ma2 <- daily_wide6_imp_ma %>%
#   mutate_at(vars(ends_with('temp')),funs(max7=make.mmax),y=0:6)
# 
# daily_wide6_imp_ma2 <- daily_wide6_imp_ma %>%
#   mutate_at(vars(ends_with('min_dry_bulb_temp')),funs(max7=make.mmin),y=0:6)

daily_wide6_imp_ma <- daily_wide6_imp %>%
  mutate_at(vars(-one_of('t_date'),-matches('[a-zA-Z\\_]*min|max[a-zA-Z\\_]')),funs(ma7=make.ma),y=0:6) %>%
  mutate_at(vars(matches('[a-zA-Z\\_]*min[a-zA-Z\\_]')),funs(min7=make.mmin),y=0:6) %>%
  mutate_at(vars(matches('[a-zA-Z\\_]*max[a-zA-Z\\_]')),funs(max7=make.mmax),y=0:6)



# chk <- daily_wide6_imp_ma2 %>% dplyr::select(t_date,starts_with('wea_ohare_min_dry_bulb_temp'))
# colnames(daily_wide6_imp_ma2)
colnames(daily_wide6_imp_ma)

# dim(daily_wide6_imp_ma2)


################################################################################
## Create lag terms
## Take 7-week moving average and lag it 1 to 16 weeks.  


daily_wide6_imp_ma_lag <- daily_wide6_imp_ma %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag1wk=make.lag),y=7*(1)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag2wk=make.lag),y=7*(2)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag3wk=make.lag),y=7*(3)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag4wk=make.lag),y=7*(4)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag5wk=make.lag),y=7*(5)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag6wk=make.lag),y=7*(6)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag7wk=make.lag),y=7*(7)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag8wk=make.lag),y=7*(8)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag9wk=make.lag),y=7*(9)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag10wk=make.lag),y=7*(10)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag11wk=make.lag),y=7*(11)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag12wk=make.lag),y=7*(12)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag13wk=make.lag),y=7*(13)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag14wk=make.lag),y=7*(14)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag15wk=make.lag),y=7*(15)) %>%
  mutate_at(vars(one_of(colnames(dplyr::select(.,ends_with('_ma7'),ends_with('_max7'),ends_with('_min7'))))),funs(lag16wk=make.lag),y=7*(16)) 

dim(daily_wide6_imp_ma_lag) ## 4747 x 739

# colnames(daily_wide6_imp_ma_lag %>% dplyr::select())


## Are we sure we want 7-day moving averaged lagged out to 4 months? 
## Why not 14-day?  Or 3-day?  Or 30-day? 
## There's no practical difference between averages and sums, but
## why not max temps instead of average?  
## And should we do this for all variables -- or just for key subsets?
## And shouldn't we pull nearest precipitation data in, too?  But can we 
## even do that?  It would have to be done one station at a time.  Sigh.  


################################################################################
## Quick EDA on data with moving averages, min, max, lag terms

daily_wide6_imp_ma_lag %>% 
  dplyr::select(-t_date) %>%
  dplyr::select(matches('ohare')) %>%
  dplyr::select(matches('avg|days')) %>%
  dplyr::select(-matches('wind_dir')) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations with imputation/moving avg/min/max/lag terms') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste(plot.path,'Weather EDA',"Weather correlations - with lag terms - subset example.png",sep='\\'),width=30,height=30)




################################################################################
## PCA

# daily_wide6_imp_ma_lag[366:370,1]

# Double check that we have no NAs
for.pca <- daily_wide6_imp_ma_lag[-(1:365),-(1)]
for.pca.missing <- colSums(is.na(for.pca)) / dim(for.pca)[1]
summary(for.pca.missing)
colnames(for.pca[,for.pca.missing>0])

# plot(daily_wide6_imp %>% dplyr::select(wea_midway_avg_dry_bulb_temp,wea_midway_tavg2))


wea_pca_rev <- prcomp(daily_wide6_imp_ma_lag[-(1:365),-(1)], scale = TRUE)
summary(wea_pca_rev)
biplot(wea_pca_rev , scale = 0)

wea_pca_rev_var <- wea_pca_rev$sdev ^2
wea_pca_rev_var_explained <- wea_pca_rev_var / sum(wea_pca_rev_var)
wea_pca_rev_var_explained_cum <- cumsum(wea_pca_rev_var_explained)
pca_var_df <- data.frame(wea_pca_rev_var,wea_pca_rev_var_explained,wea_pca_rev_var_explained_cum)

plot(wea_pca_rev_var_explained 
     , xlab="Principal Component"
     , ylab=" Proportion of Variance Explained "
     , ylim=c(0,1) 
     ,type="b")
plot(wea_pca_rev_var_explained_cum
     , xlab="Principal Component"
     , ylab ="Cumulative Proportion of Variance Explained"
     , ylim=c(0,1) 
     , type="b")
## 303 components explain 99% of variance

wea_wide_pca <- cbind(t_date=daily_wide6_imp_ma_lag[-(1:365),1]
                      ,as.data.frame(wea_pca_rev$x)) %>%
  mutate(t_date = as.Date(t_date))

save(wea_wide_pca
     ,file=paste(weather.path,"wea_wide_pca.RData",sep='\\')
     , compress = FALSE)


wea_wide <- daily_wide6_imp_ma_lag

save(wea_wide
     ,file=paste(weather.path,"wea_wide.RData",sep='\\')
     , compress = FALSE)



# I'm disinclined to use the pca_rev version because it makes it much more 
# confusing to work with moving averages and lag terms, but we certainly could.


wea_wide_pca[,(2:50)] %>% 
  cor(.,use = "pairwise.complete.obs") %>%
  as.data.frame(.) %>% 
  mutate_all(funs(coalesce),0) %>%
  .[!(colSums(.) == 0 & rowSums(.) == 0),!(colSums(.) == 0 & rowSums(.) == 0)] %>%
  `rownames<-`(names(.)) %>% 
  as.matrix(.) %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=3) +
  ggtitle('Weather correlations - principal components') +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave(paste(plot.path,'Weather EDA',"Weather correlations - PCA version.png",sep='\\'),width=30,height=30)
# Good.  There are no correlations between these variables after running PCA.





################################################################################
## One more version
## Run PCA on weather variables first (fixed to a given date) then apply 
## moving averages and lag terms to the results




################################################################################
## Add them to data.frame

df_results_wea <- df_results %>% dplyr::select(-matches('wea_')) %>% inner_join(daily_wide6_imp_ma_lag,by=c("t_date"))
df_results_wea_pca <- df_results %>% dplyr::select(-matches('wea_')) %>% inner_join(wea_wide_pca,by=c("t_date"))

dim(df_results)
dim(df_results_wea)
dim(df_results_wea_pca)

save(df_results_wea
     ,file=paste(base.path,'data','processed',"df_results_wea.RData",sep='\\')
     , compress = FALSE)
save(df_results_wea_pca
     ,file=paste(base.path,'data','processed',"df_results_wea_pca.RData",sep='\\')
     , compress = FALSE)





################################################################################
## Old code related to a different version of PCA

chi_daily_wide_imp_pca_rev2 <- chi_daily_wide_imp %>%
  inner_join(chi_daily_wide_imp_pca_rev)

chi_daily_wide_imp_pca_rev2 %>% dplyr::select(matches('ohare|PC')) %>%
  setNames(gsub('wea_ohare_','',names(.))) %>%
  cor() %>%
  as.data.frame() %>%
  .[colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC'))
             %>% setNames(gsub('wea_ohare_','',names(.))))
    ,colnames(chi_daily_wide_imp_pca_rev[,-1])] %>%
  mutate(raw = rownames(.)) %>%
  # dplyr::select(colnames(chi_daily_wide_imp_pca_rev[,-1])) %>%
  gather(.,pc, corr, -raw) %>% 
  ggplot(aes(raw, pc)) + 
  geom_tile(aes(fill = corr), colour = "white") + 
  geom_text(aes(label=sprintf("%0.2f", round(corr, digits = 2)),alpha=abs(corr))
            ,size=3, show_legend = F) +
  # scale_color_gradient(low="blue", high="red", mid="white") +  
  # scale_colour_distiller(palette="BlGrWh") + 
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0) +  
  xlim(as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC')) %>% setNames(gsub('wea_ohare_','',names(.)))) ) )) +
  ylim(as.character(rev(as.factor(colnames(chi_daily_wide_imp_pca_rev[,-1]))))) +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('NOAA OHare local climatological data - correlations between raw and pca_rev') 

ggsave(paste(plot.path,"OHare_LCD_imp_corr_with_pca_rev.png",sep='\\'),width=12,height=8)


chi_daily_wide_imp_pca_rev2 %>% dplyr::select(matches('midway|ohare|PC')) %>%
  cor() %>%
  as.data.frame() %>%
  .[colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('midway|ohare|PC')))
    ,colnames(chi_daily_wide_imp_pca_rev[,-1])] %>%
  mutate(raw = rownames(.)) %>%
  gather(.,pc, corr, -raw) %>% 
  ggplot(aes(raw, pc)) + 
  geom_tile(aes(fill = corr), colour = "white") + 
  geom_text(aes(label=sprintf("%0.2f", round(corr, digits = 2)),alpha=abs(corr))
            ,size=3, show_guide = F) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0) +  
  xlim(c(as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('ohare|PC')))))),
       as.character(as.factor(colnames(chi_daily_wide_imp[,-1] %>% dplyr::select(matches('midway|PC')) )))) +
  ylim(as.character(rev(as.factor(colnames(chi_daily_wide_imp_pca_rev[,-1]))))) +
  theme(axis.title.x  = element_blank(),axis.title.y = element_blank()
        ,axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('NOAA local climatological data - correlations between raw and pca_rev') 
ggsave(paste(plot.path,"LCD_imp_corr_with_pca_rev.png",sep='\\'),width=16,height=12)




chi_daily_wide_imp %>% dplyr::select(-t_date) %>%
  # filter(rowSums(is.na(.)) == 0) %>% 
  setNames(gsub('wea_','',names(.))) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE,ggtheme = "fresh",lab_size = 3,digits=2
                         # ,hc.order =TRUE
                         # ,tl.srt=0
  ) +
  ggtitle('NOAA local climatological data') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste(plot.path,"LCD_corr_imp.png",sep='\\'),width=14,height=14)

## I'm going to use this clustering to guide pca on subsets of variables.
## If we have 5 highly correlated variables from 2 stations, we might be 
## able to reduce this from 10 variables to 3-4 to capture most variability
## (including inter-station variability).  Repeat this for other groups
## of variables.  Doing so can probably reduce us from 36 variables to perhaps
## 10-15.  Once done, calculate moving averages and then lag them.  
## We'll still end up with 100-200 variables for modeling but I think it 
## could prove useful.  

## Might also add outcomes to these correlation plots to get an early look
## at that.  (This needs date-specific data merged to trap results.) 



