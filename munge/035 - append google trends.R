
################################################################################
## Read in CSVs

base.path <- "D:\\ajc188\\github\\capstone_project"
plot.path <- paste("D:\\ajc188\\github\\capstone_project","images",sep='\\')

trend.dead.birds <- read.csv(paste(base.path,'data','raw'
                                   ,'google_searches'
                                   ,'multiTimeline - dead birds - Chicago IL.csv',sep='\\')
         ,stringsAsFactors = FALSE)
trend.mosquito.bites <- read.csv(paste(base.path,'data','raw'
                                       ,'google_searches'
                                       ,'multiTimeline - mosquito bites - Chicago IL.csv',sep='\\')
                                 ,stringsAsFactors = FALSE)
trend.symptoms <- read.csv(paste(base.path,'data','raw'
                                       ,'google_searches'
                                       ,'multiTimeline - symptoms of West Nile - Chicago IL.csv',sep='\\')
                                 ,stringsAsFactors = FALSE)
trend.wn <- read.csv(paste(base.path,'data','raw'
                                       ,'google_searches'
                                       ,'multiTimeline - west nile - Chicago IL.csv',sep='\\')
                                 ,stringsAsFactors = FALSE)



################################################################################
## Make data frames for each trend dataset

make.df <- function(named.vector) {
  df.out <- data.frame(
    search.term = as.character(named.vector[1,])
    ,month = as.Date(paste(
      rownames(named.vector)[-1],'01',sep='-'),format='%Y-%m-%d')
    ,relative.interest = as.numeric(gsub('<1','0.5',named.vector[,1])[-1])
  )
  return(df.out)
}


wnv.trends.list <- lapply(list(trend.dead.birds,trend.mosquito.bites,trend.symptoms,trend.wn)
                          ,make.df)
wnv.trends <- do.call("rbind",wnv.trends.list)
wnv.trends$label =  tolower(gsub(' ','_',gsub(': (Chicago IL)','',wnv.trends$search.term, fixed=TRUE)))

wnv.trends.wide <- wnv.trends %>% 
  dplyr::select(-search.term) %>%
  mutate(yr = lubridate::year(month),mo = lubridate::month(month)) %>%
  # gather(variable,value,-c(label,month)) %>%
  # unite(label.value, label, variable) %>%
  spread(label,relative.interest) %>%
  setNames(tolower(paste('goog'
                         ,names(.)
                         ,sep='_'
  ))) %>% 
  rename(month = goog_month,yr = goog_yr, mo = goog_mo) %>%
  arrange(month)

wnv.trends.ts <- ts(wnv.trends.wide[,-(1:3)]
                    ,freq=12
                    ,start=min(wnv.trends.wide$yr))

str(wnv.trends.ts)

ts.plot(wnv.trends.ts)
plot(wnv.trends.ts)

# ?ts


################################################################################
## Append to data frame

# str(df_rev)

df_rev <- left_join(df_rev
                    ,wnv.trends.wide[,-1]
                    ,by=c("t_yr"="yr","t_mo"="mo"))

# df_rev$goog_dead_birds

# summary(df_rev %>% dplyr::select(matches('goog')))

# kdepairs(df_rev %>% dplyr::select(matches('goog')))


################################################################################
## Save data

save(df_rev, file="df_rev.RData", compress = FALSE)
write.csv(df_rev,paste("D:\\ajc188\\github\\capstone_project","data\\processed","df_rev.csv",sep='\\'),row.names=FALSE)




