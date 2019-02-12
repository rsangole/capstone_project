
################################################################################
## Load shared CSV file 

base.path <- "D:\\ajc188\\github\\capstone_project"
plot.path <- paste("D:\\ajc188\\github\\capstone_project","images",sep='\\')


df <- read.csv(paste(base.path,'data','processed','wnv.trap.date.rev3b.csv',sep='\\')
                      ,stringsAsFactors = FALSE)

df_train <- df[df$part_train,]
df_validate <- df[df$part_validate,]
df_test <- df[df$part_test,]



################################################################################
## Reduce reference time data to weekly, monthly, yearly sets

eval_wks <- ref.dates %>% group_by(eval.wk) %>%
  summarise(yr=min(yr),mo=min(mo),wk.start = min(date),wk.end = max(date)
            ,train=as.logical(max(train))
            ,validate=as.logical(max(validate)),test=as.logical(max(test))) %>%
  setNames(tolower(gsub("\\.","_",names(.))) ) %>%
  setNames(paste('t',names(.),sep='_')) %>%
  mutate(t_eval_wk = as.integer(t_eval_wk))

eval_mos <- ref.dates %>% group_by(yr,mo) %>%
  summarise(wk.start = min(date),wk.end = max(date)
            ,train=max(train),validate=max(validate),test=max(test)) %>%
  setNames(tolower(gsub("\\.","_",names(.))) ) %>%
  setNames(paste('t',names(.),sep='_'))

eval_yrs <- ref.dates %>% group_by(yr) %>%
  summarise(wk.start = min(date),wk.end = max(date)
            ,train=max(train),validate=max(validate),test=max(test)) %>%
  setNames(tolower(gsub("\\.","_",names(.))) ) %>%
  setNames(paste('t',names(.),sep='_'))



################################################################################
## Create trap-level weekly version of trap data without imputation

# summary(df_train)

trap_wkly_long <- df_train %>% group_by(trap_trap_name,t_eval_wk) %>% 
  summarise(
    n_rows = n()
    ,n_obs = sum(!is.na(mos_any_wnv_present))
    ,mos_max_tot_num_mosquitos = max(mos_tot_num_mosquitos) 
    ,mos_mean_tot_num_mosquitos = mean(mos_tot_num_mosquitos)  
    ,mos_any_wnv_present = max(mos_any_wnv_present) 
  )

trap_wkly_wide <- trap_wkly_long  %>% 
  group_by(trap_trap_name,t_eval_wk) %>%
  dplyr::select(c('trap_trap_name','t_eval_wk','mos_mean_tot_num_mosquitos')) %>%
  dplyr::rename(m = mos_mean_tot_num_mosquitos) %>%
  gather(variable,value,-c(trap_trap_name,t_eval_wk)) %>%
  unite(trap.variable, trap_trap_name, variable) %>%
  spread(trap.variable,value) %>%
  setNames(gsub("_m","",names(.))) 

trap_wkly_wide2 <- left_join(eval_wks %>% filter(t_train)
                             ,trap_wkly_wide
                             ,by=("t_eval_wk")) %>%
  mutate(t_eval_wk = as.integer(t_eval_wk)) %>% 
  dplyr::select(-c(t_mo,t_wk_end)) 

# ftable(trap_wkly_wide2$t_yr)


trap_wkly_ts <- ts(trap_wkly_wide2 %>% dplyr::select(-(1:6))
                   # ,start=as.Date('2006-01-01')
                   ,frequency=52
)
# plot(trap_wkly_ts[,1:10])

# Plot ts (10 at a time which is the limit for multiple time series plots)
ts.plots <- sapply(1:ceiling(dim(trap_wkly_ts)[2]/10),function(x) {
  ts <- trap_wkly_ts[,
                     ((x-1)*10+1):
                       min(((x-1)*10+10),dim(trap_wkly_ts)[2])
                     ]
  batch.name = paste('Trap-level Weekly Time Series, batch ',x,'.png',sep='')
  png(paste(plot.path,batch.name,sep='\\'))
  plot(ts)
  dev.off()
})





################################################################################
## Create community-level weekly version of community data without imputation

community_wkly_long <- df_train %>% group_by(loc_community,t_eval_wk) %>% 
  summarise(
    n_rows = n()
    ,n_obs = sum(!is.na(mos_any_wnv_present))
    ,mos_max_tot_num_mosquitos = max(mos_tot_num_mosquitos) 
    ,mos_mean_tot_num_mosquitos = mean(mos_tot_num_mosquitos)  
    ,mos_any_wnv_present = max(mos_any_wnv_present) 
  )

community_wkly_wide <- community_wkly_long  %>% 
  group_by(loc_community,t_eval_wk) %>%
  dplyr::select(c('loc_community','t_eval_wk','mos_mean_tot_num_mosquitos')) %>%
  dplyr::rename(m = mos_mean_tot_num_mosquitos) %>%
  gather(variable,value,-c(loc_community,t_eval_wk)) %>%
  unite(community.variable, loc_community, variable) %>%
  spread(community.variable,value) %>%
  setNames(gsub("_m","",names(.))) 

community_wkly_wide2 <- left_join(eval_wks %>% filter(t_train)
                             ,community_wkly_wide
                             ,by=("t_eval_wk")) %>%
  mutate(t_eval_wk = as.integer(t_eval_wk)) %>% 
  dplyr::select(-c(t_mo,t_wk_end)) 

# ftable(community_wkly_wide2$t_yr)


community_wkly_ts <- ts(community_wkly_wide2 %>% dplyr::select(-(1:6))
                   # ,start=as.Date('2006-01-01')
                   ,frequency=52
)
# plot(community_wkly_ts[,1:10])

# Plot ts (10 at a time which is the limit for multiple time series plots)
ts.plots <- sapply(1:ceiling(dim(community_wkly_ts)[2]/10),function(x) {
  ts <- community_wkly_ts[,
                     ((x-1)*10+1):
                       min(((x-1)*10+10),dim(community_wkly_ts)[2])
                     ]
  batch.name = paste('Community-level Weekly Time Series, batch ',x,'.png',sep='')
  png(paste(plot.path,batch.name,sep='\\'))
  plot(ts)
  dev.off()
})



# png(paste(plot.path,'test.png',sep='\\'))
# plot(community_wkly_ts[,1:10])
# plot(community_wkly_ts[,1:10],title='Community-level Weekly Time Series')
# ts.plot(community_wkly_ts)
# dev.off()
# 
# ts.plot(community_wkly_ts[,1:10])



################################################################################
## Impute missing data

community_wkly_ts_imp <- community_wkly_ts


## If there are no observations in this time period, treat it as zero.
observed.values <- rowSums(!is.na(community_wkly_ts_imp))
community_wkly_ts_imp[observed.values == 0,]<- 0

# require(mice)
# miss.data.pat <- md.pattern(community_wkly_ts_imp)

# str(community_wkly_ts_imp)

# Convert back to data.frame before imputing (and clean up names)
community_wkly_ts_imp <- as.data.frame(community_wkly_ts_imp) %>% 
  setNames(gsub(" ","_",names(.))) 

imputed.via.predmean <- mice(community_wkly_ts_imp, m=5, maxit = 10, method = 'pmm', seed = 500)


# Cycle through imputations, using the mean of the 5 imputations
# I used a for loop here which is not great but is quick-and-dirty


extract.mean <- function(x) {
  imputations <- imputed.via.predmean$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values <- function(x) {
  means <- extract.mean(x)
  community_wkly_ts_imp2[as.integer(names(means)),x] <- means
  return(community_wkly_ts_imp2)
}

# extract.mean(names(imputed.via.predmean$imp)[1])
# update.values(names(imputed.via.predmean$imp)[1])$ARCHER_HEIGHTS


summary(community_wkly_ts_imp)

community_wkly_ts_imp2 <- community_wkly_ts_imp

for(col in names(imputed.via.predmean$imp)) {
  community_wkly_ts_imp2 <- update.values(col)
}

summary(community_wkly_ts_imp2)

community_wkly_ts_imp <- ts(community_wkly_ts_imp2,frequency=52)

# ts.plots <- sapply(1:ceiling(dim(community_wkly_ts_imp)[2]/10),function(x) {
#   ts <- community_wkly_ts_imp[,
#                           ((x-1)*10+1):
#                             min(((x-1)*10+10),dim(community_wkly_ts_imp)[2])
#                           ]
#   # batch.name = paste('Community-level Weekly Time Series, batch ',x,'.png',sep='')
#   # png(paste(plot.path,batch.name,sep='\\'))
#   plot(ts)
#   # dev.off()
# })


################################################################################
## Try working with community-level time series (with fully imputed data)

str()
acf(community_wkly_ts_imp[,1])

plot(community_wkly_ts_imp[,1])
plot.ts(community_wkly_ts_imp[,1])

seasonplot(community_wkly_ts_imp[,1])
decompose(community_wkly_ts_imp[,1])

gglagplot(community_wkly_ts_imp[,1])
ggAcf(community_wkly_ts_imp[,1])
ggsubseriesplot(community_wkly_ts_imp[,1])


community_wkly_ts_imp[,1] %>% decompose(type="multiplicative") %>%
  autoplot()

community_wkly_ts_imp[,1] %>% stl(t.window=13, s.window="periodic"
                                  , robust=TRUE) %>%
  autoplot()








