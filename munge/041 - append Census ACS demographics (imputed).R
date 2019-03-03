

################################################################################
## Load Census ACS demographics (extracted from 5-year Census summary files
## in SAS after those files and numerous supporting documents were downloaded
## via FTP)

base.path <- "D:\\ajc188\\github\\capstone_project"
src.path <- paste(base.path,"data","raw","census_acs",sep="\\")

demo.blkgrp <- read.csv(paste(src.path,"Demographics_BlkGrp.csv",sep="\\"),stringsAsFactors = FALSE)
demo.tract <- read.csv(paste(src.path,"Demographics_tract.csv",sep="\\"),stringsAsFactors = FALSE)



################################################################################
## Bring in Census ACS demographics

# colnames(df_results)
# colnames(df_results2)

df_results <- df_results %>% 
  left_join(.
            ,demo.blkgrp %>% 
              filter(year==2017) %>%
              setNames(paste("demo_blkgrp",names(.),'2017',sep="_")) %>%
              mutate(loc_census_block_group_id = as.numeric(gsub("15000US",'',demo_blkgrp_GEOID_2017))) %>%
              dplyr::select(-c(demo_blkgrp_SUMLEVEL_2017,demo_blkgrp_FILETYPE_2017,demo_blkgrp_GEOID_2017))
            ,by=c("loc_census_block_group_id")) %>%
  left_join(.
            ,demo.tract %>% 
              filter(year==2017) %>%
              setNames(paste("demo_tract",names(.),'2017',sep="_")) %>%
              mutate(loc_census_tract_id = as.numeric(gsub("14000US",'',demo_tract_GEOID_2017))) %>%
              dplyr::select(-c(demo_tract_SUMLEVEL_2017,demo_tract_FILETYPE_2017,demo_tract_GEOID_2017))
            ,by=c("loc_census_tract_id")) 

summary(df_results %>% dplyr::select(contains('demo_blkgrp')))
summary(df_results %>% dplyr::select(contains('demo_tract')))

# Do we need to impute median age?  


################################################################################
## Impute missing demographics (just median age)

summary(blkgrp_median_age)
summary(demo.blkgrp[is.na(demo.blkgrp$age_median),])
hist(demo.blkgrp[is.na(demo.blkgrp$age_median),'tot_pop'])
hist(demo.blkgrp[!is.na(demo.blkgrp$age_median),'tot_pop'])
# EDA says these are block groups with very low population.

blkgrp_median_age <- demo.blkgrp %>%
  dplyr::select(matches('age|GEOID|year|under18|18to64|65plus')) %>%
  arrange(GEOID,year)

require("mice")
blkgrp_median_age_imp0 <- mice(blkgrp_median_age, m=10, maxit = 10, method = 'pmm', seed = 500)

extract.mean.blk.demo <- function(x,imp.obj) {
  imputations <- imp.obj$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values.blk.demo <- function(x,imp.obj) {
  means <- extract.mean.blk.demo(x,imp.obj)
  blkgrp_median_age_imp[as.integer(names(means)),x] <- means
  return(blkgrp_median_age_imp)
}

blkgrp_median_age_imp <- blkgrp_median_age

for(col in names(blkgrp_median_age_imp0$imp)) {
  # print(dim(blkgrp_median_age_imp0$imp[col][[1]])[1])
  if(dim(blkgrp_median_age_imp0$imp[col][[1]])[1] > 0 ) {
    # print(col)
    blkgrp_median_age_imp <- update.values.blk.demo(col,blkgrp_median_age_imp0)
    # update.values.blk.demo(col,blkgrp_median_age_imp0)
  }
}

# str(blkgrp_median_age_imp)
summary(blkgrp_median_age_imp)



tract_median_age <- demo.tract %>%
  dplyr::select(matches('age|GEOID|year|under18|18to64|65plus')) %>%
  arrange(GEOID,year)

tract_median_age_imp0 <- mice(tract_median_age, m=10, maxit = 10, method = 'pmm', seed = 500)

extract.mean.tract.demo <- function(x,imp.obj) {
  imputations <- imp.obj$imp[x]
  means <- rowMeans(as.data.frame(imputations))
  return(means)
}

update.values.tract.demo <- function(x,imp.obj) {
  means <- extract.mean.tract.demo(x,imp.obj)
  tract_median_age_imp[as.integer(names(means)),x] <- means
  return(tract_median_age_imp)
}

tract_median_age_imp <- tract_median_age

for(col in names(tract_median_age_imp0$imp)) {
  # print(dim(tract_median_age_imp0$imp[col][[1]])[1])
  if(dim(tract_median_age_imp0$imp[col][[1]])[1] > 0 ) {
    # print(col)
    tract_median_age_imp <- update.values.tract.demo(col,tract_median_age_imp0)
    # update.values.tract.demo(col,tract_median_age_imp0)
  }
}

# str(tract_median_age_imp)
summary(tract_median_age_imp)



################################################################################
## Combine imputed data with the rest of the ACS demographics

demo.tract.imp <- demo.tract %>%
  dplyr::select(-matches('age|under18|18to64|65plus')) %>%
  left_join(tract_median_age_imp) %>%
  arrange(GEOID,year)

summary(demo.tract.imp)


demo.blkgrp.imp <- demo.blkgrp %>%
  dplyr::select(-matches('age|under18|18to64|65plus')) %>%
  left_join(blkgrp_median_age_imp) %>%
  arrange(GEOID,year)

summary(demo.blkgrp.imp)



################################################################################
## Since ACS 5-year summaries are published with an almost 2-year lag, force 
## our models to use only data available two years earlier.

demo.blkgrp.imp$available.year <- demo.blkgrp.imp$year + 2
demo.tract.imp$available.year <- demo.tract.imp$year + 2



################################################################################
## Save imputed/formatted data


save(demo.blkgrp.imp, file=paste(src.path,"demo.blkgrp.imp.RData",sep='\\'), compress = FALSE)
save(demo.tract.imp, file=paste(src.path,"demo.tract.imp.RData",sep='\\'), compress = FALSE)


write.csv(demo.blkgrp.imp, paste(src.path,'demo.blkgrp.imp.csv',sep='\\'),row.names = FALSE)
write.csv(demo.tract.imp, paste(src.path,'demo.tract.imp.csv',sep='\\'),row.names = FALSE)




################################################################################
## Create mappings from WNV trap data to demographics

# wnv.2.demo <- fuzzyjoin::fuzzy_inner_join(
#   df_results %>% dplyr::select(trap_trap_name,t_yr,loc_census_block_group_id)
#   ,demo.blkgrp.imp %>% 
#     mutate(loc_census_block_group_id = as.numeric(gsub('15000US','',GEOID))) %>%
#     dplyr::select(c(loc_census_block_group_id,available.year)) 
#   ,by=c("loc_census_block_group_id"="loc_census_block_group_id"
#         ,"t_yr"="available.year")
#   ,match_fun=list(`==`,`>=`)
# ) %>% 

# ,demo.blkgrp.imp %>% dplyr::select(-c(SUMLEVEL,FILETYPE,year)) %>%
#   setNames(paste('demo','bg',names(.),sep='_')) %>%
#   dplyr::select()

bg.yr <- df_results %>% 
  dplyr::select(t_yr,loc_census_block_group_id) %>% 
  distinct() %>% arrange(loc_census_block_group_id,t_yr)

bg.yr2 <- bg.yr %>%
  left_join(.
            ,demo.blkgrp.imp %>% 
              mutate(loc_census_block_group_id = as.numeric(gsub('15000US','',GEOID))) %>%
              dplyr::select(c(loc_census_block_group_id,year,available.year)) 
            ,by=c("loc_census_block_group_id"="loc_census_block_group_id")
            ,"t_yr"="available.year") %>%
  filter(available.year <= t_yr) %>%
  group_by(loc_census_block_group_id,t_yr) %>% 
  summarize(pre.available.year = max(available.year))

bg.yr2b <- bg.yr %>%
  left_join(.
            ,demo.blkgrp.imp %>% 
              mutate(loc_census_block_group_id = as.numeric(gsub('15000US','',GEOID))) %>%
              dplyr::select(c(loc_census_block_group_id,year,available.year)) 
            ,by=c("loc_census_block_group_id"="loc_census_block_group_id")
            ,"t_yr"="available.year") %>%
  filter(available.year >= t_yr) %>%
  group_by(loc_census_block_group_id,t_yr) %>% 
  summarize(post.available.year = min(available.year))

bg.yr3 <- bg.yr %>%
  left_join(bg.yr2,by=c("loc_census_block_group_id","t_yr")) %>%
  left_join(bg.yr2b,by=c("loc_census_block_group_id","t_yr")) %>%
  mutate(available.year = coalesce(pre.available.year,post.available.year))
  
df_results <- df_results %>%
  inner_join(bg.yr3 %>% dplyr::select(-c(pre.available.year,post.available.year))
             ,by=c("loc_census_block_group_id","t_yr")) %>%
  inner_join(demo.blkgrp.imp %>% dplyr::select(-c(SUMLEVEL,FILETYPE)) %>%
               rename(acs_year = year) %>%
               setNames(paste('demo','bg',names(.),sep='_')) %>%
               mutate(loc_census_block_group_id = as.numeric(gsub('15000US','',demo_bg_GEOID))) %>%
               dplyr::select(-c(demo_bg_GEOID))
             ,by=c("loc_census_block_group_id","available.year" = "demo_bg_available.year")
  ) %>% dplyr::select(-available.year)
# summary(df_results2$available.year)

# colnames(df_results)
# colnames(df_results2)
# summary(df_results2$available.year)
# summary(as.factor(df_results2$available.year))
# summary(as.factor(trap.bg.yr3$available.year))
# ftable(df_results2$t_yr,df_results2$available.year)
# ftable(df_results2$t_yr,df_results2$acs_year)
# 
# summary(df_results2$loc_census_block_group_id)
# summary(df_results2$demo_bg_female_pct)
# summary(as.factor(df_results2[is.na(df_results2$demo_bg_female_pct),]$available.year))
# summary(as.factor(df_results2$demo_bg_acs_year))
# summary(df_results2 %>% dplyr::select(matches('demo_bg_')))
# ftable(df_results2$t_yr,df_results2$demo_bg_acs_year)
# ftable(as.factor(demo.blkgrp.imp$year))
## Something is still off here, but I think it's due to block groups changing
## between 2009 and 2010.  I don't necessarily want to re-match lat/lng to 
## 2009 block groups, so we'll just work with this as "good enough."  





tract.yr <- df_results %>% 
  dplyr::select(t_yr,loc_census_tract_id) %>% 
  distinct() %>% arrange(loc_census_tract_id,t_yr)

tract.yr2 <- tract.yr %>%
  left_join(.
            ,demo.tract.imp %>% 
              mutate(loc_census_tract_id = as.numeric(gsub('14000US','',GEOID))) %>%
              dplyr::select(c(loc_census_tract_id,year,available.year)) 
            ,by=c("loc_census_tract_id"="loc_census_tract_id")
            ,"t_yr"="available.year") %>%
  filter(available.year <= t_yr) %>%
  group_by(loc_census_tract_id,t_yr) %>% 
  summarize(pre.available.year = max(available.year))

tract.yr2b <- tract.yr %>%
  left_join(.
            ,demo.tract.imp %>% 
              mutate(loc_census_tract_id = as.numeric(gsub('14000US','',GEOID))) %>%
              dplyr::select(c(loc_census_tract_id,year,available.year)) 
            ,by=c("loc_census_tract_id"="loc_census_tract_id")
            ,"t_yr"="available.year") %>%
  filter(available.year >= t_yr) %>%
  group_by(loc_census_tract_id,t_yr) %>% 
  summarize(post.available.year = min(available.year))

tract.yr3 <- tract.yr %>%
  left_join(tract.yr2,by=c("loc_census_tract_id","t_yr")) %>%
  left_join(tract.yr2b,by=c("loc_census_tract_id","t_yr")) %>%
  mutate(available.year = coalesce(pre.available.year,post.available.year))

df_results <- df_results %>%
  inner_join(tract.yr3 %>% dplyr::select(-c(pre.available.year,post.available.year))
             ,by=c("loc_census_tract_id","t_yr")) %>%
  inner_join(demo.tract.imp %>% dplyr::select(-c(SUMLEVEL,FILETYPE)) %>%
               rename(acs_year = year) %>%
               setNames(paste('demo','tract',names(.),sep='_')) %>%
               mutate(loc_census_tract_id = as.numeric(gsub('14000US','',demo_tract_GEOID))) %>%
               dplyr::select(-c(demo_tract_GEOID))
             ,by=c("loc_census_tract_id","available.year" = "demo_tract_available.year")
  ) %>% dplyr::select(-available.year)
# summary(df_results2$available.year)

# colnames(df_results)
# colnames(df_results2)
# summary(df_results2$available.year)
# summary(as.factor(df_results2$available.year))
# summary(as.factor(trap.tract.yr3$available.year))
# ftable(df_results2$t_yr,df_results2$available.year)
# ftable(df_results2$t_yr,df_results2$acs_year)
# 
# summary(df_results2$loc_census_tract_id)
# summary(df_results2$demo_tract_female_pct)
# summary(as.factor(df_results2[is.na(df_results2$demo_tract_female_pct),]$available.year))
# summary(as.factor(df_results2$demo_tract_acs_year))
# summary(df_results2 %>% dplyr::select(matches('demo_tract_')))
# ftable(df_results2$t_yr,df_results2$demo_tract_acs_year)
# ftable(as.factor(demo.tract.imp$year))
## Something is still off here, but I think it's due to block groups changing
## between 2009 and 2010.  I don't necessarily want to re-match lat/lng to 
## 2009 block groups, so we'll just work with this as "good enough."  



