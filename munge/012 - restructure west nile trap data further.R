

################################################################################
## Misc
################################################################################

# # wnv.trap.date2 <- left_join(ref.dates
# #                             ,wnv.trap.date
# #                             ,by=c("date"))
# 
# # wnv.trap.date %>% summarise(nrows=n(),dates=n_distinct(date))
# # wnv.trap.date2 %>% summarise(nrows=n(),dates=n_distinct(date))
# 
# 
# # str(wnv.traps)
# wnv.trap.date2a <- inner_join(
#   cbind(ref.dates,dummy=1)
#   ,cbind(wnv.traps,dummy=1)
#   ,by=c("dummy"))
# wnv.trap.date2b <- left_join(wnv.trap.date2a[,!colnames(wnv.trap.date2a) %in% c("rev.lat","rev.lng")]
#                              ,wnv.trap.date[,!colnames(wnv.trap.date) %in% 
#                                               c("yr",'mo','wk','day.of.wk'
#                                                 ,'day.of.wk.name'
#                                                 ,'satellite.ind'
#                                                 ,'lat.lng.src')]
#                              ,by=c("trap.name","date"))
# 
# wnv.trap.date2a %>% summarise(nrows=n(),dates=n_distinct(date),traps=n_distinct(trap.name))
# wnv.trap.date2b %>% summarise(nrows=n(),dates=n_distinct(date),traps=n_distinct(trap.name))
# 
# 
# wnv.trap.date2b <- wnv.trap.date2b[,!colnames(wnv.trap.date2b) %in% c("dummy")]
# 
# 
# summary(wnv.trap.date2b)
# 
# wnv.trap.date2c <- left_join(wnv.trap.date2b
#                               ,)



################################################################################
## Clean up trap-level summary data
################################################################################

str(wnv.traps)

colnames(wnv.traps)[colnames(wnv.traps) == 'rev.lat'] <- 'lat'
colnames(wnv.traps)[colnames(wnv.traps) == 'rev.lng'] <- 'lng'
wnv.traps$lat <- as.numeric(wnv.traps$lat)
wnv.traps$lng <- as.numeric(wnv.traps$lng)

print(colnames(wnv.traps))

wnv.traps <- 
  wnv.traps[,
            c("trap.name","lat","lng"                     
              ,"lat.lng.src","satellite.ind"
              ,"ZCTA5CE10","BlkGrp.geoid","Tract.geoid","community"               
              ,"zone_class","zone_type"            
              ,"LT_HS_pct__BlkGrp2017","median_HHInc__BlkGrp2017","LT_Pov_pct__BlkGrp2017"  
              ,"LT_HS_pct__Tract2017","median_HHInc__Tract2017","LT_Pov_pct__Tract2017"  
            )
            ] %>% arrange(trap.name)
wnv.traps$community <- as.factor(wnv.traps$community)

wnv.traps$median_HHInc__BlkGrp2017 <- ifelse(is.na(wnv.traps$median_HHInc__BlkGrp2017)
                                             ,0,wnv.traps$median_HHInc__BlkGrp2017)
wnv.traps$median_HHInc__Tract2017 <- ifelse(is.na(wnv.traps$median_HHInc__Tract2017)
                                            ,0,wnv.traps$median_HHInc__Tract2017)
wnv.traps$LT_Pov_pct__BlkGrp2017 <- ifelse(is.na(wnv.traps$LT_Pov_pct__BlkGrp2017)
                                           ,0,wnv.traps$LT_Pov_pct__BlkGrp2017)
wnv.traps$LT_Pov_pct__Tract2017 <- ifelse(is.na(wnv.traps$LT_Pov_pct__Tract2017)
                                          ,0,wnv.traps$LT_Pov_pct__Tract2017)
wnv.traps$LT_HS_pct__BlkGrp2017 <- ifelse(is.na(wnv.traps$LT_HS_pct__BlkGrp2017)
                                          ,0,wnv.traps$LT_HS_pct__BlkGrp2017)
wnv.traps$LT_HS_pct__Tract2017 <- ifelse(is.na(wnv.traps$LT_HS_pct__Tract2017)
                                         ,0,wnv.traps$LT_HS_pct__Tract2017)

summary(wnv.traps)

# wnv.community <- wnv.traps %>% group_by(community) %>% summarise(n=n())
# wnv.BlkGrp <- wnv.traps %>% group_by(BlkGrp.geoid) %>% summarise(n=n())
# wnv.Tract <- wnv.traps %>% group_by(Tract.geoid) %>% summarise(n=n())
# wnv.zcta <- wnv.traps %>% group_by(ZCTA5CE10) %>% summarise(n=n())
# wnv.zone_class <- wnv.traps %>% group_by(zone_class) %>% summarise(n=n())
# wnv.zone_type <- wnv.traps %>% group_by(zone_type) %>% summarise(n=n())

wnv.traps[rowSums(is.na(wnv.traps)) > 0,]
# No rows with missing data on these key variables.  Good.  




################################################################################
## Combine wnv trap data with reference dates
################################################################################

# Get column names related to date variables so that I can make sure I only
# include them once in the final dataset.
date.metadata <- colnames(ref.dates)
date.metadata <- date.metadata[!date.metadata %in% c("date")]

# Get unique trap names.
# wnv.traps.rev0 <- wnv.trap.date %>% group_by(trap.name) %>% summarise(n=n())
# str(wnv.traps.rev0)
trap.metadata <- colnames(wnv.traps)
trap.metadata <- trap.metadata[!trap.metadata %in% c("trap.name")]

# Cartesian join between trap names and reference dates.  This is an 
# exploded dataset with many rows we won't be using for modeling but will 
# possibly need in order to pick up lagged weather data, etc.  
wnv.trap.date.rev0 <- inner_join(cbind(ref.dates,dummy=1)
                                 ,cbind(wnv.traps,dummy=1)
                                 ,by=c("dummy"))
# str(wnv.trap.date.rev0)
# Remove the dummy variable that we used to force dplyr to do a Cartesian
# join.
wnv.trap.date.rev0 <- wnv.trap.date.rev0[,!colnames(wnv.trap.date.rev0) %in%
                                           c("dummy")]
str(wnv.trap.date.rev0)

# Remove date-related variables from the trap/date pair data that we don't 
# need at the moment.
wnv.trap.date.rev1 <- wnv.trap.date[,!colnames(wnv.trap.date) %in% date.metadata]
wnv.trap.date.rev1 <- wnv.trap.date.rev1[,!colnames(wnv.trap.date.rev1) %in% 
                                           c(trap.metadata
                                             ,"rev.lat","rev.lng"
                                             ,"lat.lng.src.x","satellite.ind.x"
                                             ,"STATEFP","COUNTYFP","TRACTCE"
                                             ,"BLKGRPCE","lat.lng.src.y"
                                             ,"satellite.ind.y","location")
                                         ]
str(wnv.trap.date.rev1)

# Combine actual mosquito trap data (one row per trap per data collection date)
# with set of complete trap/date pairs 2006-18
wnv.trap.date.rev <- left_join(wnv.trap.date.rev0
                               ,wnv.trap.date.rev1
                               ,by=c("trap.name","date"))
summary(wnv.trap.date.rev)


################################################################################
## Spot check data
################################################################################

chk.a.trap <- wnv.trap.date.rev[wnv.trap.date.rev$trap.name %in% wnv.traps.rev0$trap.name[15],]
summary(chk.a.trap)


################################################################################
## Combine with weather data
################################################################################

str(wnv.trap.date.rev)
str(chgo.airports.wide.imp)

wnv.trap.date.rev2 <- left_join(
  wnv.trap.date.rev
  ,chgo.airports.wide.imp
  ,by=c("date")
)

dim(wnv.trap.date.rev)
dim(wnv.trap.date.rev2)


str(wnv.trap.date.rev2)



################################################################################
## Save trap/date pairs with ancillary data sources merged to date
################################################################################

base.path <- "D:\\ajc188\\github\\capstone_project"

my.path <- paste(base.path,"data","processed",sep="\\")


getwd()
setwd(my.path)
getwd()
list.files()


save(wnv.trap.date.rev2, file="wnv.trap.date.rev2.RData", compress = FALSE)
write.csv(wnv.trap.date.rev2, paste(my.path,'wnv.trap.date.rev2.csv',sep='\\'),row.names = FALSE)



