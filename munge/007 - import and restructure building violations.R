

################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
repo.path <- "D:\\ajc188\\github\\capstone_project"

filename.list <- list.files(path = paste(repo.path,"data\\raw\\building_violations",sep='\\'))           
filename.list



################################################################################
## Load building violation data
################################################################################

violations.csv <- "Building_Violations.csv"

violations <- read.csv(paste(repo.path,"data\\raw\\building_violations",violations.csv,sep="\\"),stringsAsFactors = FALSE)

# str(violations)




################################################################################
## Data manipulation
################################################################################

violations$violation.date <- as.Date(violations$VIOLATION.DATE,format="%m/%d/%Y")

violation.locations <- violations %>% group_by(LATITUDE,LONGITUDE) %>%
  summarise(nrows=n()
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
            # ,max.total.paid = max(is.finite(Total.Paid)*Total.Paid,na.rm = TRUE)
  )


################################################################################
## Save aggregated data
################################################################################


# save(wnv.traps, file="wnv.traps.RData", compress = FALSE)
# write.csv(wnv.traps, paste(my.path,'wnv.traps.csv',sep='\\'),row.names = FALSE)

write.csv(violation.locations,paste(repo.path,"data\\raw\\building_violations","violation.locations.csv",sep='\\'),row.names=FALSE)



################################################################################
## Convert coordinates to a shapefile in ESRI ArcGIS then to a layer file
## then perform spatial join (intersection) with block group shapefile, then 
## convert resulting shapefile's tabular component (DBF) to CSV.  Finally, 
## read back into R.  
## Repeat for Chicago community area and Zip Code Tabulation Area (ZCTA).
################################################################################

viol.loc.zcta <- read.dbf(paste(repo.path,'data\\raw\\building_violations\\GIS','violation_locations_zcta.dbf',sep="\\"))
viol.loc.blkgrp <- read.dbf(paste(repo.path,'data\\raw\\building_violations\\GIS','violation_locations_bg.dbf',sep="\\"))
viol.loc.community.area <- read.dbf(paste(repo.path,'data\\raw\\building_violations\\GIS','violation_locations_CommArea.dbf',sep="\\"))

# str(viol.loc.blkgrp)

viol.loc.1 <- left_join(violation.locations,viol.loc.zcta[,c("LATITUDE","LONGITUDE","ZCTA5CE10")]
                        ,by=c("LATITUDE","LONGITUDE"))
viol.loc.2 <- left_join(viol.loc.1,viol.loc.blkgrp[,c("LATITUDE","LONGITUDE","STATEFP"
                                                      ,"COUNTYFP"
                                                      ,"TRACTCE"
                                                      ,"BLKGRPCE"
                                                      ,"GEOID")]
                        ,by=c("LATITUDE","LONGITUDE"))
viol.loc.3 <- left_join(viol.loc.2,viol.loc.community.area[,c("LATITUDE","LONGITUDE","community")]
                        ,by=c("LATITUDE","LONGITUDE"))

summary(viol.loc.3)
violation.locations <- viol.loc.3


################################################################################
## Save updated geographic file
################################################################################

write.csv(violation.locations,paste(repo.path,"data\\raw\\building_violations","violation.locations.csv",sep='\\'),row.names=FALSE)


################################################################################
## Merge back into violations data
################################################################################

violations2 <- left_join(violations,violation.locations,by=c("LATITUDE","LONGITUDE"))


violations.commarea <- violations2 %>% group_by(community) %>%
    summarise(nrows=n()
              ,min.violation.date=min(violation.date)
              ,max.violation.date = max(violation.date)
    ) %>% arrange(-nrows)

violations.blkgrp <- violations2 %>% group_by(GEOID) %>%
  summarise(nrows=n()
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
  ) %>% arrange(-nrows)


violations.zcta <- violations2 %>% group_by(ZCTA5CE10) %>%
  summarise(nrows=n()
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
  ) %>% arrange(-nrows)


str(violations2)

violations2.head <- head(violations2)

violations2 %>% group_by(INSPECTION.STATUS,INSPECTION.WAIVED) %>%
  summarise(nrows=n()
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
  ) %>% arrange(-nrows) 

viol.chk1 <- violations2 %>% group_by(INSPECTION.CATEGORY,DEPARTMENT.BUREAU) %>%
  summarise(nrows=n()
            ,n.failed = sum(INSPECTION.STATUS %in% c("FAILED"))
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
  ) %>% arrange(-nrows)  

viol.chk2 <- violations2 %>% group_by(DEPARTMENT.BUREAU) %>%
  summarise(nrows=n()
            ,n.failed = sum(INSPECTION.STATUS %in% c("FAILED"))
            ,min.violation.date=min(violation.date)
            ,max.violation.date = max(violation.date)
  ) %>% arrange(-nrows)  

not.failed1 <- violations2[!violations2$INSPECTION.STATUS %in% c("FAILED","OPEN"),][1:100,]
not.failed2 <- inner_join(violations2,not.failed1,by=c("LATITUDE","LONGITUDE",
                                                       "DEPARTMENT.BUREAU")) %>%
  arrange(LATITUDE,LONGITUDE,violation.date.x)



# str(violations2)
# 
# violations2b <- violations2[,colnames(violations2) %in% c(
#   "ID","violation.date","LATITUDE","LONGITUDE","LOCATION"
#   ,"ZCTA5CE10","STATEFP","COUNTYFP","TRACTCE","BLKGRPCE","GEOID","community"
#   ,"DEPARTMENT.BUREAU","INSPECTION.CATEGORY","VIOLATION.STATUS"
# )]

# str(violations2b)

violations2c <- violations2[,colnames(violations2) %in% c(
  "ID","violation.date","LATITUDE","LONGITUDE"
  ,"ZCTA5CE10","GEOID","community"
  ,"DEPARTMENT.BUREAU","INSPECTION.CATEGORY","VIOLATION.STATUS"
)]

summary(violations2c$violation.date)
str(violations2c)



################################################################################
## Aggregate to specific date for 3 different geographic area levels
################################################################################


violations.BlkGrp.date <- violations2 %>% 
  group_by(GEOID,violation.date) %>%
  summarise(violations.cnt = n())

violations.zcta.date <- violations2 %>% 
  group_by(ZCTA5CE10,violation.date) %>%
  summarise(violations.cnt = n())

violations.CommArea.date <- violations2 %>% 
  group_by(community,violation.date) %>%
  summarise(violations.cnt = n())

# summary(violations.BlkGrp.date)
# summary(violations.zcta.date)
# summary(violations.CommArea.date)



################################################################################
## Aggregate to specific date for 3 different geographic area levels
################################################################################

# BlkGrps <- violations.BlkGrp.date %>% group_by(GEOID) %>% summarise(n=n(),dummy=1)
# ZCTAs <- violations.zcta.date %>% group_by(ZCTA5CE10) %>% summarise(n=n(),dummy=1)
# CommAreas <- violations.CommArea.date %>% group_by(community) %>% summarise(n=n(),dummy=1)
# 
# BlkGrp.dates <- inner_join(cbind(ref.dates,dummy=1),BlkGrps,by=c("dummy"))
# ZCTA.dates <- inner_join(cbind(ref.dates,dummy=1),ZCTAs,by=c("dummy"))
# CommArea.dates <- inner_join(cbind(ref.dates,dummy=1),CommAreas,by=c("dummy"))


wnv.trap.date2 <- inner_join(wnv.traps
                             ,wnv.trap.date[,colnames(wnv.trap.date) == 'trap.name' || 
                                              !colnames(wnv.trap.date) %in% 
                                              colnames(wnv.traps)]
                             ,by=c("trap.name"))
# BlkGrp.date.pairs <- wnv.trap.date2 %>% group_by(BlkGrp.geoid,date) %>% summarise(n=n())
# BlkGrp.date.pairs$date.prev.180d <- BlkGrp.date.pairs$date - 30

# require(fuzzyjoin)
# str(BlkGrp.date.pairs)
# 
# violations.BlkGrp.date2 <- fuzzyjoin::fuzzy_left_join(BlkGrp.date.pairs 
#                                      ,violations.BlkGrp.date
#                                      ,by=c("date"="violation.date"
#                                            ,"date.prev.180d"="violation.date"
#                                            ,"BlkGrp.geoid"="GEOID")
#                                      ,match_fun = list(`<=`,`>=`,`=`)) %>%
#   group_by(BlkGrp.geoid,date) %>% summarise(violation.cnt = sum(violation.cnt))



aggr.viol.for.date <- function(df,viol.df,window.d=180) {
  df$date.begin <- df$date - window.d
  
  # print(viol.df)
  
  # print(summary(as.factor(df[,cols])))
  # print(summary(viol.df[,cols]))
  out.df <-  fuzzyjoin::fuzzy_left_join(df
                                        ,viol.df
                                        ,by=c("date"="violation.date"
                                              ,"date.begin"="violation.date")
                                        ,match_fun = list(`>=`,`<=`)) %>%
    group_by(date) %>% summarise(violation.cnt = sum(violation.cnt))
  # print(out.df)
  return(out.df)
}


# test.1 <- wnv.trap.date2[1:6,]
# aggr.viol.for.date(test.1,violations2)


aggr.viol.by.col <- function(cols,df=wnv.trap.date2,viol.df=violations2) {
  df.place <- df %>% group_by(!!! syms(cols)) %>% summarise(n=n())
  viol.place <- viol.df %>% group_by(!!! syms(cols)) %>% summarise(n=n())
  places <- inner_join(df.place,viol.place,by=cols)

  place.list <- as.list(rep(NA,nrow(places)))
  # return(place.list)
  # place.date.viol <
  print(paste("Places: ",nrow(places)))
  for(i in 1:nrow(places)) {
    place.df.dates <- semi_join(df,places[i,],by=cols) %>%
      group_by(!!! syms(cols),date) %>% summarise(n=n())
    # return(place.df.dates)
    # print(paste(as.character(places[i,1]),dim(place.df.dates)[1]))
    if(dim(place.df.dates)[1] == 0) place.list[i] <- NA
    else {
      place.viol.df.dates <- semi_join(viol.df,places[i,],by=cols) %>%
        group_by(!!! syms(cols),violation.date) %>% summarise(violation.cnt = n())
      # return(place.viol.df.dates)
      # print(paste(as.character(places[i,1]),dim(place.viol.df.dates)[1]))
      if(dim(place.viol.df.dates)[1] == 0) place.list[i] <- NA
      else {
        if(exists("place.date.violations")) {
          place.date.violations <- bind_rows(
            place.date.violations
            ,cbind(places[i,length(cols)],aggr.viol.for.date(place.df.dates,place.viol.df.dates))
            )
        }
        else {
          place.date.violations <- cbind(places[i,length(cols)],aggr.viol.for.date(place.df.dates,place.viol.df.dates))
        }
        # place.list[i] <- aggr.viol.for.date(place.df.dates,place.viol.df.dates)
        # print(str(place.list[i]))
        # place.list[[i]] <- left_join(place.df.dates,place.list[[i]],by=c("date"))
        # place.list[[i]]$violation.cnt <- coalesce(place.list[[i]]$violation.cnt,0)
        # x <- aggr.viol.for.date(cols,place.df.dates,place.viol.df.dates)
        # print(str(x))
      }
    }
  }
  # rm(place.date.violations)
  return(place.date.violations)
}

# test <- aggr.viol.by.col(c('community'))

# test <- aggr.viol.by.col(c('community'),df=wnv.trap.date2[100:2000,])
# dim(wnv.trap.date2)

# test <- as.list(c("A","B","C","D","E")); test[1]


# print(str(test))
# str(test[1])

CommArea.date.violations <- aggr.viol.by.col(c('community'))
# dim(CommArea.date.violations)
summary(CommArea.date.violations)

names(violations2)[names(violations2) == 'GEOID'] <- 'BlkGrp.geoid'
violations2$BlkGrp.geoid <- as.character(violations2$BlkGrp.geoid)
wnv.trap.date2$BlkGrp.geoid <- as.character(wnv.trap.date2$BlkGrp.geoid)
# str(wnv.trap.date2$BlkGrp.geoid)
# str(violations2$BlkGrp.geoid)
BlkGrp.date.violations <- aggr.viol.by.col(c('BlkGrp.geoid'))
# chk.bg <- as.data.frame(summary(as.factor(wnv.trap.date2$BlkGrp.geoid)))
# ftable(wnv.trap.date2$BlkGrp.geoid)
# chk.bg <- as.data.frame(summary(as.factor(violations2$BlkGrp.geoid)))

wnv.trap.date2$ZCTA5CE10 <- as.character(wnv.trap.date2$ZCTA5CE10)
violations2$BlkGrp.geoid <- as.character(violations2$BlkGrp.geoid)
wnv.trap.date2$BlkGrp.geoid <- as.character(wnv.trap.date2$BlkGrp.geoid)
# str(wnv.trap.date2$ZCTA5CE10)
# str(violations2$ZCTA5CE10)
zcta.date.violations <- aggr.viol.by.col(c('ZCTA5CE10'))
# chk.zcta <- as.data.frame(summary(as.factor(violations2$ZCTA5CE10)))

summary(zcta.date.violations)

str(CommArea.date.violations)
str(BlkGrp.date.violations)
str(zcta.date.violations)



################################################################################
## Save to CSV and R data.frames
################################################################################



write.csv(violations2c,paste(repo.path,"data\\raw\\building_violations","violations.csv",sep='\\'),row.names=FALSE)


write.csv(CommArea.date.violations,paste(repo.path,"data\\raw\\building_violations","CommArea.date.violations.csv",sep='\\'),row.names=FALSE)
write.csv(BlkGrp.date.violations,paste(repo.path,"data\\raw\\building_violations","BlkGrp.date.violations.csv",sep='\\'),row.names=FALSE)
write.csv(zcta.date.violations,paste(repo.path,"data\\raw\\building_violations","zcta.date.violations.csv",sep='\\'),row.names=FALSE)


getwd()
setwd(paste(repo.path,"data\\raw\\building_violations",sep='\\'))
getwd()

save(CommArea.date.violations, file="CommArea.date.violations.RData", compress = FALSE)
save(BlkGrp.date.violations, file="BlkGrp.date.violations.RData", compress = FALSE)
save(zcta.date.violations, file="zcta.date.violations.RData", compress = FALSE)




################################################################################
## Append violation counts to wnv.trap.date data.frame
################################################################################

str(CommArea.date.violations)

names(CommArea.date.violations)[names(CommArea.date.violations) == 'violation.cnt'] <- 'comm.180d.violation.cnt'
names(BlkGrp.date.violations)[names(BlkGrp.date.violations) == 'violation.cnt'] <- 'BlkGrp.180d.violation.cnt'
names(zcta.date.violations)[names(zcta.date.violations) == 'violation.cnt'] <- 'zcta.180d.violation.cnt'


wnv.trap.date3 <- left_join(wnv.trap.date2,CommArea.date.violations)
wnv.trap.date4 <- left_join(wnv.trap.date3,BlkGrp.date.violations)
wnv.trap.date5 <- left_join(wnv.trap.date4,zcta.date.violations)


summary(wnv.trap.date5$comm.180d.violation.cnt)
summary(wnv.trap.date5$BlkGrp.180d.violation.cnt)
summary(wnv.trap.date5$zcta.180d.violation.cnt)

wnv.trap.date5$comm.180d.violation.cnt <- coalesce(wnv.trap.date5$comm.180d.violation.cnt,as.integer(0))
wnv.trap.date5$BlkGrp.180d.violation.cnt <- coalesce(wnv.trap.date5$BlkGrp.180d.violation.cnt,as.integer(0))
wnv.trap.date5$zcta.180d.violation.cnt <- coalesce(wnv.trap.date5$zcta.180d.violation.cnt,as.integer(0))

summary(wnv.trap.date5$comm.180d.violation.cnt)
summary(wnv.trap.date5$BlkGrp.180d.violation.cnt)
summary(wnv.trap.date5$zcta.180d.violation.cnt)

# summary(BlkGrp.date.violations$BlkGrp.180d.violation.cnt)
# summary(wnv.trap.date4$BlkGrp.180d.violation.cnt)
# str(BlkGrp.date.violations$BlkGrp.geoid)
# str(wnv.trap.date4$BlkGrp.geoid)
# str(wnv.trap.date3$BlkGrp.geoid)
# str(wnv.trap.date2$BlkGrp.geoid)
# str(wnv.trap.date$BlkGrp.geoid)
# str(wnv.trap.date$GEOID)
# str(wnv.trap.date$geoid)

wnv.trap.date <- wnv.trap.date5


# str(wnv.trap.date)

wnv.trap.date$lat <- as.numeric(wnv.trap.date$lat)
wnv.trap.date$lng <- as.numeric(wnv.trap.date$lng)


# kdepairs(wnv.trap.date[,c("comm.180d.violation.cnt"
#                           ,"BlkGrp.180d.violation.cnt"
#                           ,"zcta.180d.violation.cnt"
#                           # ,"LT_HS_pct__BlkGrp2017"
#                           # ,"LT_Pov_pct__BlkGrp2017"
#                           # ,"median_HHInc__BlkGrp2017"
#                           ,"lat"
#                           ,"lng"
#                           ,"tot.NumMosquitos")
#                        ])


################################################################################
## Clean up more unnecessary objects
################################################################################


################################################################################
## Save updated WNV trap date dataframe
## Note: no strong association seen.  I wonder if we could improve this if we
##   divide number of building violations by population or another denominator.
## Or perhaps we could filter building violation types.
################################################################################

getwd()
setwd(paste(repo.path,"data\\processed",sep='\\'))
getwd()


save(wnv.trap.date, file="wnv.trap.date.RData", compress = FALSE)
write.csv(wnv.trap.date,paste(repo.path,"data\\processed","wnv.trap.date.csv",sep='\\'),row.names=FALSE)






