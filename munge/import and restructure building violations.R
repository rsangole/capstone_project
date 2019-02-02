

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



str(violations2)

violations2b <- violations2[,colnames(violations2) %in% c(
  "ID","violation.date","LATITUDE","LONGITUDE","LOCATION"
  ,"ZCTA5CE10","STATEFP","COUNTYFP","TRACTCE","BLKGRPCE","GEOID","community"
  ,"DEPARTMENT.BUREAU","INSPECTION.CATEGORY","VIOLATION.STATUS"
)]

str(violations2b)

violations2c <- violations2[,colnames(violations2) %in% c(
  "ID","violation.date","LATITUDE","LONGITUDE"
  ,"ZCTA5CE10","GEOID","community"
  ,"DEPARTMENT.BUREAU","INSPECTION.CATEGORY","VIOLATION.STATUS"
)]


################################################################################
## Save CSV
################################################################################


write.csv(violations2c,paste(repo.path,"data\\raw\\building_violations","violations.csv",sep='\\'),row.names=FALSE)



################################################################################
## Clean up more unnecessary objects
################################################################################








