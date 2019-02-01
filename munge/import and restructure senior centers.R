

################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
repo.path <- "D:\\ajc188\\github\\capstone_project"

filename.list <- list.files(path = paste(repo.path,"data\\raw\\senior_centers",sep='\\'))           
filename.list



################################################################################
## Load senior.center data
################################################################################

senior.centers.csv <- "Senior_Centers.csv"

senior.centers <- read.csv(paste(repo.path,"data\\raw\\senior_centers",senior.centers.csv,sep="\\"),stringsAsFactors = FALSE)

# str(senior.centers)



################################################################################
## Convert coordinates to a shapefile in ESRI ArcGIS then to a layer file
## then perform spatial join (intersection) with block group shapefile, then 
## convert resulting shapefile's tabular component (DBF) to CSV.  Finally, 
## read back into R.  
## Repeat for Chicago community area and Zip Code Tabulation Area (ZCTA).
################################################################################

senior.centers.zcta <- read.dbf(paste(repo.path,'data\\raw\\senior_centers','Senior_Centers_zcta.dbf',sep="\\"))
senior.centers.blkgrp <- read.dbf(paste(repo.path,'data\\raw\\senior_centers','Senior_Centers_bg.dbf',sep="\\"))
senior.centers.community.area <- read.dbf(paste(repo.path,'data\\raw\\senior_centers','Senior_Centers_CommArea.dbf',sep="\\"))

senior.centers.1 <- left_join(senior.centers,senior.centers.zcta[,c("LOCATION","ZCTA5CE10")]
                        ,by=c("LOCATION"))
senior.centers.2 <- left_join(senior.centers.1,senior.centers.blkgrp[,c("LOCATION"
                                                         ,"STATEFP"
                                                      ,"COUNTYFP"
                                                      ,"TRACTCE"
                                                      ,"BLKGRPCE"
                                                      ,"GEOID"
                                                      ,"INTPTLAT","INTPTLON")]
                        ,by=c("LOCATION"))
senior.centers.3 <- left_join(senior.centers.2,senior.centers.community.area[,c("LOCATION","community")]
                        ,by=c("LOCATION"))

names(senior.centers.3)[names(senior.centers.3) == 'INTPTLON'] <- 'LONGITUDE'
names(senior.centers.3)[names(senior.centers.3) == 'INTPTLAT'] <- 'LATITUDE'


summary(senior.centers.3)
senior.centers <- senior.centers.3


################################################################################
## Save updated geographic file
################################################################################

write.csv(senior.centers,paste(repo.path,"data\\raw\\senior_centers","senior.centers.csv",sep='\\'),row.names=FALSE)



################################################################################
## Clean up more unnecessary objects
################################################################################








