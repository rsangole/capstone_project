

################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
repo.path <- "D:\\ajc188\\github\\capstone_project"

filename.list <- list.files(path = paste(repo.path,"data\\raw\\hospitals",sep='\\'))           
filename.list



################################################################################
## Load hospital data
################################################################################

hospitals.csv <- "HospitalPoint.csv"

hospitals <- read.csv(paste(repo.path,"data\\raw\\hospitals",hospitals.csv,sep="\\"),stringsAsFactors = FALSE)

# str(hospitals)



################################################################################
## Convert coordinates to a shapefile in ESRI ArcGIS then to a layer file
## then perform spatial join (intersection) with block group shapefile, then 
## convert resulting shapefile's tabular component (DBF) to CSV.  Finally, 
## read back into R.  
## Repeat for Chicago community area and Zip Code Tabulation Area (ZCTA).
################################################################################

hospitals.zcta <- read.dbf(paste(repo.path,'data\\raw\\hospitals','HospitalPoint_zcta.dbf',sep="\\"))
hospitals.blkgrp <- read.dbf(paste(repo.path,'data\\raw\\hospitals','HospitalPoint_bg2018.dbf',sep="\\"))
hospitals.community.area <- read.dbf(paste(repo.path,'data\\raw\\hospitals','HospitalPoint_CommArea.dbf',sep="\\"))

hospitals.1 <- left_join(hospitals,hospitals.zcta[,c("CFNAME","POINT_X","POINT_Y","ZCTA5CE10")]
                        ,by=c("CFNAME","POINT_X","POINT_Y"))
hospitals.2 <- left_join(hospitals.1,hospitals.blkgrp[,c("CFNAME","POINT_X","POINT_Y"
                                                         ,"STATEFP"
                                                      ,"COUNTYFP"
                                                      ,"TRACTCE"
                                                      ,"BLKGRPCE"
                                                      ,"GEOID"
                                                      ,"INTPTLAT","INTPTLON")]
                        ,by=c("CFNAME","POINT_X","POINT_Y"))
hospitals.3 <- left_join(hospitals.2,hospitals.community.area[,c("CFNAME","POINT_X","POINT_Y","communit_1")]
                        ,by=c("CFNAME","POINT_X","POINT_Y"))
names(hospitals.3)[names(hospitals.3) == 'communit_1'] <- 'community'
# names(hospitals.3)[names(hospitals.3) == 'POINT_X'] <- 'LONGITUDE'
# names(hospitals.3)[names(hospitals.3) == 'POINT_Y'] <- 'LATITUDE'
names(hospitals.3)[names(hospitals.3) == 'INTPTLON'] <- 'LONGITUDE'
names(hospitals.3)[names(hospitals.3) == 'INTPTLAT'] <- 'LATITUDE'


summary(hospitals.3)
hospitals <- hospitals.3


################################################################################
## Save updated geographic file
################################################################################

write.csv(hospitals,paste(repo.path,"data\\raw\\hospitals","hospitals.csv",sep='\\'),row.names=FALSE)



################################################################################
## Clean up more unnecessary objects
################################################################################








