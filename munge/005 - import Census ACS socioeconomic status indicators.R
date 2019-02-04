
################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\github\\capstone_project"

my.path <- paste(base.path,"data","raw","processed",sep="\\")
src.path <- paste(base.path,"data","raw","census_acs",sep="\\")

plot.path <- paste(base.path,"images",sep="\\")
# submission.path <- paste(base.path,"submissions",sep="\\")
# output.path <- paste(base.path,"reports",sep="\\")

filename.list <- list.files(path = src.path)           
filename.list




################################################################################
## Load Census American Community Survey (ACS) tract- and block group-level
## socioeconomic status (SES) indicators extracted via 2009 to 2017 ACS 5-year
## summary files via FTP and a customized SAS program.
## These are year-specific 5-year estimates at different geographic levels.
## We can actually use a year of data specific to each WNV trap test date
## OR just use a selected year for all reporting purposes.  I haven't tested
## variability across years.  The earliest estimates available are for 
## 2005-09, and the first couple years don't have the full education summary
## tables available in later years.  
################################################################################

SES.blkgrp <- read.csv(paste(src.path,"SES_BlkGrp.csv",sep="\\"),stringsAsFactors = FALSE)
SES.tract <- read.csv(paste(src.path,"SES_tract.csv",sep="\\"),stringsAsFactors = FALSE)

# str(cdph.wnv)



################################################################################
## For now, just pull the most recent version and append it to WNV trap data.
################################################################################

names(wnv.traps)[names(wnv.traps) == 'GEOID'] <- 'BlkGrp.geoid'
wnv.traps$Tract.geoid <- as.numeric(substring(wnv.traps$BlkGrp.geoid,1,11))

# summary(SES.blkgrp)
SES.blkgrp.2017 <- SES.blkgrp[SES.blkgrp$year_end == 2017
                              ,c("geoid"
                                  ,"LT_HS_pct","median_HHInc","LT_Pov_pct")]
colnames(SES.blkgrp.2017) <- c("geoid"
                               ,"LT_HS_pct__BlkGrp2017"
                               ,"median_HHInc__BlkGrp2017"
                               ,"LT_Pov_pct__BlkGrp2017")
wnv.ses1 <- left_join(wnv.traps
                      ,SES.blkgrp.2017
                      ,by=c("BlkGrp.geoid"="geoid"))


SES.tract.2017 <- SES.tract[SES.tract$year_end == 2017
                              ,c("geoid"
                                 ,"LT_HS_pct","median_HHInc","LT_Pov_pct")]
colnames(SES.tract.2017) <- c("geoid"
                               ,"LT_HS_pct__Tract2017"
                               ,"median_HHInc__Tract2017"
                               ,"LT_Pov_pct__Tract2017")
wnv.ses2 <- left_join(wnv.ses1
                      ,SES.tract.2017
                      ,by=c("Tract.geoid"="geoid"))


################################################################################
## Save updated trap data
################################################################################

wnv.traps <- wnv.ses2

save(wnv.traps, file="wnv.traps.RData", compress = FALSE)
write.csv(wnv.traps, paste(src.path,'wnv.traps.csv',sep='\\'),row.names = FALSE)



################################################################################
## Clean up more unnecessary objects
################################################################################








