
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
## Reshape into wide format and see what variability looks like

SES.blkgrp.yr <- SES.blkgrp %>% group_by(geoid) %>%
  dplyr::select(c("LT_HS_pct","LT_Pov_pct","median_HHInc","year_end")) %>%
  gather(variable,value,-c(geoid,year_end)) %>%
  unite(variable_year, variable, year_end) %>%
  spread(variable_year,value)

summary(SES.blkgrp.yr %>% ungroup() %>% dplyr::select(contains("LT_HS_pct")))
# kdepairs(SES.blkgrp.yr %>% ungroup() %>% dplyr::select(contains("LT_HS_pct")))
educ.yr <- SES.blkgrp.yr %>% ungroup() %>% 
  dplyr::select(contains("LT_HS_pct")) %>%
  setNames(gsub("LT_HS_pct_",'',names(.))) %>%
  .[,-(1:3)]
png(paste(plot.path,'Low education by year (block group, pct below high school educ), kernel density plot.PNG',sep='\\'))
kdepairs(educ.yr[rowSums(is.na(educ.yr)) == 0,]) + title('LT_HS_pct')
dev.off()

income.yr <- SES.blkgrp.yr %>% ungroup() %>% 
  dplyr::select(contains("median_HHInc")) %>%
  setNames(gsub("median_HHInc_",'',names(.)))
summary(income.yr)
kdepairs(income.yr[rowSums(is.na(income.yr)) == 0,])
png(paste(plot.path,'Median household income by year (block group), kernel density plot.PNG',sep='\\'))
kdepairs(poverty.yr[rowSums(is.na(income.yr)) == 0,]) + title('median_HHInc')
dev.off()


poverty.yr <- SES.blkgrp.yr %>% ungroup() %>% 
  dplyr::select(contains("LT_Pov_pct")) %>%
  setNames(gsub("LT_Pov_pct_",'',names(.)))
summary(poverty.yr,title='LT_Pov_pct')
png(paste(plot.path,'Poverty by year (block group, pct below FPL), kernel density plot.PNG',sep='\\'))
kdepairs(poverty.yr[rowSums(is.na(poverty.yr)) == 0,]) + title('LT_Pov_pct')
dev.off()

# ?kdepairs 

rm(list=c("educ.yr","income.yr","poverty.yr"))



################################################################################
## Pull the year specific value if available
## But since there is always a lag of 1-2 years, we should do the same.
## We also have to deal with missing data in general, and especially for 
## education 2007-11.  It turns out education was not available from ACS
## prior to that for these geographies at least.  

lm(`2017`~.,data=poverty.yr)
lm(`2009`~.,data=poverty.yr)
lm(LT_Pov_pct~year,SES.blkgrp)
plot(lm(LT_Pov_pct~year,SES.blkgrp))

require(lme4)
mixed.model <- lme4::lmer(LT_Pov_pct ~ year + (1|geoid),data = SES.blkgrp)
plot(mixed.model)
summary(mixed.model)

SES.blkgrp$year.2011 <- as.integer(SES.blkgrp$year == 2011)
SES.blkgrp$year.2012 <- as.integer(SES.blkgrp$year == 2012)
SES.blkgrp$year.2013 <- as.integer(SES.blkgrp$year == 2013)
SES.blkgrp$year.2014 <- as.integer(SES.blkgrp$year == 2014)
SES.blkgrp$year.2015 <- as.integer(SES.blkgrp$year == 2015)
SES.blkgrp$year.2016 <- as.integer(SES.blkgrp$year == 2016)
SES.blkgrp$year.2017 <- as.integer(SES.blkgrp$year == 2017)

lm(LT_Pov_pct~year.2012 + year.2013 + year.2014 + year.2015 + year.2016 +
     year.2017,data=SES.blkgrp)


SES.blkgrp %>% group_by(year_end,year_start) %>% summarise(n=n(),n.blkgrps = n_distinct(geoid))
SES.tract %>% group_by(year_end,year_start) %>% summarise(n=n(),n.blkgrps = n_distinct(geoid))

ref.dates %>% group_by(yr) %>% summarise(n=n())

SES.blkgrp.yr <- SES.blkgrp %>% group_by(geoid) %>%
  dplyr::select(c("LT_HS_pct","LT_Pov_pct","median_HHInc","year_end")) 
  # %>%
  # gather(variable,value,-c(geoid,year_end)) %>%
  # unite(variable_year, variable, year_end) %>%
  # spread(variable_year,value)



df_rev2 <- df_rev %>% 



################################################################################
## Save updated trap data
################################################################################

wnv.traps <- wnv.ses2

save(wnv.traps, file="wnv.traps.RData", compress = FALSE)
write.csv(wnv.traps, paste(src.path,'wnv.traps.csv',sep='\\'),row.names = FALSE)



################################################################################
## Clean up more unnecessary objects
################################################################################








