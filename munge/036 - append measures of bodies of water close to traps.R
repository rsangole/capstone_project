
################################################################################
## 1. Take wnv trap data (geocoded points)
## 2. Open in ESRI ArcCatalog, perform buffers to 0.5, 1, 2, 5 miles
## 3. Download 2018 TIGER LINE area water files from Census for Cook County IL
## 4. In ESRI ArcCatalog, perform intersect between each buffered trap file
##    and the area water file
## 5. Create a blank map in ESRI ArcMap.  Open layers --> Properties --> 
##    Coordinate System and select a Projected Coordinate System such as 
##    UTM --> NAD 1983 --> NAD 1983 UTM Zone 16N
## 6. When prompted, set up Transformations from other projects as appropriate
## 7. Add shapefiles (buffered points intersected with area water)
## 8. For each shapefile, right click, open attribute table, add a new field, 
##    giving it an appropriate name (areawater) and data type (double 15.2)
## 9. Right click on the new field in the attribute table, and select 
##    Calculate Geometry.
## 10. Now we can import the resulting tabular data to R and aggregate to each
##    trap. This will give us the sum of 2D areas of every body of water 
##    within a given distance of the trap.

repo.path <- "D:\\ajc188\\GitHub\\capstone_project"
wnv.src <- paste(repo.path,"data","raw","chi_dept_public_health",'GIS',sep="\\")

areawater_halfmi <- read.dbf(paste(wnv.src,'wnv_traps_areawater_halfmi.dbf',sep="\\")) %>%
  group_by(trap_name) %>% summarise(areawater_sqft_halfmi = sum(areawater))

areawater_1mi <- read.dbf(paste(wnv.src,'wnv_traps_areawater_1mi.dbf',sep="\\")) %>%
  group_by(trap_name) %>% summarise(areawater_sqft_1mi = sum(areawater))

areawater_2mi <- read.dbf(paste(wnv.src,'wnv_traps_areawater_2mi.dbf',sep="\\")) %>%
  group_by(trap_name) %>% summarise(areawater_sqft_2mi = sum(areawater))

areawater_5mi <- read.dbf(paste(wnv.src,'wnv_traps_areawater_5mi.dbf',sep="\\")) %>%
  group_by(trap_name) %>% summarise(areawater_sqft_5mi = sum(areawater))


################################################################################
## Append to wnv dataset


df_rev <- df_rev %>% 
  left_join(.,areawater_halfmi,by=c("trap_trap_name"="trap_name")) %>% 
  left_join(.,areawater_1mi,by=c("trap_trap_name"="trap_name")) %>% 
  left_join(.,areawater_2mi,by=c("trap_trap_name"="trap_name")) %>% 
  left_join(.,areawater_5mi,by=c("trap_trap_name"="trap_name")) %>% 
  mutate(areawater_sqft_halfmi = coalesce(areawater_sqft_halfmi,0)
         ,areawater_sqft_1mi = coalesce(areawater_sqft_1mi,0)
         ,areawater_sqft_2mi = coalesce(areawater_sqft_2mi,0)
         ,areawater_sqft_5mi = coalesce(areawater_sqft_5mi,0)
  ) %>% 
  mutate(areawater_sqmi_halfmi = areawater_sqft_halfmi/(5280^2)
         ,areawater_sqmi_1mi = areawater_sqft_1mi/(5280^2)
         ,areawater_sqmi_2mi = areawater_sqft_2mi/(5280^2)
         ,areawater_sqmi_5mi = areawater_sqft_5mi/(5280^2)
  ) %>% 
  mutate(areawater_pct_halfmi = areawater_sqmi_halfmi/(pi*0.5^2)
         ,areawater_pct_1mi = areawater_sqmi_1mi/(pi*1^2)
         ,areawater_pct_2mi = areawater_sqmi_2mi/(pi*2^2)
         ,areawater_pct_5mi = areawater_sqmi_5mi/(pi*5^2)
  ) %>% 
  mutate(sqrt_areawater_pct_halfmi = areawater_pct_halfmi^0.5
         ,sqrt_areawater_pct_1mi = areawater_pct_1mi^0.5
         ,sqrt_areawater_pct_2mi = areawater_pct_2mi^0.5
         ,sqrt_areawater_pct_5mi = areawater_pct_5mi^0.5
  )


################################################################################
## Brief EDA

summary(df_rev %>% dplyr::select(matches('areawater_sqmi')))
summary(df_rev %>% dplyr::select(matches('areawater_pct')))

sapply(df_rev %>% dplyr::select(matches('areawater_sqmi')),hist)

sapply(df_rev %>% dplyr::select(matches('areawater_pct')),hist)

sapply(df_rev %>% dplyr::select(matches('sqrt_areawater_pct')),hist)

summary(df_rev %>% dplyr::select(matches('sqrt_areawater_pct')))

hist(df_rev$areawater_pct_5mi)
hist(df_rev$sqrt_areawater_pct_5mi)

## Anything close to Lake Michigan has a high proportion of water nearby.
## Mosquito *bites* (not necessarily mosquito growth) are associated with 
## droughts.  Being near a large body of water means fewer mosquito bites.
## We can test whether this is true of mosquito presence in general as well.
## Much of this will already be teased out of the data by simply using 
## lat/lng in a sophisticated method, but adding water data directly may 
## improve upon that.  







