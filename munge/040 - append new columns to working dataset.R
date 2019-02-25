
################################################################################

# wnv.trap.date.rev3b <- wnv.trap.date.rev3b %>%
#   dplyr::select(-c(zone.type.name,zone.type.abbrev))

# colnames(wnv.trap.date.rev3b)


################################################################################
## Start where we left off

df_results <- wnv.trap.date.rev3b %>%
  rename(
    t_date = date,
    t_yr = yr,
    t_mo = mo,
    t_day = day,
    t_qtr = qtr,
    t_wk = wk,
    t_day.of.yr = day.of.yr,
    t_day.of.wk = day.of.wk,
    t_day.of.wk.name = day.of.wk.name,
    t_eval.day = eval.day,
    t_eval.wk = eval.wk,
    part_train = train,
    part_validate = validate,
    part_test = test,
    part_partition = partition,
    trap_trap.name = trap.name,
    loc_lat = lat,
    loc_lng = lng,
    loc_lat.lng.src = lat.lng.src,
    trap_satellite.ind = satellite.ind,
    loc_zipcode = ZCTA5CE10,
    loc_census_block_group_id = BlkGrp.geoid,
    loc_census_tract_id = Tract.geoid,
    loc_community = community,
    zone_class = zone_class,
    zone_type = zone_type,
    ses_LT_HS_pct__BlkGrp2017 = LT_HS_pct__BlkGrp2017,
    ses_median_HHInc__BlkGrp2017 = median_HHInc__BlkGrp2017,
    ses_LT_Pov_pct__BlkGrp2017 = LT_Pov_pct__BlkGrp2017,
    ses_LT_HS_pct__Tract2017 = LT_HS_pct__Tract2017,
    ses_median_HHInc__Tract2017 = median_HHInc__Tract2017,
    ses_LT_Pov_pct__Tract2017 = LT_Pov_pct__Tract2017,
    trap_trap_type = trap_type,
    mos_erraticus.NumMosquitos = erraticus.NumMosquitos,
    mos_pipiens.NumMosquitos = pipiens.NumMosquitos,
    mos_pipiens_restuans.NumMosquitos = pipiens_restuans.NumMosquitos,
    mos_restuans.NumMosquitos = restuans.NumMosquitos,
    mos_salinarius.NumMosquitos = salinarius.NumMosquitos,
    mos_tarsalis.NumMosquitos = tarsalis.NumMosquitos,
    mos_territans.NumMosquitos = territans.NumMosquitos,
    mos_unspecified.NumMosquitos = unspecified.NumMosquitos,
    mos_tot.NumMosquitos = tot.NumMosquitos,
    mos_erraticus.WnvPresent = erraticus.WnvPresent,
    mos_pipiens.WnvPresent = pipiens.WnvPresent,
    mos_pipiens_restuans.WnvPresent = pipiens_restuans.WnvPresent,
    mos_restuans.WnvPresent = restuans.WnvPresent,
    mos_salinarius.WnvPresent = salinarius.WnvPresent,
    mos_tarsalis.WnvPresent = tarsalis.WnvPresent,
    mos_territans.WnvPresent = territans.WnvPresent,
    mos_unspecified.WnvPresent = unspecified.WnvPresent,
    mos_any.WnvPresent = any.WnvPresent,
    nbrhud_comm.180d.violation.cnt = comm.180d.violation.cnt,
    nbrhud_BlkGrp.180d.violation.cnt = BlkGrp.180d.violation.cnt,
    nbrhud_zipcode.180d.violation.cnt = zcta.180d.violation.cnt,
    nbrhud_comm.180d.vacancies.cnt = comm.180d.vacancies.cnt,
    nbrhud_BlkGrp.180d.vacancies.cnt = BlkGrp.180d.vacancies.cnt,
    nbrhud_zipcode.180d.vacancies.cnt = zcta.180d.vacancies.cnt,
    wea_midway_PRCP = USW00014819_PRCP,
    wea_midway_tavg2 = USW00014819_tavg2,
    wea_midway_TMAX = USW00014819_TMAX,
    wea_midway_TMIN = USW00014819_TMIN,
    wea_ohare_PRCP = USW00094846_PRCP,
    wea_ohare_tavg2 = USW00094846_tavg2,
    wea_ohare_TMAX = USW00094846_TMAX,
    wea_ohare_TMIN = USW00094846_TMIN,
    wea_midway_tavg2.ma7 = USW00014819_tavg2.ma7,
    wea_ohare_tavg2.ma7 = USW00094846_tavg2.ma7,
    wea_midway_tavg2.ma60 = USW00014819_tavg2.ma60,
    wea_ohare_tavg2.ma60 = USW00094846_tavg2.ma60,
    wea_midway_PRCP.sum7 = USW00014819_PRCP.sum7,
    wea_ohare_PRCP.sum7 = USW00094846_PRCP.sum7,
    wea_midway_PRCP.sum60 = USW00014819_PRCP.sum60,
    wea_ohare_PRCP.sum60 = USW00094846_PRCP.sum60,
    wea_midway_PRCP.sum60.lag7 = USW00014819_PRCP.sum60.lag7,
    wea_midway_PRCP.sum60.lag14 = USW00014819_PRCP.sum60.lag14,
    wea_midway_PRCP.sum60.lag21 = USW00014819_PRCP.sum60.lag21,
    wea_midway_PRCP.sum60.lag28 = USW00014819_PRCP.sum60.lag28,
    wea_midway_PRCP.sum60.lag35 = USW00014819_PRCP.sum60.lag35,
    wea_midway_PRCP.sum60.lag42 = USW00014819_PRCP.sum60.lag42,
    wea_midway_PRCP.sum60.lag49 = USW00014819_PRCP.sum60.lag49,
    wea_midway_PRCP.sum60.lag56 = USW00014819_PRCP.sum60.lag56,
    wea_midway_PRCP.sum60.lag63 = USW00014819_PRCP.sum60.lag63,
    wea_midway_PRCP.sum60.lag90 = USW00014819_PRCP.sum60.lag90,
    wea_midway_PRCP.sum60.lag120 = USW00014819_PRCP.sum60.lag120,
    wea_midway_tavg2.ma60.lag7 = USW00014819_tavg2.ma60.lag7,
    wea_midway_tavg2.ma60.lag14 = USW00014819_tavg2.ma60.lag14,
    wea_midway_tavg2.ma60.lag21 = USW00014819_tavg2.ma60.lag21,
    wea_midway_tavg2.ma60.lag28 = USW00014819_tavg2.ma60.lag28,
    wea_midway_tavg2.ma60.lag35 = USW00014819_tavg2.ma60.lag35,
    wea_midway_tavg2.ma60.lag42 = USW00014819_tavg2.ma60.lag42,
    wea_midway_tavg2.ma60.lag49 = USW00014819_tavg2.ma60.lag49,
    wea_midway_tavg2.ma60.lag56 = USW00014819_tavg2.ma60.lag56,
    wea_midway_tavg2.ma60.lag63 = USW00014819_tavg2.ma60.lag63,
    wea_midway_tavg2.ma60.lag90 = USW00014819_tavg2.ma60.lag90,
    wea_midway_tavg2.ma60.lag120 = USW00014819_tavg2.ma60.lag120,
    wea_ohare_PRCP.sum60.lag7 = USW00094846_PRCP.sum60.lag7,
    wea_ohare_PRCP.sum60.lag14 = USW00094846_PRCP.sum60.lag14,
    wea_ohare_PRCP.sum60.lag21 = USW00094846_PRCP.sum60.lag21,
    wea_ohare_PRCP.sum60.lag28 = USW00094846_PRCP.sum60.lag28,
    wea_ohare_PRCP.sum60.lag35 = USW00094846_PRCP.sum60.lag35,
    wea_ohare_PRCP.sum60.lag42 = USW00094846_PRCP.sum60.lag42,
    wea_ohare_PRCP.sum60.lag49 = USW00094846_PRCP.sum60.lag49,
    wea_ohare_PRCP.sum60.lag56 = USW00094846_PRCP.sum60.lag56,
    wea_ohare_PRCP.sum60.lag63 = USW00094846_PRCP.sum60.lag63,
    wea_ohare_PRCP.sum60.lag90 = USW00094846_PRCP.sum60.lag90,
    wea_ohare_PRCP.sum60.lag120 = USW00094846_PRCP.sum60.lag120,
    wea_ohare_tavg2.ma60.lag7 = USW00094846_tavg2.ma60.lag7,
    wea_ohare_tavg2.ma60.lag14 = USW00094846_tavg2.ma60.lag14,
    wea_ohare_tavg2.ma60.lag21 = USW00094846_tavg2.ma60.lag21,
    wea_ohare_tavg2.ma60.lag28 = USW00094846_tavg2.ma60.lag28,
    wea_ohare_tavg2.ma60.lag35 = USW00094846_tavg2.ma60.lag35,
    wea_ohare_tavg2.ma60.lag42 = USW00094846_tavg2.ma60.lag42,
    wea_ohare_tavg2.ma60.lag49 = USW00094846_tavg2.ma60.lag49,
    wea_ohare_tavg2.ma60.lag56 = USW00094846_tavg2.ma60.lag56,
    wea_ohare_tavg2.ma60.lag63 = USW00094846_tavg2.ma60.lag63,
    wea_ohare_tavg2.ma60.lag90 = USW00094846_tavg2.ma60.lag90,
    wea_ohare_tavg2.ma60.lag120 = USW00094846_tavg2.ma60.lag120
  ) %>%
  janitor::clean_names()



################################################################################
## Add labels for zone types

zone.types <- data.frame(id=c(1,2,3,4,5,6,12)
                         ,zone_type_abbrev=c("B","C","M","R","PD","PMD","POS")
                         ,zone_type_name=c("Business","Commercial","Manufacturing"
                                           ,"Residential","Planned Development"
                                           ,"Planned Manufacturing Districts"
                                           ,"Parks and Open Space"))
df_results <- left_join(df_results
                                 ,zone.types
                                 ,by=c("zone_type"="id")) %>%
  mutate(zone_type = as.factor(zone_type)
         ,zone_type_abbrev = as.factor(zone_type_abbrev)
         ,zone_type_name = as.factor(zone_type_name))
# summary(df_results %>% dplyr::select(contains('zone_type')))
# summary(df_results %>% dplyr::select(contains('zone')))


################################################################################
## Append goog trends

wnv.trends.wide2 <- wnv.trends.wide %>% 
  rename(googtrend_sym_wnv = goog_symptoms_of_west_nile_virus
         ,googtrend_deadbirds = goog_dead_birds
         ,googtrend_westnile = goog_west_nile
         ,googtrend_mosq_bites = goog_mosquito_bites) %>%
  janitor::clean_names() %>%
  mutate(googtrend_sym_wnv_lag1 = lag(googtrend_sym_wnv)
         ,googtrend_deadbirds_lag1 = lag(googtrend_deadbirds)
         ,googtrend_westnile_lag1 = lag(googtrend_westnile)
         ,googtrend_mosq_bites_lag1 = lag(googtrend_mosq_bites)
  )
  

df_results <- df_results %>%
  left_join(.,wnv.trends.wide2,by=c("t_yr"="yr","t_mo"="mo")) %>%
  dplyr::select(-month)

# summary(wnv.trap.date.rev4b %>% dplyr::select(matches('goog')))
  
  



################################################################################
## Check data type for day of week name

# summary(df_results$t_day_of_wk_name)
# factor(df_results$t_day_of_wk_name,levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
# summary(factor(df_results$t_day_of_wk_name,levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")))



################################################################################
## Append measures of bodies of water close to traps
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


df_results <- df_results %>% 
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

# summary(df_rev %>% dplyr::select(matches('areawater_sqmi')))
# summary(df_rev %>% dplyr::select(matches('areawater_pct')))
# 
# sapply(df_rev %>% dplyr::select(matches('areawater_sqmi')),hist)
# 
# sapply(df_rev %>% dplyr::select(matches('areawater_pct')),hist)
# 
# sapply(df_rev %>% dplyr::select(matches('sqrt_areawater_pct')),hist)
# 
# summary(df_rev %>% dplyr::select(matches('sqrt_areawater_pct')))
# 
# hist(df_rev$areawater_pct_5mi)
# hist(df_rev$sqrt_areawater_pct_5mi)




