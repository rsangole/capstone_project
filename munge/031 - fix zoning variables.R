
################################################################################
## Load shared CSV file 

base.path <- "D:\\ajc188\\github\\capstone_project"
plot.path <- paste("D:\\ajc188\\github\\capstone_project","images",sep='\\')


df <- read.csv(paste(base.path,'data','processed','wnv.trap.date.rev3b.csv',sep='\\')
               ,stringsAsFactors = FALSE)

df_train <- df[df$part_train,]
df_validate <- df[df$part_validate,]
df_test <- df[df$part_test,]

df$t_date <- as.Date(df$t_date)
# df_rev$t_date <- as.Date(df_rev$t_date)



################################################################################
## Add labels for zoning data


# ftable(df$zone_type)
# ftable(df$zone_class)
# ftable(df$zone_class,df$zone_type)


zone.types <- data.frame(id=c(1,2,3,4,5,6,12)
                         ,zone_type_abbrev=c("B","C","M","R","PD","PMD","POS")
                         ,zone_type_name=c("Business","Commercial","Manufacturing"
                                           ,"Residential","Planned Development"
                                           ,"Planned Manufacturing Districts"
                                           ,"Parks and Open Space"))



df_rev <- left_join(df,zone.types,by=c("zone_type" = "id"))



ftable(df_rev$zone_type,df_rev$zone_type_abbrev)
ftable(df_rev$zone_type,df_rev$zone_type_name)
ftable(df_rev$zone_type_abbrev,df_rev$zone_type_name)
ftable(df_rev$zone_class,df_rev$zone_type_name)





