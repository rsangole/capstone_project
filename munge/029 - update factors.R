
ftable(wnv.trap.date.rev3b$zone_type)
ftable(wnv.trap.date.rev3b$zone_class)
ftable(wnv.trap.date.rev3b$zone_class,wnv.trap.date.rev3b$zone_type)
zone.types <- data.frame(id=c(1,2,3,4,5,6,12)
                         ,zone.type.abbrev=c("B","C","M","R","PD","PMD","POS")
                         ,zone.type.name=c("Business","Commercial","Manufacturing"
                                           ,"Residential","Planned Development"
                                           ,"Planned Manufacturing Districts"
                                           ,"Parks and Open Space"))
# wnv.trap.date.rev3b$zone_type <- as.integer(wnv.trap.date.rev3b$zone_type)
# wnv.trap.date.rev3b[wnv.trap.date.rev3b$zone_type == 7,]$zone_type <- 12
wnv.trap.date.rev3b <- left_join(wnv.trap.date.rev3b
                                  ,zone.types
                                  ,by=c("zone_type"="id"))
ftable(wnv.trap.date.rev3b$zone_type,wnv.trap.date.rev3b$zone.type.abbrev)
ftable(wnv.trap.date.rev3b$zone_type,wnv.trap.date.rev3b$zone.type.name)
ftable(wnv.trap.date.rev3b$zone.type.abbrev,wnv.trap.date.rev3b$zone.type.name)
ftable(wnv.trap.date.rev3b$zone_class,wnv.trap.date.rev3b$zone.type.name)
wnv.trap.date.rev3b$day.of.wk.name <- factor(wnv.trap.date.rev3b$day.of.wk.name
                                             ,levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))



