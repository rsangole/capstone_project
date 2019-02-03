


# wnv.trap.date2 <- left_join(ref.dates
#                             ,wnv.trap.date
#                             ,by=c("date"))

# wnv.trap.date %>% summarise(nrows=n(),dates=n_distinct(date))
# wnv.trap.date2 %>% summarise(nrows=n(),dates=n_distinct(date))


# str(wnv.traps)
wnv.trap.date2a <- inner_join(
  cbind(ref.dates,dummy=1)
  ,cbind(wnv.traps,dummy=1)
  ,by=c("dummy"))
wnv.trap.date2b <- left_join(wnv.trap.date2a[,!colnames(wnv.trap.date2a) %in% c("rev.lat","rev.lng")]
                             ,wnv.trap.date[,!colnames(wnv.trap.date) %in% 
                                              c("yr",'mo','wk','day.of.wk'
                                                ,'day.of.wk.name'
                                                ,'satellite.ind'
                                                ,'lat.lng.src')]
                             ,by=c("trap.name","date"))

wnv.trap.date2a %>% summarise(nrows=n(),dates=n_distinct(date),traps=n_distinct(trap.name))
wnv.trap.date2b %>% summarise(nrows=n(),dates=n_distinct(date),traps=n_distinct(trap.name))


wnv.trap.date2b <- wnv.trap.date2b[,!colnames(wnv.trap.date2b) %in% c("dummy")]


summary(wnv.trap.date2b)

wnv.trap.date2c <- left_join(wnv.trap.date2b
                              ,)






