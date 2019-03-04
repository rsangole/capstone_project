

wnv.traps %>% 
  group_by(trap.name,lat,lng) %>%
  summarise(n=n() 
            ,zcta.cnt = n_distinct(ZCTA5CE10)
            ,state.cnt = n_distinct(STATEFP)
            ,countyfp.cnt = n_distinct(COUNTYFP)
            ,tract.cnt = n_distinct(TRACTCE)
            ,tract.cnt2 = n_distinct(substring(GEOID,1,11))
            ,blkgrp.cnt = n_distinct(GEOID)
            ,community.cnt = n_distinct(community)
            )

wnv.traps %>% 
  summarise(n=n() 
            ,zcta.cnt = n_distinct(ZCTA5CE10)
            ,state.cnt = n_distinct(STATEFP)
,countyfp.cnt = n_distinct(COUNTYFP)
,tract.cnt = n_distinct(TRACTCE)
,tract.cnt2 = n_distinct(substring(GEOID,1,11))
,blkgrp.cnt = n_distinct(GEOID)
,community.cnt = n_distinct(community)
)



df %>% group_by(part_partition) %>%
  summarise(nrows=n(),min.date = min(t_date),max.date=max(t_date)
            ,mean.mos = mean(mos_tot_num_mosquitos)
            ,mean.wnv = mean(mos_any_wnv_present))

                                                                                                                                                                             
