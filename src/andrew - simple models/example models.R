
################################################################################
## Try a simple model 
## Linear model
################################################################################

colnames(wnv.trap.date.rev3b)
paste(colnames(wnv.trap.date.rev3b),collapse=' + ')
summary(wnv.trap.date.rev3b)


lm1 <- lm(tot.NumMosquitos~date+lat+lng
          ,data=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,])
summary(lm1)

lm2 <- lm(tot.NumMosquitos~date+lat+lng+lat:lng
          ,data=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,])
summary(lm2)

lm3 <- lm(tot.NumMosquitos~date+lat+lng+lat:lng+
            USW00014819_tavg2+USW00014819_PRCP+
            USW00094846_tavg2+USW00094846_PRCP
          ,data=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,])
summary(lm3)



################################################################################
## Random forest
################################################################################

set.seed(85283)
rf1 <- randomForest(tot.NumMosquitos~date+lat+lng+
                      USW00014819_tavg2+USW00014819_PRCP+
                      USW00094846_tavg2+USW00094846_PRCP
                    ,data=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,])
varImpPlot(rf1)
summary(rf1)



rf2 <- randomForest(tot.NumMosquitos~date + yr + mo + wk + day.of.yr + day.of.wk 
                    + eval.wk + lat + lng + satellite.ind + ZCTA5CE10 
                    + zone_class + zone_type 
                    + LT_HS_pct__BlkGrp2017 + median_HHInc__BlkGrp2017 + LT_Pov_pct__BlkGrp2017 
                    + LT_HS_pct__Tract2017 + median_HHInc__Tract2017 + LT_Pov_pct__Tract2017 
                    # + trap_type 
                    + comm.180d.violation.cnt + BlkGrp.180d.violation.cnt 
                    + zcta.180d.violation.cnt 
                    + comm.180d.vacancies.cnt + BlkGrp.180d.vacancies.cnt 
                    + zcta.180d.vacancies.cnt 
                    + USW00014819_PRCP + USW00014819_tavg2 + USW00014819_TMAX 
                    + USW00014819_TMIN + USW00094846_PRCP 
                    + USW00094846_tavg2 + USW00094846_TMAX + USW00094846_TMIN 
                    + USW00014819_tavg2.ma7 + USW00094846_tavg2.ma7 + USW00014819_tavg2.ma60 
                    + USW00094846_tavg2.ma60 + USW00014819_PRCP.sum7 + USW00094846_PRCP.sum7 
                    + USW00014819_PRCP.sum60 + USW00094846_PRCP.sum60 
                    + USW00014819_PRCP.sum60.lag7 + USW00014819_PRCP.sum60.lag14 
                    + USW00014819_PRCP.sum60.lag21 + USW00014819_PRCP.sum60.lag28 
                    + USW00014819_PRCP.sum60.lag35 + USW00014819_PRCP.sum60.lag42 
                    + USW00014819_PRCP.sum60.lag49 + USW00014819_PRCP.sum60.lag56 
                    + USW00014819_PRCP.sum60.lag63 + USW00014819_PRCP.sum60.lag90 
                    + USW00014819_PRCP.sum60.lag120 + USW00014819_tavg2.ma60.lag7 
                    + USW00014819_tavg2.ma60.lag14 + USW00014819_tavg2.ma60.lag21 
                    + USW00014819_tavg2.ma60.lag28 + USW00014819_tavg2.ma60.lag35 
                    + USW00014819_tavg2.ma60.lag42 + USW00014819_tavg2.ma60.lag49 
                    + USW00014819_tavg2.ma60.lag56 + USW00014819_tavg2.ma60.lag63 
                    + USW00014819_tavg2.ma60.lag90 + USW00014819_tavg2.ma60.lag120 
                    + USW00094846_PRCP.sum60.lag7 + USW00094846_PRCP.sum60.lag14 
                    + USW00094846_PRCP.sum60.lag21 + USW00094846_PRCP.sum60.lag28 
                    + USW00094846_PRCP.sum60.lag35 + USW00094846_PRCP.sum60.lag42 
                    + USW00094846_PRCP.sum60.lag49 + USW00094846_PRCP.sum60.lag56 
                    + USW00094846_PRCP.sum60.lag63 + USW00094846_PRCP.sum60.lag90 
                    + USW00094846_PRCP.sum60.lag120 + USW00094846_tavg2.ma60.lag7 
                    + USW00094846_tavg2.ma60.lag14 + USW00094846_tavg2.ma60.lag21 
                    + USW00094846_tavg2.ma60.lag28 + USW00094846_tavg2.ma60.lag35 
                    + USW00094846_tavg2.ma60.lag42 + USW00094846_tavg2.ma60.lag49 
                    + USW00094846_tavg2.ma60.lag56 + USW00094846_tavg2.ma60.lag63 
                    + USW00094846_tavg2.ma60.lag90 + USW00094846_tavg2.ma60.lag120
                    ,data=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,])
varImpPlot(rf2)
summary(rf2)





################################################################################
## Need to pull in our code for assessing models out of sample
################################################################################




