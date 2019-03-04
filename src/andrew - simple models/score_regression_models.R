

score.model <- function(model,df=reg_df_results_no_wea,plot.path=
                            paste(paste(base.path,"images",sep="\\"))) {
  df <- df[df$part_train | df$part_validate,]

  df$pred <- predict(model, df)
  df$err <- df$pred - df$mos_tot_num_mosquitos
  df$se <- df$err ^ 2



  metrics <- data.frame(
    model.name = deparse(substitute(model))
    ,train.mse = sum(df[df$part_train,]$se)/dim(df[df$part_train,])[1]
    ,validate.mse = sum(df[df$part_validate,]$se)/dim(df[df$part_validate,])[1]
  )
  metrics$train.rmse <-  metrics$train.mse ^ 0.5
  metrics$validate.rmse <-  metrics$validate.mse ^ 0.5
  # print(metrics)

  plot <- ggplot(data=df
                 ,aes(x=t_date,y=mos_tot_num_mosquitos)
                 ,ggtheme = "fresh") +
    geom_line() +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 years", date_labels = "%y") +
    facet_wrap(~t_yr,ncol=3,scales='free_x') +
    geom_line(aes(x=t_date,y=pred,color=part_partition),color='red') +
    ggtitle(paste('Predictions - ',deparse(substitute(model)),sep=''))
  print(plot)
  ggsave(paste(plot.path,'Regression problem',paste("ts__",deparse(substitute(model)),".png",sep=''),sep='\\'),width=12,height=8)

  return(metrics)
}

# score.model(rf_rev2b)

# ftable(reg_df_results_no_wea$t_yr,reg_df_results_no_wea$part_partition)


