
# "Color blind palette"
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



plot.point <- function(cols,metric=tot.NumMosquitos
                            ,df=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,]) {
  df.plot <- df %>%
    group_by(lat,lng,!!! sym(cols)) %>%
    summarise(tot.NumMosquitos = sum(tot.NumMosquitos)
              ,pct.WnvPresent = sum(any.WnvPresent)/n()
              ,WnvPresent.Mosquitos = sum(tot.NumMosquitos) * 
                sum(any.WnvPresent)/n()
    ) 
  
  plots <- ggplot(aes(lng,lat),data = as.data.frame(df.plot)) +
    geom_jitter(width = 0.01, height = 0.01, alpha = 0.4, stroke = 1
                ,aes(colour=(!!! sym(cols))
                     # ,shape=(!!! sym(cols))
                     ,size=(!!! sym(metric))
                )) +
    coord_equal() + 
    scale_colour_manual(values=cbPalette) +
    # theme_linedraw() +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) 
  
  plot(plots)
  
  return(plots)
  
}


plot.point(cols=c("zone.type.name"),metric=c("tot.NumMosquitos"))
plot.point(cols=c("zone.type.name"),metric=c("pct.WnvPresent"))
plot.point(cols=c("zone.type.name"),metric=c("WnvPresent.Mosquitos"))

plot.point(cols=c("day.of.wk.name"),metric=c("WnvPresent.Mosquitos"))
plot.point(cols=c("satellite.ind"),metric=c("WnvPresent.Mosquitos"))
plot.point(cols=c("trap_type"),metric=c("WnvPresent.Mosquitos"))





plot.polygon <- function(cols,area.name="community",metric=tot.NumMosquitos
                       ,df=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,]) {
  df.plot <- df %>%
    group_by(!! sym(area.name)) %>%
    summarise(tot.NumMosquitos = sum(tot.NumMosquitos)
              ,pct.WnvPresent = sum(any.WnvPresent)/n()
              ,WnvPresent.Mosquitos = sum(tot.NumMosquitos) * 
                sum(any.WnvPresent)/n()
    ) 

  if(area.name == 'community') {
    df.area <- comm.area.df
    df.plot2 <- inner_join(df.plot,df.area,by=c("community"="id"))
  } 
  else {
    if(area.name == 'ZCTA5CE10') {
      df.area <- zcta.df
      df.plot$ZCTA5CE10 <- as.character(df.plot$ZCTA5CE10)
      df.plot2 <- inner_join(df.plot,df.area,by=c("ZCTA5CE10"="id"))
    }
    else {
      if(area.name == 'BlkGrp.geoid') {
        df.area <- bg.df
        df.plot$BlkGrp.geoid <- as.character(df.plot$BlkGrp.geoid)
        df.plot2 <- inner_join(df.plot,df.area,by=c("BlkGrp.geoid"="id"))
      }
      else {
        if(area.name == 'Tract.geoid') {
          df.area <- tract.df
          df.plot$Tract.geoid <- as.character(df.plot$Tract.geoid)
          df.plot2 <- inner_join(df.plot,df.area,by=c("Tract.geoid"="id"))
        }
      }
    }
  }
  
  # df.plot2[,!! sym(metric)] <- 
  
  # return(df.plot2)
 
  plots <- ggplot(aes(x = long, y = lat, group = group)
                      ,data = as.data.frame(df.plot2)) +
    xlim(min(comm.area.df$long), max(comm.area.df$long)) + 
    ylim(min(comm.area.df$lat), max(comm.area.df$lat)) + 
    geom_polygon(alpha = 0.4
                 # ,colour="grey"
                 ,aes(fill=(!! sym(metric))) 
                # ,aes(fill=(!! sym(metric))
                #      # ,shape=(!! sym(cols))
                #      ,size=(!! sym(metric))
                # )
                ) +
    geom_polygon(aes(x=long,y=lat,group=group)
                 ,data = df.area
                 ,colour="grey"
                 ,fill=NA
                 ) +
    # scale_colour_gradient(colours = (!! sym(metric))) + 
    coord_equal() + 
    # scale_colour_manual(values=cbPalette) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
  geom_point(data=cbind(wnv.traps,group=1)
             ,aes(x=as.numeric(rev.lng),y=as.numeric(rev.lat))
             ,size=0.5)
    
  plot(plots)
  
  return(plots)
  
}

# plot.polygon(cols=c("zone.type.name"),area.name="community",metric=c("tot.NumMosquitos"))

plot.polygon(cols=c("zone.type.name"),area.name="community",metric=c("tot.NumMosquitos"))
plot.polygon(cols=c("zone.type.name"),area.name="community",metric=c("pct.WnvPresent"))
plot.polygon(cols=c("zone.type.name"),area.name="community",metric=c("WnvPresent.Mosquitos"))

plot.point.time(cols=c("zone.type.name"),metric=c("tot.NumMosquitos"))
plot.point.time(cols=c("zone.type.name"),metric=c("pct.WnvPresent"))
plot.point.time(cols=c("zone.type.name"),metric=c("WnvPresent.Mosquitos"))


plot.polygon(cols=c("zone.type.name"),area.name="ZCTA5CE10",metric=c("tot.NumMosquitos"))
plot.polygon(cols=c("zone.type.name"),area.name="Tract.geoid",metric=c("tot.NumMosquitos"))
plot.polygon(cols=c("zone.type.name"),area.name="BlkGrp.geoid",metric=c("tot.NumMosquitos"))





chk2 <- chk[chk$community == 'OHARE',]



my.plot <- plot.point(cols=c("zone.type.name"),metric=c("tot.NumMosquitos"))
my.plot + geom_polygon(data = comm.area.df
                       , aes(x = long, y = lat, group = group)
                       , colour="grey"
                       , alpha=0.2
                       , fill = NA)
  
ggplot() + geom_polygon(data = comm.area, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




plot.point.time <- function(cols,metric=tot.NumMosquitos
                            ,df=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,]) {
  df.plot <- df %>%
    group_by(yr,mo,lat,lng,!!! sym(cols)) %>%
    summarise(tot.NumMosquitos = sum(tot.NumMosquitos)
              ,pct.WnvPresent = sum(any.WnvPresent)/n()
              ,WnvPresent.Mosquitos = sum(tot.NumMosquitos) * 
                sum(any.WnvPresent)/n()
    ) %>%
    mutate(mo.name = factor(month.abb[mo],levels=month.abb))

  plots <- ggplot(aes(lng,lat),data = as.data.frame(df.plot)) +
    geom_jitter(width = 0.01, height = 0.01, alpha = 0.4, stroke = 1
                ,aes(colour=(!!! sym(cols))
                     # ,shape=(!!! sym(cols))
                     ,size=(!!! sym(metric))
                )) +
    facet_grid(rows=vars(yr),cols=vars(mo.name)
               ,switch="y") + 
    coord_equal() + 
    scale_colour_manual(values=cbPalette) +
    # theme_linedraw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) 
  
  plot(plots)
  
  return(plots)
  
}





# Zone types are distributed across the city but fixed in time.
# These are a bit difficult to read.  Too many types to tell what's
# going on, and it's hard to even tell trends in trap test results.

# plot.point.time(cols=c("zone_type"),metric=c("tot.NumMosquitos"))
# plot.point.time(cols=c("zone_type"),metric=c("pct.WnvPresent"))
# plot.point.time(cols=c("zone_type"),metric=c("WnvPresent.Mosquitos"))

plot.point.time(cols=c("zone.type.name"),metric=c("tot.NumMosquitos"))
plot.point.time(cols=c("zone.type.name"),metric=c("pct.WnvPresent"))
plot.point.time(cols=c("zone.type.name"),metric=c("WnvPresent.Mosquitos"))


# Day of week is probably not a useful predictor.  There are strong
# temporal trends (e.g. some months where all tests were done the 
# same weekday), so this probably muddles results if used as a predictor.

plot.point.time(cols=c("day.of.wk.name"),metric=c("tot.NumMosquitos"))
plot.point.time(cols=c("day.of.wk.name"),metric=c("WnvPresent.Mosquitos"))

plot.point.time(cols=c("satellite.ind"),metric=c("tot.NumMosquitos"))
plot.point.time(cols=c("satellite.ind"),metric=c("WnvPresent.Mosquitos"))

plot.point.time(cols=c("trap_type"),metric=c("tot.NumMosquitos"))
plot.point.time(cols=c("trap_type"),metric=c("WnvPresent.Mosquitos"))










plot.polygon.time <- function(cols,area.name="community",metric=tot.NumMosquitos
                         ,df=wnv.trap.date.rev3b[wnv.trap.date.rev3b$train,]) {
  df.plot <- df %>%
    group_by(yr,mo,!! sym(area.name)) %>%
    summarise(tot.NumMosquitos = sum(tot.NumMosquitos)
              ,pct.WnvPresent = sum(any.WnvPresent)/n()
              ,WnvPresent.Mosquitos = sum(tot.NumMosquitos*any.WnvPresent)
              ,WnvPresent.Mosquitos2 = sum(tot.NumMosquitos) * 
                sum(any.WnvPresent)/n()
    ) %>%
    mutate(mo.name = factor(month.abb[mo],levels=month.abb))
  
  
  if(area.name == 'community') {
    df.area <- comm.area.df
    df.plot2 <- inner_join(df.plot,df.area,by=c("community"="id"))
  } 
  else {
    if(area.name == 'ZCTA5CE10') {
      df.area <- zcta.df
      df.plot$ZCTA5CE10 <- as.character(df.plot$ZCTA5CE10)
      df.plot2 <- inner_join(df.plot,df.area,by=c("ZCTA5CE10"="id"))
    }
    else {
      if(area.name == 'BlkGrp.geoid') {
        df.area <- bg.df
        df.plot$BlkGrp.geoid <- as.character(df.plot$BlkGrp.geoid)
        df.plot2 <- inner_join(df.plot,df.area,by=c("BlkGrp.geoid"="id"))
      }
      else {
        if(area.name == 'Tract.geoid') {
          df.area <- tract.df
          df.plot$Tract.geoid <- as.character(df.plot$Tract.geoid)
          df.plot2 <- inner_join(df.plot,df.area,by=c("Tract.geoid"="id"))
        }
      }
    }
  }
  
  # df.plot2[,!! sym(metric)] <- 
  
  # return(df.plot2)
  
  plots <- ggplot(aes(x = long, y = lat, group = group)
                  ,data = as.data.frame(df.plot2)) +
    xlim(min(comm.area.df$long), max(comm.area.df$long)) + 
    ylim(min(comm.area.df$lat), max(comm.area.df$lat)) + 
    geom_polygon(alpha = 0.4
                 # ,colour="grey"
                 ,aes(fill=(!! sym(metric))) 
                 # ,aes(fill=(!! sym(metric))
                 #      # ,shape=(!! sym(cols))
                 #      ,size=(!! sym(metric))
                 # )
    ) +
    facet_grid(rows=vars(yr),cols=vars(mo.name)
               ,switch="y") + 
    # geom_polygon(aes(x=long,y=lat,group=group)
    #              ,data = df.area
    #              ,colour="grey"
    #              ,fill=NA
    # ) +
    # scale_colour_gradient(colours = (!! sym(metric))) + 
    coord_equal() + 
    scale_colour_manual(values=cbPalette) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) #+
    # geom_point(data=cbind(wnv.traps,group=1)
    #            ,aes(x=as.numeric(rev.lng),y=as.numeric(rev.lat))
    #            ,size=0.5)
  
  plot(plots)
  
  return(plots)
  
}


plot.polygon.time(cols=c("zone.type.name"),area.name="community",metric=c("tot.NumMosquitos"))


plot.polygon.time(cols=c("zone.type.name"),area.name="community",metric=c("pct.WnvPresent"))

plot.polygon.time(cols=c("zone.type.name"),area.name="community",metric=c("WnvPresent.Mosquitos"))




