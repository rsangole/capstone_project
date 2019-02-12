
################################################################################
## Set up packages

# install.packages("rgdal")
# install.packages("rgeos")

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rgeos")



################################################################################
## Construct dataframes for each set of polygons

community.path <- paste("D:\\ajc188\\github\\capstone_project\\data\\raw"
                   ,"chi_community_areas","GIS"
                   ,sep="\\")
comm.area = readOGR(dsn=community.path, layer="ChicagoCommunityAreas"
                    , stringsAsFactors = F)
comm.area.df <- broom::tidy(comm.area, region = "community")


tract.path <- paste("D:\\ajc188\\github\\capstone_project\\data\\raw"
                    ,"census_gis","GIS","tl_2018_17_tract"
                    ,sep="\\")
tract = readOGR(dsn=tract.path, layer="tl_2018_17_tract"
                , stringsAsFactors = F)
tract.df <- broom::tidy(tract, region = "GEOID")


bg.path <- paste("D:\\ajc188\\github\\capstone_project\\data\\raw"
                    ,"census_gis","GIS","tl_2018_17_bg"
                    ,sep="\\")
bg = readOGR(dsn=bg.path, layer="tl_2018_17_bg"
                , stringsAsFactors = F)
bg.df <- broom::tidy(bg, region = "GEOID")


zcta.path <- paste("D:\\ajc188\\github\\capstone_project\\data\\raw"
                    ,"census_gis","GIS","tl_2018_us_zcta510"
                    ,sep="\\")
zcta = readOGR(dsn=zcta.path, layer="tl_2018_us_zcta510_ILCook"
                , stringsAsFactors = F)
zcta.df <- broom::tidy(zcta, region = "ZCTA5CE10")

# wnv_traps_CommArea



################################################################################
## Demonstrate ggplot


plot.comm.area <- ggplot() + 
  geom_polygon(data = comm.area.df, aes(x = long, y = lat, group = group)
               ,colour = "black", fill = NA)

plot.tract <- ggplot() + 
  geom_polygon(data = tract.df, aes(x = long, y = lat, group = group)
               ,colour = "red", fill = NA)

plot.bg <- ggplot() + 
  geom_polygon(data = bg.df, aes(x = long, y = lat, group = group)
               ,colour = "blue", fill = NA)

plot.zcta <- ggplot() + 
  geom_polygon(data = zcta.df, aes(x = long, y = lat, group = group)
               ,colour = "orange", fill = NA)




plot.comm.area + 
  xlim(min(comm.area.df$long), max(comm.area.df$long)) + 
  ylim(min(comm.area.df$lat), max(comm.area.df$lat)) + 
  geom_point(data=wnv.traps,aes(x=as.numeric(rev.lng),y=as.numeric(rev.lat))) +
  # geom_polygon(data = tract.df, aes(x = long, y = lat, group = group)
  #              ,colour = "red", fill = NA) + 
  # geom_polygon(data = bg.df, aes(x = long, y = lat, group = group)
  #              ,colour = "blue", fill = NA) + 
  # geom_polygon(data = zcta.df, aes(x = long, y = lat, group = group)
  #              ,colour = "orange", alpha=0.5, fill = NA) +
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
        axis.line.y = element_blank()) 






