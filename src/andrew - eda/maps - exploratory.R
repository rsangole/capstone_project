

# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))

# some standard map packages.
install.packages(c("maps", "mapdata"))
install.packages(c("digest","scales"))

install.packages(c("ggmap"))

# the github version of ggmap, which recently pulled in a small fix I had
# for a bug 
# devtools::install_github("dkahle/ggmap")


# require("ggplot2")
# require("maps")
# require("mapdata")
# 
# require("digest")
# require("scales")
# # require("dplyr")
# require("glue")
# require("purrr")
# # require("tibble")
# # require("tidyr")

require("ggmap")



chgo <- map_data("chicago")
?namespace:maps
?map_data

# chgo <- qmap("Chicago", zoom = 12, color = "bw", legend = "topleft")


?readRDS
?read.shp

read.shp()
?read_sf


install.packages("sf")
require(sf)
shape <- read_sf(dsn = ".", layer = "SHAPEFILE")


chgo <- read_sf(dsn = 
                  paste("D:","ajc188","github","capstone_project","data"
                        ,"raw","chi_community_areas","GIS"
                        ,sep="\\")
                ,layer = "ChicagoCommunityAreas")
str(chgo)




