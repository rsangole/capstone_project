rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggmap)
library(animation)
#install.packages("animation")
# This code produces an animated map showing spray areas
# overlaid with mosquito test results, month-by-month.


# Before running:
# - Make sure ImageMagick is installed on your system.
# - Browse online documentation (easily Googled) for using the
#   animation package on your OS. My methods used here for a
#   Windows environment may differ from what is needed on your machine.


# I had to fiddle a bit to get ImageMagick (animation package dependency)
# to work on my Windows machine. The only real hurdle was getting
# the animation package to be aware of my ImageMagick installation.
# Run the following command to check:
ani.options("convert")
# It should return the path to ImageMagick's convert executable. If not,
# set the path as follows (obviously, your path may vary from mine):
ani.options(convert = "C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe")

setwd("/Users/tedinciong/kaggle/westnile/all/")
dir()
train <- read.csv("train.csv", header = TRUE)
head(train)
table(train$NumMosquitos)

spray <- read.csv("spray.csv", header = TRUE)
train$WnvPresent <- factor(train$WnvPresent) # make WnVPresent a factor

chicago_map <- readRDS("./mapdata_copyright_openstreetmap_contributors.rds")


# For every year-month (e.g. "2011-08"), create a plot of spray areas for that month
# overlaid with mosquito data for that month. Use the saveHTML command to turn these
# plots into an interactive animation.
saveHTML({
  for (yearmonth in levels(as.factor(format(as.Date(train$Date),"%Y-%m")))){
    # get training data matching this year-month
    mosquito_data <- train %>% filter(format(as.Date(train$Date),"%Y-%m") == yearmonth)
    # get spray data matching this year-month (spray year-months are subset of train year-months)
    spray_data <- spray %>% filter(format(as.Date(spray$Date),"%Y-%m") == yearmonth)

    # Plot spatio-temporal map of spray and mosquito data.
    map <- ggmap(ggmap = chicago_map, darken = 0.5) +
      geom_point(data = spray_data, aes(x=Longitude, y=Latitude), color = "yellow") +
      geom_point(data = mosquito_data, aes(x=Longitude, y=Latitude, color = WnvPresent)) +
      ggtitle(format(as.Date(mosquito_data$Date),"%B, %Y")) +
      scale_color_discrete(
        name = "West Nile",
        breaks = c(0,1),
        labels = c("Absent", "Present"))
    print(map)
  }
}, interval = 0.1, verbose = FALSE, autoplay = FALSE)

graphics.off()
