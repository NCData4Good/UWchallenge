library(readr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(colorRamps)
library(rgdal)
library(leaflet)

library(shinydashboard)

options(digits=1)

## Schools
load("data/schools.rda")

## Grocery stores
load("data/places.rda")

## School district shapes (TODO reference source)
sd.shapes <- readOGR("data/sd-shapes", "sd-shapes")
sd.shapes$color <- rep(1:5, ceiling(nrow(sd.shapes)/5))[1:nrow(sd.shapes)]

## School district shapes (TODO reference source)
district.shapes <- readOGR("data/district-shapes", "district-shapes")
district.shapes$name <- as.character(district.shapes$name)
district.shapes$color <-
  rep(1:5, ceiling(nrow(district.shapes)/5))[1:nrow(district.shapes)]

schools <- schools[schools$school.name %in% unique(sd.shapes$schnam),]

schoolIcon <- makeIcon(
  "./data/icons/school.png",
  iconWidth = 30, iconHeight = 30
)

cols <- c("blue", "red")
