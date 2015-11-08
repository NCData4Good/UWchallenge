wd = "/Users/szelenka/Documents/_svn/Data4Good/2015-11-07/"
setwd(wd)

# Farmers Market
export <- read.csv("Farmers Market.csv",sep=",",header=T)
nc_export <- subset(export,State== "North Carolina")

# Grocery Stores
grocery_store <- read.csv("cleandata_grocery_store.csv",sep=",",header=T)

# SNAP Retailers
snap <- read.csv("SNAP-NC.csv", stringsAsFactors = FALSE)

# Food Banks
foodbank <- read.csv("food_bank-NC.csv", stringsAsFactors = FALSE)

# Food Pantries
pantries <- read.csv("pantries.csv", stringsAsFactors = FALSE)

# Bus Stops
agencies <- read.csv("local_agencies.csv", stringsAsFactors = FALSE)
busstops <- readRDS("busstops.rds")

# Schools
NCschools.info <- read.csv("NCschools-info.csv", stringsAsFactors = FALSE)
local_schools_perf1314 <- read.csv("local_school_perf1314.csv", stringsAsFactors = FALSE)
local_schools_perf1415 <- read.csv("local_school_perf1415.csv", stringsAsFactors = FALSE)

# ACS Census
# library(acs)
# censustableS2201 <- read.acs("ACS_13_5YR_S2201_with_ann.csv", endyear = 2013, span = 5, skip = 2)

# Census Tract
library(sp)
library(rgdal)
library(geosphere)
library(rgeos)
library(maptools)
shpfile.tract <- "tl_2015_37_tract/tl_2015_37_tract.shp"
ogrListLayers(shpfile.tract)
censustract <- readOGR(shpfile.tract, "tl_2015_37_tract")

# Census Public School Zoning
shpfile.tract <- "tl_2015_37_tract/tl_2015_37_tract.shp"
ogrListLayers(shpfile.tract)
censustract <- readOGR(shpfile.tract, "tl_2015_37_tract")

# Places in NC
NCdata <- read.csv("NCdata.csv", stringsAsFactors = FALSE)

# Feeding Sites
feeding.sites = read.csv("feeding-sites DO NOT DISTRIBUTE.csv", header = T)

# Active Agency Programs
agencies <- read.csv("agency_programs.csv", stringsAsFactors = FALSE)

# FIPS for NC counties
fips.nc.counties = read.csv("FoodbankCENC_counties.csv", header=T)

# Foodbank CENC
fips.nc.locations = read.csv("FoodbankCENC_locations.csv", header=T)

# Census by county
api.key.install("ea9423d558f0b583ec9366b3f5ad09201dda2d5a", file = "key.rda")
triangleNC <- geo.make(state = 37, county = c(63, 101, 135, 183), tract = '*', check = TRUE)
censustable <- acs.fetch(endyear = 2013, geography = triangleNC, table.number = "B22002", col.names = "pretty")

