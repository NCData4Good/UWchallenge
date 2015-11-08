library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(rgdal)
library(rgeos)
library(parallel)

setwd("~/Dropbox/Projects/datacrunch/data/original")

names_to_r <- function(names){
  names <- str_replace_all(names, "^ ", "")
  names <- str_replace_all(names, " ", ".")
  names <- str_replace_all(names, "__", ".")
  names <- str_replace_all(names, "_", ".")
  names <- str_replace_all(names, "\\.\\.", ".")
  names <- tolower(names)
}

test_in_sd <-
function(lon, lat, sd.name, i) {
  points.df <- data.frame(lon = lon[i], lat = lat[i])
  proj.string <- proj4string(sd.shapes[sd.shapes$schnam == sd.name,])

  gContains(sd.shapes[sd.shapes$schnam == sd.name,],
            SpatialPoints(points.df,
                          proj4string=CRS(proj.string)))
}

get_sd_vec <-
function(lon, lat, sd.name){
  sapply(1:length(lon),
         function(i) test_in_sd(lon, lat, sd.name, i))
}

get_sd_df <- function(lon, lat){
  data.frame(setNames(mclapply(sd.shapes$schnam,
                               function(sd.name) get_sd_vec(lon, lat, sd.name),
                               mc.cores=6), sd.shapes$schnam),
             check.names = FALSE)
}

test_in_district <-
function(lon, lat, sd.name, i) {
  points.df <- data.frame(lon = lon[i], lat = lat[i])
  proj.string <- proj4string(district.shapes[district.shapes$name == sd.name,])

  gContains(district.shapes[district.shapes$name == sd.name,],
            SpatialPoints(points.df,
                          proj4string=CRS(proj.string)))
}

get_district_vec <-
function(lon, lat, sd.name){
  sapply(1:length(lon),
         function(i) test_in_district(lon, lat, sd.name, i))
}

get_district_df <- function(lon, lat){
  data.frame(setNames(mclapply(district.shapes$name,
                               function(sd.name) get_district_vec(lon, lat, sd.name),
                               mc.cores=6), district.shapes$name),
             check.names = FALSE)
}

################################################################################
## School boundaries shapefiles
sd.shapes <-
readOGR("SABS_1314/SABS_1314.shp",
        layer="SABS_1314")

str(sd.shapes@data)
sd.shapes <- sd.shapes[sd.shapes$leaid %in% c(3703480, ## Orange
                                              3700720, ## CH
                                              3701260, ## Durham
                                              3704720, ## Wake
                                              3702370),] ## Johnston

sd.shapes <- spTransform(sd.shapes, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80"))

sd.shapes <- sd.shapes[sd.shapes$schnam != "Unassigned", ]

sd.shapes$schnam <- as.character(sd.shapes$schnam)

sd.shapes$schnam[sd.shapes$ncessch == 370126000546] <-
  "OAK GROVE ELEMENTARY DURHAM"
sd.shapes$schnam[sd.shapes$ncessch == 370472001311] <-
  "OAK GROVE ELEMENTARY WAKE"

## filter unassigned
names(sd.shapes@data) <- names_to_r(names(sd.shapes@data))

writeOGR(sd.shapes, "../sd-shapes", "sd-shapes", driver = "ESRI Shapefile")

################################################################################
## School district shapefiles
district.shapes <- readOGR("tl_2015_37_unsd/tl_2015_37_unsd.shp",
                         layer="tl_2015_37_unsd")
str(district.shapes@data)

names(district.shapes@data) <- names_to_r(names(district.shapes@data))

district.shapes <-
  district.shapes[district.shapes$name %in% c("Chapel Hill-Carrboro Schools",
                                              "Durham Public Schools",
                                              "Johnston County Schools",
                                              "Orange County Schools",
                                              "Wake County Schools"),]

district.shapes$name <- as.character(district.shapes$name)

writeOGR(district.shapes, "../district-shapes",
         "district-shapes", driver = "ESRI Shapefile")

################################################################################
## Census district shapefiles
census.shapes <- readOGR("tl_2015_37_tract/tl_2015_37_tract.shp",
                         layer="tl_2015_37_tract")
str(census.shapes@data)

census.shapes <-
  census.shapes[census.shapes$COUNTYFP %in% c("063", "183", "135", "101"),]

names(census.shapes@data) <- names_to_r(names(census.shapes@data))

writeOGR(census.shapes, "../census-shapes",
         "census-shapes", driver = "ESRI Shapefile")

################################################################################
## Feeding Sites
feeding.sites <- read_csv("feeding-sites DO NOT DISTRIBUTE.csv",
                          col_types = list(SandP = col_character(),
                                           BandP = col_character(),
                                           BandS = col_character(),
                                           `B L and P` = col_character(),
                                           `B L and S` = col_character(),
                                           `ZZZip Code` = col_character(),
                                           `ZZSite Type` = col_character(),
                                           `Coded Site Type` = col_character(),
                                           ` Early Meal Start` = col_character(),
                                           ` Early Meal End` = col_character(),
                                           ` Start Date` = col_character(),
                                           ` End Date` = col_character(),
                                           ` Total Days of Operation` = col_character(),
                                           ` Late Meal Start` = col_character(),
                                           ` Late Meal End` = col_character(),
                                           `ZZZZHH Income Level 1 SE_T056_002` = col_character()))

names(feeding.sites) <- names_to_r(names(feeding.sites))
str(feeding.sites)

################################################################################
## Places
places <- data.frame(place.type = "Feeding Site",
                     place.name = feeding.sites$zzsite.name,
                     lat = feeding.sites$zzlat,
                     lon = feeding.sites$zzlon,
                     stringsAsFactors = FALSE)

################################################################################
## Agency Programs
## all.agency.programs <- read_csv("agency_programs.csv")
## str(all.agency.programs)

## names(all.agency.programs) <- names_to_r(names(all.agency.programs))

## unique(all.agency.programs$program.code)

## mobile.fps <-
##   all.agency.programs %>%
##   filter(program.code == "MOBILE FP")

## places <- rbind(places, data.frame(place.type = "Mobile Pantry",
##                      place.name = mobile.fps$name,
##                      lat = mobile.fps$lat,
##                      lon = mobile.fps$lng,
##                      stringsAsFactors = FALSE))


################################################################################
## Agency Programs
nc.places <- read_csv("NCData.csv")
str(nc.places)

names(nc.places) <- names_to_r(names(nc.places))
unique(nc.places$feature.class)

churches <- nc.places[nc.places$feature.class == "Church" &
                      nc.places$county.name %in% c("Durham", "Johnston",
                                                   "Orange", "Wake") ,]

places <- rbind(places, data.frame(place.type = "Churches",
                     place.name = churches$feature.name,
                     lat = churches$prim.lat.dec,
                     lon = churches$prim.long.dec,
                     stringsAsFactors = FALSE))

################################################################################
## Grocery (and others) Stores
all.grocery.stores <- read_csv("cleandata_grocery_store.csv")
str(all.grocery.stores)

all.grocery.stores <- all.grocery.stores[,2:length(all.grocery.stores)]

names(all.grocery.stores) <- names_to_r(names(all.grocery.stores))

## break out into types
unique(all.grocery.stores$primary.sic.description)

## convenience stores
convenience.stores <-
  all.grocery.stores %>%
  filter(primary.sic.description == "Convenience Stores")

places <- rbind(places, data.frame(place.type = "Convenience Stores",
                     place.name = convenience.stores$company.name,
                     lat = convenience.stores$latitude,
                     lon = convenience.stores$longitude,
                     stringsAsFactors = FALSE))

## cs.sd.df <- get_sd_df(convenience.stores$longitude,
##                       convenience.stores$latitude)

## convenience.stores <-
##   cbind(convenience.stores, cs.sd.df)

## save(convenience.stores, file="../convenience-stores.rda")

## Grocery stores
grocery.stores <-
  all.grocery.stores %>%
  filter(str_detect(primary.sic.description, "^Grocers"))

places <- rbind(places, data.frame(place.type = "Grocery Stores",
                     place.name = grocery.stores$company.name,
                     lat = grocery.stores$latitude,
                     lon = grocery.stores$longitude,
                     stringsAsFactors = FALSE))

## gs.sd.df <- get_sd_df(grocery.stores$longitude,
##                       grocery.stores$latitude)

## grocery.stores <-
##   cbind(grocery.stores, gs.sd.df)

## save(grocery.stores, file="../grocery-stores.rda")

################################################################################
## Farmers Markets
farmers.markets <- read_csv("Export.csv",
                            col_types = list(Season4Date = col_character(),
                                             Season4Time = col_character(),
                                             x = col_character(),
                                             y = col_character()))

str(farmers.markets)

farmers.markets <-
  farmers.markets %>%
  filter(State == "North Carolina",
         County %in% c("Orange", "Durham", "Wake", "Johnston"))

str(farmers.markets)

farmers.markets$x <- as.numeric(farmers.markets$x)
farmers.markets$y <- as.numeric(farmers.markets$y)

names(farmers.markets) <- names_to_r(names(farmers.markets))

places <- rbind(places, data.frame(place.type = "Farmers Markets",
                     place.name = farmers.markets$marketname,
                     lat = farmers.markets$y,
                     lon = farmers.markets$x,
                     stringsAsFactors = FALSE))

## fm.sd.df <- get_sd_df(farmers.markets$x,
##                    farmers.markets$y)

## farmers.markets <-
##   cbind(farmers.markets, fm.sd.df)

## save(farmers.markets, file="../farmers-markets.rda")

################################################################################
## Food banks
## food.banks <- read_csv("food_bank-NC.csv")
## str(food.banks)

## food.banks <-
##   food.banks %>%
##   filter(city == "Raleigh")

## places <- rbind(places, data.frame(place.type = "Food Bank",
##                      place.name = food.banks$food.bank,
##                      lat = food.banks$lat,
##                      lon = food.banks$lng,
##                      stringsAsFactors = FALSE))

## fb.sd.df <- get_sd_df(food.banks$lng,
##                       food.banks$lat)

## food.banks <-
##   cbind(food.banks, fb.sd.df)

## save(food.banks, file="../food-banks.rda")

################################################################################
## Food pantries
food.pantries <- read_csv("pantries.csv")
str(food.pantries)
unique(food.pantries$county)

places <- rbind(places, data.frame(place.type = "Food Pantries",
                     place.name = food.pantries$foodpantry,
                     lat = food.pantries$lat,
                     lon = food.pantries$lon,
                     stringsAsFactors = FALSE))

## fp.sd.df <- get_sd_df(food.pantries$lon,
##                       food.pantries$lat)

## food.pantries <-
##   cbind(food.pantries, fp.sd.df)

## save(food.pantries, file="../food-pantries.rda")

################################################################################
## SNAP retailers
snap.retailers <- read_csv("SNAP-NC.csv")
str(snap.retailers)

snap.retailers <-
  snap.retailers %>%
  filter(County %in% c("ORANGE",
                       "WAKE",
                       "DURHAM",
                       "JOHNSTON"))

names(snap.retailers) <- names_to_r(names(snap.retailers))

places <- rbind(places, data.frame(place.type = "SNAP Retailers",
                     place.name = snap.retailers$store.name,
                     lat = snap.retailers$latitude,
                     lon = snap.retailers$longitude,
                     stringsAsFactors = FALSE))

## snap.sd.df <- get_sd_df(snap.retailers$longitude,
##                         snap.retailers$latitude)

## snap.retailers <-
##   cbind(snap.retailers, snap.sd.df)

## save(snap.retailers, file="../snap-retailers.rda")

################################################################################
## Bus Agencies
## bus.agencies <- read_csv("local_agencies.csv")
## str(bus.agencies)

## names(bus.agencies) <- names_to_r(names(bus.agencies))

## ba.sd.df <- get_sd_df(bus.agencies$position.lng,
##                       bus.agencies$position.lat)

## bus.agencies <-
##   cbind(bus.agencies, ba.sd.df)

## save(bus.agencies, file="../bus-agencies.rda")

################################################################################
## Bus Stops
bus.stops <- readRDS("busstops.rds")
str(bus.stops)

names(bus.stops) <- names_to_r(names(bus.stops))

places <- rbind(places, data.frame(place.type = "Bus Stops",
                     place.name = bus.stops$name,
                     lat = bus.stops$location.lat,
                     lon = bus.stops$location.lng,
                     stringsAsFactors = FALSE))

## bs.sd.df <- get_sd_df(bus.stops$location.lng,
##                       bus.stops$location.lat)

## bus.stops <-
##   cbind(bus.stops, bs.sd.df)

## save(bus.stops, file="../bus-stops.rda")

################################################################################
## School performance 13-14

school.performance.1314 <-
read_csv("school_perf1314.csv",
         col_types = list(School.Code = col_character()))

str(school.performance.1314)

school.performance.1314 <-
  school.performance.1314 %>%
  filter(LEA.Name %in% c("Orange County Schools",
                         "Durham Public Schools",
                         "Wake County Schools",
                         "Chapel Hill-Carrboro Schools",
                         "Johnston County Schools",
                         "Charter Schools",
                         "Deaf and Blind Schools"))

names(school.performance.1314) <- names_to_r(names(school.performance.1314))

school.performance.1314$school.name <-
  toupper(school.performance.1314$school.name)

school.performance.1314$school.name[school.performance.1314$school.code == 320360] <-
  "OAK GROVE ELEMENTARY DURHAM"

school.performance.1314$school.name[school.performance.1314$school.code == 920522] <-
  "OAK GROVE ELEMENTARY WAKE"

save(school.performance.1314, file="../school-performance-1314.rda")

################################################################################
## School performance 14-15

school.performance.1415 <- read_csv("school_perf1415.csv",
         col_types = list(School.Code = col_character()))

str(school.performance.1415)

school.performance.1415 <-
  school.performance.1415 %>%
  filter(LEA.Name %in% c("Orange County Schools",
                         "Durham Public Schools",
                         "Wake County Schools",
                         "Chapel Hill-Carrboro Schools",
                         "Johnston County Schools",
                         "Charter Schools",
                         "Deaf and Blind Schools"))

names(school.performance.1415) <- names_to_r(names(school.performance.1415))

school.performance.1415$school.name <-
  toupper(school.performance.1415$school.name)

school.performance.1415$school.name[school.performance.1415$school.code == 320360] <-
  "OAK GROVE ELEMENTARY DURHAM"

school.performance.1415$school.name[school.performance.1415$school.code == 920522] <-
  "OAK GROVE ELEMENTARY WAKE"

save(school.performance.1415, file="../school-performance-1415.rda")

################################################################################
## Census

census <-
read_csv("data-for-diplomas-unmerged/Census_Data_2010.csv",
         col_types = list(GIDTR = col_character(),
                          pct_BILQ_Mailout_count_CEN_2010 = col_numeric(),
                          pct_Age5p_African_ACS_08_12 = col_numeric(),
                          pct_Age5p_Polish_ACS_08_12 = col_numeric(),
                          pct_Age5p_OthEuro_ACS_08_12 = col_numeric(),
                          pct_Age5p_Hindi_ACS_08_12 = col_numeric(),
                          pct_Age5p_Persian_ACS_08_12 = col_numeric(),
                          pct_Age5p_OthPacIsl_ACS_08_12 = col_numeric(),
                          pct_Age5p_FrCreole_ACS_08_12 = col_numeric(),
                          pct_Age5p_Greek_ACS_08_12 = col_numeric(),
                          pct_Age5p_Urdu_ACS_08_12 = col_numeric(),
                          pct_Age5p_Cambodian_ACS_08_12 = col_numeric(),
                          pct_Age5p_Laotian_ACS_08_12 = col_numeric(),
                          pct_Age5p_WGerman_ACS_08_12 = col_numeric(),
                          pct_Age5p_OthUnSp_ACS_08_12 = col_numeric(),
                          pct_Age5p_Hungarian_ACS_08_12 = col_numeric(),
                          pct_Age5p_Armenian_ACS_08_12 = col_numeric(),
                          pct_Age5p_Hebrew_ACS_08_12 = col_numeric(),
                          pct_Age5p_Hmong_ACS_08_12 = col_numeric(),
                          pct_Age5p_Scandinav_ACS_08_12 = col_numeric(),
                          pct_Age5p_Yiddish_ACS_08_12 = col_numeric(),
                          pct_Age5p_Navajo_ACS_08_12 = col_numeric()))

names(census) <- paste0("cen.", names_to_r(names(census)))
census$cen.tract <- sprintf("%06d", census$cen.tract)

census <-
  census[census$cen.state.name == "North Carolina",]

census <-
  census[census$cen.county.name %in% c("Durham County",
                                       "Johnston County",
                                       "Orange County",
                                       "Wake County"),]

## remove duplicated tract in census
census <-
  census %>%
  filter(cen.tract != 980100,
         cen.county.name != "Durham County")

################################################################################
## Schools
schools <- read_csv("NCschools-info.csv")

schools <-
  schools %>%
  filter(County %in% c("ORANGE COUNTY", "WAKE COUNTY", "DURHAM COUNTY",
                       "JOHNSTON COUNTY"))

str(schools)

names(schools) <- names_to_r(names(schools))

schools$school.name[schools$school.code == "320360"] <-
  "OAK GROVE ELEMENTARY DURHAM"

schools$school.name[schools$school.code == "920522"] <-
  "OAK GROVE ELEMENTARY WAKE"

## drop incomplete data - looks like charter schools only
## schools$school.name[!complete.cases(schools)]
schools <- schools[complete.cases(schools),]

################################################################################
## Map places to schools
## places <-
##   places %>%
##   filter(place.type %in% c("Convenience Stores",
##                            "Food Bank"))
places <- rbind(places, data.frame(place.type = "Schools",
                     place.name = schools$school.name,
                     lat = schools$latitude,
                     lon = schools$longitude,
                     stringsAsFactors = FALSE))

places.sd.df <- get_sd_df(places$lon,
                          places$lat)

places <-
  cbind(places, places.sd.df)

places.district.df <- get_district_df(places$lon,
                                      places$lat)

places <-
  cbind(places, places.district.df)

save(places, file="../places.rda")

################################################################################
## Census/District overlap

sd.shapes$census.tract <- character(nrow(sd.shapes))
sd.shapes$census.tract.overlap <- 0

for(tract.id in census.shapes$tractce){
  intersect <-
    sapply(
        1:nrow(sd.shapes),
        function(i) gIntersection(
          census.shapes[census.shapes$tractce == tract.id,],
          sd.shapes[i,]
        )
      )

  intersect <- sapply(intersect, function(x) if(!is.null(x)) gArea(x) else 0)

  sd.shapes$census.tract <-
    ifelse(intersect > sd.shapes$census.tract.overlap,
           tract.id,
           sd.shapes$census.tract)

  sd.shapes$census.tract.overlap <-
  ifelse(intersect > sd.shapes$census.tract.overlap,
         intersect,
         sd.shapes$census.tract.overlap)
}

census.overlap <-
  sd.shapes@data %>%
  select(schnam, census.tract)

census.overlap$schnam <- toupper(census.overlap$schnam)

schools <-
  schools %>%
    left_join(census.overlap, by = c("school.name" = "schnam") )

## ## Visual sanity check
## census.shapes$color <- rep(1:5, ceiling(nrow(census.shapes)/5))[1:nrow(census.shapes)]
## leaflet(census.shapes) %>%
## addProviderTiles("CartoDB.Positron") %>%
## addPolygons(
##       stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5,
##       color = ~colorFactor(brewer.pal(5, "Set2"), census.shapes$color)(color), ##
##       popup = ~census.shapes$tractce) %>%
##     addCircles(lng=~schools$longitude,
##                lat=~schools$latitude,
##                popup=~paste0(schools$school.name, "\n Tract:", schools$census.tract))

## get census tract for schools with no district shapes - simple within tract
for(tract.id in census.shapes$tractce){
  in.tract <-
  sapply(
        1:nrow(schools),
        function(i) gContains(
          census.shapes[census.shapes$tractce == tract.id,],
          SpatialPoints(as.data.frame(schools[i,c("longitude", "latitude")]),
                        proj4string=CRS(
                          proj4string(
                            census.shapes[census.shapes$tractce == tract.id,]))
                       )
        )
      )

  schools$census.tract[in.tract] <-
    ifelse(is.na(schools$census.tract), tract.id, schools$census.tract)[in.tract]
}

## Add Census Data
schools <-
  schools %>%
    left_join(census, by = c("census.tract" = "cen.tract") )

################################################################################
## Summarize places data to school level

## Farmers markets
fm.sd.df <- places %>%
  filter(place.type == "Farmers Markets") %>%
  select(5:length(places))

fm.count.by.school <-
  data.frame(school.name = names(colSums(fm.sd.df)),
             fm.count = colSums(fm.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(fm.count.by.school, by = c("school.name" = "school.name") )

## Food pantries
fp.sd.df <- places %>%
  filter(place.type == "Food Pantries") %>%
  select(5:length(places))

fp.count.by.school <-
  data.frame(school.name = names(colSums(fp.sd.df)),
             fp.count = colSums(fp.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(fp.count.by.school, by = c("school.name" = "school.name") )

## SNAP retailers
snap.sd.df <- places %>%
  filter(place.type == "SNAP Retailers") %>%
  select(5:length(places))

snap.count.by.school <-
  data.frame(school.name = names(colSums(snap.sd.df)),
             snap.count = colSums(snap.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(snap.count.by.school, by = c("school.name" = "school.name") )


## Bus Stops
bs.sd.df <- places %>%
  filter(place.type == "Bus Stops") %>%
  select(5:length(places))

bs.count.by.school <-
  data.frame(school.name = names(colSums(bs.sd.df)),
             bs.count = colSums(bs.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(bs.count.by.school, by = c("school.name" = "school.name") )

## Grocery Stores
gs.sd.df <- places %>%
  filter(place.type == "Grocery Stores") %>%
  select(5:length(places))

gs.count.by.school <-
  data.frame(school.name = names(colSums(gs.sd.df)),
             gs.count = colSums(gs.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(gs.count.by.school, by = c("school.name" = "school.name") )

## Convenience Stores
cs.sd.df <- places %>%
  filter(place.type == "Convenience Stores") %>%
  select(5:length(places))

cs.count.by.school <-
  data.frame(school.name = names(colSums(cs.sd.df)),
             cs.count = colSums(cs.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(cs.count.by.school, by = c("school.name" = "school.name") )

## Churches
church.sd.df <- places %>%
  filter(place.type == "Churches") %>%
  select(5:length(places))

church.count.by.school <-
  data.frame(school.name = names(colSums(church.sd.df)),
             church.count = colSums(church.sd.df),
             stringsAsFactors = FALSE)

schools <-
  schools %>%
    left_join(church.count.by.school, by = c("school.name" = "school.name") )

################################################################################
## Add school performance to schools
## 13-14
names(school.performance.1314) <- paste0("sp1314.",
                                         names(school.performance.1314))

schools <-
  schools %>%
    left_join(school.performance.1314, by = c("school.name" = "sp1314.school.name") )

## 14-15
names(school.performance.1415) <- paste0("sp1415.",
                                         names(school.performance.1415))

schools <-
  schools %>%
    left_join(school.performance.1415, by = c("school.name" = "sp1415.school.name") )


save(schools, file = "../schools.rda")

################################################################################
## Distance Matrix
miles.per.meter = 100 / 2.54 / 12 / 5280
dist.mat = distm(places[,c("lon", "lat")]) * miles.per.meter
dimnames(dist.mat) = list(places$place.name, places$place.name)

save(dist.mat, file = "../dist-mat.rda")

## schoolz <- schools
schools <- schoolz

for(type in unique(places$place.type)){
  for(school.name in schools$school.name) {
    place.distances <- dist.mat[school.name,
                                places$place.name[places$place.type == type]]

    if(length(place.distances > 0)){
      closest.place.index <- which(place.distances == min(place.distances))[1]
      place.distances[closest.place.index]
      print(school.name)
      print(names(place.distances[closest.place.index]))
      type.name = names_to_r(type)
      schools[schools$school.name == school.name,
              paste0("closest.", type.name)] = names(place.distances[closest.place.index])
      schools[schools$school.name == school.name,
              paste0("distance.to.closest.", type.name)] = place.distances[closest.place.index]
    }
  }
}

save(schools, file = "../schools.rda")
