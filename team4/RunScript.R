# I like clearing my workspace to make sure I'm not working with old variables
rm(list=ls(all=TRUE))

############################################################################################
### initialize required libraries
############################################################################################
library(sp)
library(geosphere)
library(maptools)
library(rgdal)
library(ggplot2)
library(rgeos)
library(ggmap)
library(reshape)
library(plyr)
library(acs)

source("http://bit.ly/dasi_inference")


############################################################################################
### Reading data
############################################################################################

# American community survey
filename1 <- "Data4good/ACS_13_5YR_S2201.csv"
dataCensusPov <- read.csv(filename1, skip = 1)

filename11 <- "Data4good/ACS_13_5YR_S1901.csv"
dataCensusIncome <- read.csv(filename11, skip = 1)


filename2 <- "Data4good/ACS_13_5YR_S1401.csv"
dataCensusEd <- read.csv(filename2)

filename2 <- "Data4good/ACS_14_1YR_S2303.csv"
dataCensusEmploy <- read.csv(filename2)

# List of grocery stores, Need: average price per transaction, amount of organic stuff, average bottle of sodas sold.
filename3 <- "Data4good/cleandata_grocery_store.csv"
dataGroceryStoreAll <- read.csv(filename3)

# Farmer's markets from Wake, Orange, Johnston, Durham counties
filename4 <- "Data4good/Export.csv"
dataFarmersMarketAll <- read.csv(filename4)

# Location of food banks
filename5 <- "Data4good/food_bank-NC.csv"
dataFoodbanksAll <- read.csv(filename5)

# Location of bus stops
filename6 <- "Data4good/local_agencies.csv"
dataBusstopAll <- read.csv(filename6)

# School performance data 
filename7 <- "Data4good/local_school_perf1314.csv"
dataSchoolsPerf14All <- read.csv(filename7)

# School performance data
filename8 <- "Data4good/local_school_perf1415.csv"
dataSchoolsPerf15All <- read.csv(filename8)

# Location of schools data, how many students on free and reduced lunch
filename9 <- "Data4good/NCschools-info.csv"
dataSchoolsMetaAll <- read.csv(filename9)

# Location of food pantries
filename10 <- "Data4good/pantries.csv"
dataPantryAll <- read.csv(filename10)

# Location of retailers that accept SNAP 
filename11 <- "Data4good/SNAP-NC.csv"
dataSnapAll <- read.csv(filename11)


# Location of Feeding Sites 
filename12 <- "Data4good/feeding-sites.csv"
dataFeedAll <- read.csv(filename12)


############################################################################################
### Reading shapefiles
############################################################################################
# shapefile for census tracts
shapeCensus <- readOGR(dsn = "Data4good", "tl_2015_37_tract")

# shapefile for counties
shapeSchool <- readOGR(dsn = "Data4good", "tl_2015_37_unsd")

# shapefile for public school zoning
shapeSchoolD <- readOGR(dsn = "Data4good", "SABS_1314")
############################################################################################
### Manipulating census data 
############################################################################################

#
#proj4string(shapeCensus) <- CRS("+init=epsg:27700")
#shapeCensus <- spTransform(shapeCensus, CRS("+init=epsg:4326"))

# Merge poverty and census data into 1 dataframe 
dataAllCensus <- merge(dataCensusPov,dataCensusIncome, by.x = "Id", by.y = "Id")
# clip the data I want 
vecNeeded <- c(1:5,22,44,seq(134,235,2))
dataCensus.Clip <- dataAllCensus[,vecNeeded]

# clip the counties I want
# tmpCounty <- c(,'DURHAM','JOHNSTON','ORANGE','WAKE')
tmpCounty <- c('063', '101','135','183')
shapeCensus.clip <- shapeCensus[grepl(paste(tmpCounty, collapse = '|'),shapeCensus$COUNTYFP,ignore.case=T), ]

tmpCounty4 <- c('37063', '37101','37135','37183')
dataFeedAll.clip <- dataFeedAll[grepl(paste(tmpCounty4, collapse = '|'),dataFeedAll$ZZSTCOFIPS,ignore.case=T), ]
dataFeedAll.clip.SSO <- dataFeedAll[grepl("SSO",dataFeedAll$ZZProgram.Type,ignore.case=T), ]
dataFeedAll.clip.SFSP <- dataFeedAll[grepl("SFSP",dataFeedAll$ZZProgram.Type,ignore.case=T), ]

tmpSIC <- c('541105', '541101','531110')
dataGroceryAll.clip <- dataGroceryStoreAll[grepl(paste(tmpSIC, collapse = '|'),dataGroceryStoreAll$Primary.SIC.Code,ignore.case=T), ]
dataSnapAll.clip <- dataSnapAll[grepl(paste(tmpCounty2, collapse = '|'),dataSnapAll$County,ignore.case=T), ]

dataSnap <- merge(dataGroceryAll.clip,dataSnapAll.clip, by.x = "Address", by.y = "Address")

# Merging shape files and datafiles
shapeCensus.clip@data$id2 <- paste(shapeCensus.clip@data$STATEFP, shapeCensus.clip@data$COUNTYFP, shapeCensus.clip@data$TRACTCE, sep = '')
shapeCensus.f <- fortify(shapeCensus.clip,region = "id2")
shapeCensus.f <- merge(shapeCensus.f,dataCensus.Clip, by.x = "id", by.y = "Id2.x")

# Changing column names because they are asinine 
names(shapeCensus.f) <- c("id","long","lat","order","hole","piece","group","Id","Geography","Households", "Margin",
                          "Below_Poverty","NotSNAPHouseholds","NotSNAPFamilies","NotSNAPMarried","NotSNAPNonFamily",
                          "Households_10k","Families_10k","Married_10k","NonFamily_10k","Households_10_15k",
                          "Familes_10_15k","Married_10_15k","NonFamily_10_15k","Households_15_25k","Families_15_25k",
                          "Married_15_25k","NonFamily_15_25k","Households_25_35k","Families_25_35k","Married_25_35k",
                          "NonFamily_25_35k","Households_35_50k","Familes_35_50k","Married_35_50k","NonFamily_35_50k",
                          "Households_50_75k","Families_50_75k","Married_50_75k","NonFamily_50_75k","Households_75_100k",
                          "Families_75_100k","Married_75_100k","NonFamily_75_100k","Households_100_150k","Families_100_150k",
                          "Married_100_150k","NonFamily_100_150k","Households_150_200k","Families_150_200k",
                          "Married_150_200k","NonFamily_150_200k","Households_200k","Family_200k","Married_200k",
                          "NonFamily_200k","Households_Median","Families_Median","Married_Median","NonFamily_Median",
                          "Households_Mean","Families_Mean","Married_Mean","NonFamily_Mean")

### Creating meaningful data
shapeCensus.f$Per.Poverty <- (shapeCensus.f$Below_Poverty)/(shapeCensus.f$Households)*100
shapeCensus.f$Per.Poverty[which(shapeCensus.f$Per.Poverty>100)] <- 0
shapeCensus.f$Per.Living.Wage <- (shapeCensus.f$Households_10k+ shapeCensus.f$Households_10_15k + 
                                    shapeCensus.f$Households_15_25k + shapeCensus.f$Households_25_35k + 
                                    shapeCensus.f$Households_35_50k)/(shapeCensus.f$Households)*100
shapeCensus.f$Per.Living.Wage[which(shapeCensus.f$Per.Living.Wage>100)] <- 0
#shapeCensus.f$Rank.Poverty[which(shapeCensus.f$Per.Living.Wage>100)] <- 0
#shapeCensus.f$Rank.Living.Wage[which(shapeCensus.f$Per.Living.Wage>100)] <- 0


### Plotting data
Map <- ggplot(shapeCensus.f, aes(long, lat, group = group))  + geom_polygon() 

# get the bounding box of the map and get slightly larger box
b<- bbox(shapeCensus.clip)
b[1,] <- (b[1,] - mean(b[1,])) * 1.05 + mean(b[1,])
b[2,] <- (b[2,] - mean(b[2,])) * 1.05 + mean(b[2,])

# getting a google map of the area
#MapGG <- ggmap(get_map(location = b)) #comment back in to get google maps
MapGG <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T, zoom = 11))
# plotting the data over the google map
MapGG + geom_polygon(data = shapeCensus.f, aes(x = long, y = lat, group = group, fill = Per.Living.Wage),color = "black",size = 0.25, alpha = 0.75) + 
  geom_point(aes(x = Longitude, y = Latitude, color = dataSchool14.clip$Per.Lunch), data = dataSchool14.clip) + 
  scale_color_gradient(low = "blue", high = "red", name = "Percent on Free Lunch")
  
MapGG + geom_polygon(data = shapeCensus.f, aes(x = long, y = lat, group = group, fill = Per.Living.Wage),color = "black",size = 0.25, alpha = 0.75) + 
  geom_point(aes(x = ZZLon, y = ZZLat, color = dataFeedAll.clip$ZZTotal.Meals ), data = dataFeedAll.clip) + 
  scale_color_gradient(low = "blue", high = "red",name = "Total Meals Served")

MapGG + geom_polygon(data = shapeCensus.f, aes(x = long, y = lat, group = group, fill = Per.Living.Wage),color = "black",size = 0.25, alpha = 0.75) + 
  geom_point(aes(x = Longitude, y = Latitude, color = "blue"), data = dataSchool14.clip) +
  geom_point(aes(x = ZZLon, y = ZZLat, color = "red" ), data = dataFeedAll.clip) + 
  geom_point(aes(x = ZZLon, y = ZZLat, color = "red" ), data = dataFeedAll.clip) + 
  scale_color_discrete(name  ="Location", labels=c("Schools", "Feeding Sites"));

MapGG + geom_polygon(data = shapeCensus.f, aes(x = long, y = lat, group = group, fill = Per.Living.Wage),color = "black",size = 0.25, alpha = 0.75) + 
  geom_point(aes(x = Longitude, y = Latitude, size = dataSchool14.clip$Per.Lunch, color = "blue"), data = dataSchool14.clip) + 
  scale_size_continuous(name = "Percent on Free Lunch") + 
  geom_point(aes(x = Longitude.x, y = Latitude.x, color = "red" ), data = dataSnap) + 
  geom_point(aes(x = lon, y = lat, color = "green" ), data = dataPantryAll) + 
  scale_color_discrete(name = "Location",labels = c("Schools","SNAP Grocery","Pantry"))


# plotting location of SNAP 
MapGG + geom_point(aes(x = Longitude, y = Latitude ), data = dataSnapAll, alpha = 0.8)

# code to add size to points, unfortunately have not found anything worthwhile
#MapGG + geom_point(aes(x = Longitude, y = Latitude, size = [] ), data = dataSnapAll, alpha = 0.8) + scale_color_gradient(low = "blue", high = "red")





############################################################################################
### Manipulating School data 
############################################################################################

#
#proj4string(shapeCensus) <- CRS("+init=epsg:27700")
#shapeCensus <- spTransform(shapeCensus, CRS("+init=epsg:4326"))

# Merge Meta and Performance data into 1 dataframe 
dataSchool14All <- merge(dataSchoolsMetaAll, dataSchoolsPerf14All, by.x = "School.Code", by.y = "School.Code")# clip the data I want 

# clip the counties I want
tmpCounty2 <- c('DURHAM','JOHNSTON','ORANGE','WAKE')
shapeSchool.clip <- shapeSchool[grepl(paste(tmpCounty2, collapse = '|'),shapeSchool$NAME,ignore.case=T), ]
tmpCounty3 <- c('3701260', '3702370','3703480','3704720')
shapeSchoolD.clip <- shapeSchoolD[grepl(paste(tmpCounty3, collapse = '|'),shapeSchoolD$leaid,ignore.case=T), ]

dataSchool14.clip <- dataSchool14All[grepl(paste(tmpCounty2, collapse = '|'),dataSchool14All$County,ignore.case=T), ]
dataSchool14.clip<- dataSchool14.clip[which(!is.na(dataSchool14.clip$Overall.Achievement.Score.99)),]
dataSchool14.clip$School.ID <- dataSchool14.clip$School.ID/100000
dataSchool14.clip2 = dataSchool14.clip[,c(1, 2, 4,7,11:16,19,23,30)]

### Creating meaningful data
dataSchool14.clip$Per.Lunch <- 100*dataSchool14.clip$Total.Free.and.Reduced.Lunch/dataSchool14.clip$Total.Students


# Merging shape files and datafiles
shapeSchool.f <- fortify(shapeSchool.clip,region = "GEOID")
shapeSchool.f <- merge(shapeSchool.f,dataSchool14.clip, by.x = "id", by.y = "School.ID")

shapeSchoolD.f <- fortify(shapeSchoolD.clip,region = "ncessch")
shapeSchoolD.f <- merge(shapeSchoolD.f,shapeSchoolD.clip@data, by.x = "id", by.y = "ncessch")
shapeSchoolD.f <- merge(shapeSchoolD.f,dataSchool14.clip2, by.x = "leaid", by.y = "School.ID")


### Plotting data
Map <- ggplot(shapeSchool.f, aes(long,lat, group = group, fill = Total.Students))  + geom_polygon() 

# get the bounding box of the map and get slightly larger box
b<- bbox(shapeSchool.clip)
b[1,] <- (b[1,] - mean(b[1,])) * 1.05 + mean(b[1,])
b[2,] <- (b[2,] - mean(b[2,])) * 1.05 + mean(b[2,])

# getting a google map of the area
#MapGG <- ggmap(get_map(location = b)) #comment back in to get google maps
MapGG2 <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T, zoom = 11))
# plotting the data over the google map
MapGG2 +  geom_point(aes(x = Longitude, y = Latitude, size = dataSchool14.clip$Per.Lunch), data = dataSchool14.clip) #+ 
#scale_color_gradient(low = "blue")
MapGG + geom_point(aes(x = Longitude, y = Latitude, color = SPG.Score, size = dataSchool14.clip$Per.Lunch), data = dataSchool14.clip) + 
  scale_color_gradient(low = "blue", high = "red")


# code to add size to points, unfortunately have not found anything worthwhile
#MapGG + geom_point(aes(x = Longitude, y = Latitude, size = [] ), data = dataSnapAll, alpha = 0.8) + scale_color_gradient(low = "blue", high = "red")

# School performance vs student lunch 
p<- ggplot(dataSchool14.clip, aes(Per.Lunch, SPG.Score)) #+ geom_point(aes(color = Total.Students, size = Total.Free.and.Reduced.Lunch))

p2<- ggplot(dataSchool14.clip, aes(Per.Lunch, Overall.Achievement.Score.99)) #+geom_point(aes(color = Total.Students, size = Total.Free.and.Reduced.Lunch))




