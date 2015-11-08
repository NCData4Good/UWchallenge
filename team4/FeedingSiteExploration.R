setwd("E:\\Data4Good DataSets\\Rcode\\")
# Import more libraries some more than requires
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(shiny)
library(leaflet)
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(RColorBrewer)

# Read the places in NC dataset which would be potential feeding sites
placesNCData <- read.csv(file= "..//NCdata.csv", header= TRUE, na.string="NA", stringsAsFactor=FALSE)

#Schools
#Camps
#Churches
#Community centers
#Parks and playgrounds
#Public housing complexes
#Other public sites where kids gather

# Cleanup some data
placesNCData<-placesNCData[tolower(placesNCData$PRIMARY_LAT_DMS)!="unknown",]
#Total Meals provided as an indicator for efficacy of 
#Schools
#Camps
#Churches
#Community centers
#Parks and playgrounds
#Public housing complexes
#Other public sites where kids gather



# Read Maureen's data for feeding Sites in NC participating in the Summer program
feedingData <- read.csv("..//feeding-sites.csv", header= TRUE, na.string="NA", stringsAsFactors = FALSE)
feedingData$Pop..Density.SE_T002_002 <- as.numeric(feedingData$Pop..Density.SE_T002_002)
feedingData$Land.Area.SE_T002_003 <- as.numeric(feedingData$Land.Area.SE_T002_003)
feedingData$Total.Days.of.Operation <- as.numeric(feedingData$Total.Days.of.Operation)
feedingData$TotalMealsPC <- as.numeric(feedingData$TotalMealsPC)

#Read the shape data from the census shapes dataset for graphing
census.shapes <- readOGR("..//tl_2015_37_tract/tl_2015_37_tract.shp",layer="tl_2015_37_tract")
str(census.shapes@data)
census.shapes <- census.shapes[census.shapes$COUNTYFP %in% c("063", "183", "135", "101"),]
names(census.shapes@data) <-(names(census.shapes@data))


#censusTract_sp<- as(census.shapes,"SpatialPolygons")
#names(cencusTract_sp)

#Investigating the data
hist(feedingData$Total.Days.of.Operation[feedingData$ZZProgram.Type=="SSO"], breaks=50, main="Days of operation", xlab="Days")

par(mfrow=c(3,3))
hist(feedingData$ZZTotal.Meals, breaks=50, main="Total Meals", xlab="Total Meals")
hist(feedingData$ZZTotal.Meals[feedingData$ZZProgram.Type=="SSO"], xlab="Total Meals", breaks=25, main="SSO")
hist(feedingData$ZZTotal.Meals[feedingData$ZZProgram.Type=="SFSP"], xlab="Total Meals", breaks=25, main= "SFSP")

#par(mfrow=c(1,3))
hist(feedingData$TotalMealsPC, breaks=50, main="Total Meals Per capita", xlab="Total Meals PC")
hist(feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SSO"], xlab="Total Meals PC", breaks=25, main="SSO")
hist(feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SFSP"], xlab="Total Meals PC", breaks=100, main= "SFSP")

#par(mfrow=c(1,3))
hist(feedingData$Pop..Density.SE_T002_002, breaks=50, main="Population Density", xlab="Density")
hist(feedingData$Pop..Density.SE_T002_002[feedingData$ZZProgram.Type=="SSO"], xlab="Density", breaks=25, main="SSO")
hist(feedingData$Pop..Density.SE_T002_002[feedingData$ZZProgram.Type=="SFSP"], xlab="Density", breaks=25, main= "SFSP")

#par(mfrow=c(1,2))
#hist(feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SSO"], xlab="Total Meals PC", breaks=25, main="SSO")
#hist(feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SFSP"], xlab="Total Meals PC", breaks=25, main= "SFSP")
percentPop <- feedingData$Pop..Density.SE_T002_002[feedingData$ZZProgram.Type=="SSO"]/max(feedingData$Pop..Density.SE_T002_002[feedingData$ZZProgram.Type=="SSO"])
plot( feedingData$Total.Days.of.Operation[feedingData$ZZProgram.Type=="SSO"], 
                        feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SSO"],
                        xlab="Days of operation",ylab="Total Meals PC")

correlation<-cor(feedingData$Total.Days.of.Operation[feedingData$ZZProgram.Type=="SSO"],  feedingData$TotalMealsPC[feedingData$ZZProgram.Type=="SSO"])
text(150,25,labels=paste("Correlation: ",formatC(correlation,format='f',digits=2)))

#Estimate Indiocators

#Assumptions
#1. Travel Distance -


#if Total.Days.of.Operation, assume whole summer term ie 90 ? or max days?
#feedingData[is.na(feedingData$Total.Days.of.Operation),]$Total.Days.of.Operation<-90


#feedingData$FoodSupplyIndicator <- feedingData$ZZTotal.Meals/(feedingData$Pop..Density.SE_T002_002*feedingData$Total.Days.of.Operation)
# FoodSupplyIndicator-
feedingData$FoodSupplyIndicator <- feedingData$TotalMealsPC/ max(feedingData$TotalMealsPC)
# SustainabilityIndicator - Normalized Log Total.Days.of.Operation
feedingData$SustainabilityIndicator[feedingData$ZZProgram.Type=="SSO"] <- feedingData$Total.Days.of.Operation[feedingData$ZZProgram.Type=="SSO"]/max(feedingData$Total.Days.of.Operation[feedingData$ZZProgram.Type=="SSO"]) #assuming 90 summer days
# Need Indicator 
distanceWeights <- c(1,2,3,4,5)
needfulnessOfSite <-  sum(feedingData[,148:152] * distanceWeights)/feedingData$Land.Area.SE_T002_003
feedingData$NeedIndicator <- needfulnessOfSite / max(needfulnessOfSite)

par(mfrow=c(3,3))
hist(feedingData$FoodSupplyIndicator*100, breaks=200, main="Supply Indicator",xlab="Supply")
hist(log(feedingData$FoodSupplyIndicator*100), breaks=50, main="Log Supply Indicator",xlab="Supply")
boxplot(log(feedingData$FoodSupplyIndicator))


hist(feedingData$NeedIndicator*100, breaks=200, main="Demand Indicator",xlab="Demand")
hist(log(feedingData$NeedIndicator*100), breaks=50, main="Log Demand Indicator",xlab="Demand")
boxplot(log(feedingData$NeedIndicator))


hist(feedingData$SustainabilityIndicator[feedingData$ZZProgram.Type=="SSO"] * 100, breaks=200, main="Days of Operation",xlab="Sustainability")
hist(log(feedingData$SustainabilityIndicator[feedingData$ZZProgram.Type=="SSO"] * 100), breaks=50, main="Log Days Indicator" ,xlab="Sustainability")
boxplot(feedingData$SustainabilityIndicator[feedingData$ZZProgram.Type=="SSO"])



feedingData$FoodSupplyIndicatorLogZscore <- (feedingData$FoodSupplyIndicator - mean(feedingData$FoodSupplyIndicator))/sd(feedingData$FoodSupplyIndicator)
feedingData$NeedIndicatorLogZScore <- (feedingData$NeedIndicator - mean(feedingData$NeedIndicator)) / sd(feedingData$NeedIndicator)

feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"] <- rank((feedingData$FoodSupplyIndicator[feedingData$ZZProgram.Type=="SFSP"]+feedingData$FoodSupplyIndicator[feedingData$ZZProgram.Type=="SFSP"]),ties.method = "min")
feedingData$RankSSO[feedingData$ZZProgram.Type=="SSO"] <- rank((feedingData$FoodSupplyIndicator[feedingData$ZZProgram.Type=="SSO"]
                             +feedingData$FoodSupplyIndicator[feedingData$ZZProgram.Type=="SSO"]
                             +feedingData$SustainabilityIndicator[feedingData$ZZProgram.Type=="SSO"]),ties.method = "min")
idx <- feedingData$RankSSO %in% c(613,614,615,616,617)

feedingData$Numerical.Code[idx]
feedingData$PubTranAvail[idx]
idx <- feedingData$RankSFSP %in% c(length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"])-5,length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"])-4,length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"])-3,
                                   length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"])-2,
                                   length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"]))
feedingData$Numerical.Code[idx]
feedingData$PubTranAvail[idx]
#Normalize
#feedingData$FoodSupplyIndicator <- normalizeIndicatory(feedingData$FoodSupplyIndicator)

supplySustaindf<-feedingData[complete.cases(feedingData$FoodSupplyIndicator,feedingData$SustainabilityIndicator),c(244,245)]
cor(supplySustaindf)
cor(supplySustaindf[sapply(supplySustaindf, is.numeric)],use = "complete")


supplyDemanddf<- feedingData[,c(244,246)]
cor(supplyDemanddf[sapply(supplyDemanddf, is.numeric)])

supplyDemanddf<- feedingData[,c(244,246)]
cor(supplyDemanddf[sapply(supplyDemanddf, is.numeric)])

needSustaindf<- feedingData[,c(245,246)]
cor(needSustaindf[sapply(needSustaindf, is.numeric)],use="complete")

#par(mfrow=c(3,1))
#plot(supplySustaindf,col = c("#FF0000","#FFF000"))
#plot(supplyDemanddf,col = c("#FF0000","#FFF000"))
#plot(needSustaindf,col = c("#FF0000","#FFF000"))

#Set the color pallette 
#example: cols <- colorRampPalette(brewer.pal(8,"Dark2"))(10)
set1 <- colorRampPalette(brewer.pal(8,"Set1"))(length(feedingData$RankSFSP[feedingData$ZZProgram.Type=="SFSP"]))
set3 <- colorRampPalette(brewer.pal(8,"Set3"))(length(feedingData$RankSSO[feedingData$ZZProgram.Type=="SS0"]))
pl_mapdf = data.frame(Lat = placesNCData$PRIM_LAT_DEC, Long = placesNCData$PRIM_LONG_DEC, Title=paste (paste(placesNCData$FEATURE_ID ,placesNCData$MAP_NAME), placesNCData$FEATURE_CLASS))
#feed_mapdf = data.frame(Lat= feedingData$ZZLat,Long = feedingData$ZZLon ,Mag = feedingData$TotalMealsPC)
feed_mapdf = data.frame(Program= feedingData$ZZProgram.Type , County = feedingData$ZZSTCOFIPS , Lat= feedingData$ZZLat,Long = feedingData$ZZLon ,Mag = feedingData$Land.Area.SE_T002_003,RankSFSP = feedingData$RankSFSP,RankSSO=feedingData$RankSSO )
#feed_mapdf <-feed_mapdf[complete.cases(feed_mapdf),]
#leaflet()
selectCounties<- as.integer(c(37063, 37101, 37135, 37183))
feed_mapdf <- feed_mapdf[feed_mapdf$County %in% selectCounties,]

SFSPdf <- feed_mapdf[feed_mapdf$Program=="SFSP",c("Lat","Long","RankSFSP")]
SSOdf <- feed_mapdf[feed_mapdf$Program=="SSO",c("Lat","Long","RankSFSP")]
m<- leaflet(census.shapes) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
                stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
                #color = ~colorFactor(brewer.pal(4, "Paired"), census.shapes$NAME)(NAME)
                color = "#B8B8B8"
        ) %>%
        addCircles(lng=SFSPdf$Long, lat=SFSPdf$Lat, color= set1[SFSPdf$RankSFSP])
        #addCircles(lng=feed_mapdf$Long, lat=feed_mapdf$Lat, radius=feed_mapdf$Mag*100, color= set3[feed_mapdf$Rank]) %>%
        #addCircles(lng=pl_mapdf$Long, lat=pl_mapdf$Lat, color= "#FF0000", popup=pl_mapdf$Title) 
m

length(feed)
