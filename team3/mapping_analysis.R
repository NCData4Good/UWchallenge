#############
#NCData4Good#
#############

rm(list = ls(all=T))

#load required packages
require(sp)
require(rgdal)
require(ggplot2)
require(gpclib)
require(rgeos)
require(maptools)

#load pantries data
pantries <- read.table("pantries.csv", header = TRUE, sep = ",")

#get rid of unnecessary columns
#address2, city, state, phone
pantries <- pantries[,-c(3,4,5,7)]

acsdata<- read.table("./ACS_13_5YR_S2201/ACS_13_5YR_S2201_with_ann.csv",
                     skip = 1, header = T, sep = ",")

#get rid of unnecessary columns
#address2, city, state, phone
acsdata <- acsdata[,c(2,3,4,6,16,18,22,24,82,84)]
write.table(acsdata, file = "./ACS_13_5YR_S2201/acsdata.csv", sep = ",",
            row.names = FALSE)

#read the census tract shapefile
shapefile <- readOGR("./tl_2015_37_tract/",layer = "tl_2015_37_tract",
                     stringsAsFactors = FALSE)

mergeddata<-merge(shapefile, acsdata, by.x="GEOID",by.y="Id2",all.x=FALSE)
plot(mergeddata)

rm(shapefile)
rm(acsdata)

gpclibPermit()
fortified <- fortify(mergeddata, region = "GEOID")

write.table(fortified, file = "./plottable_census_tracts_and_pantries.csv", sep = ",",
            row.names = FALSE)

g <- ggplot(fortified) +
        geom_polygon(aes(x=long,y=lat, group = group)) +
        geom_point(data = pantries, aes(x=lon,y=lat), col = "green")
g

anomalies <- pantries[which(pantries[,5] > -78),]
rm(anomalies)

#extrapantries <- read.table("./agency_programs_cleaned.csv", sep = ",",
 #                           header = TRUE, fill = TRUE)
#extrapantries <- extrapantries[,c(1,2,5,9,8,7,6)]
#extrapantries <- extrapantries[which(extrapantries[,7]=="FOODPANTRY"),]
#rownames(extrapantries) <- NULL
#extrapantries <- extrapantries[,-7]

eucdist<-function(lat1,long1,lat2,long2)
{
        d = sqrt((lat1-lat2)^2 + (long1-long2)^2)
        return(d)
}

temp<-mergeddata@data
rownames(temp)<-NULL
temp<-cbind(temp,rep(0,300))
temp[,"INTPTLAT"]<-as.numeric(temp[,"INTPTLAT"])
temp[,"INTPTLON"]<-as.numeric(temp[,"INTPTLON"])
names(temp)[22] <- "distance to closest pantry"

for(i in 1:nrow(temp))
{
        mindist = 10000
        for(j in 1:nrow(pantries))
        {
                mindist = min(mindist, eucdist(pantries[j,"lat"],
                                               pantries[j,"lon"],
                                               temp[i,"INTPTLAT"],
                                               temp[i,"INTPTLON"]))
        }
        temp[i,22] = mindist
}

temp[,15]<-as.numeric(temp[,15])
temp[,23]<-temp[,15]*temp[,22]
temp[,24]<-temp[,15]*temp[,22]/temp[,14]

temp <- temp[,c(1,23,24)]
most_affected <- sort(temp[,3], decreasing = TRUE)[1:5]
temp2 <- temp
temp2[which(!(temp[,3] %in% most_affected)),2] <- NaN

newfort <- merge(fortified,temp,by.x="id",by.y="GEOID")
names(newfort)[8] <- "Distance_metric"
names(newfort)[9] <- "Distance_metric_by_proportion"

h <- ggplot(newfort) +
        geom_polygon(aes(x=long,y=lat, group = group, fill = Distance_metric)) +
        geom_point(data = pantries, aes(x=lon,y=lat, col = county)) +
        ggtitle("Distance of food pantries from population in need")
h

j <- ggplot(newfort) +
        geom_polygon(aes(x=long,y=lat, group = group, fill = Distance_metric_by_proportion)) +
        geom_point(data = pantries, aes(x=lon,y=lat, col = county)) +
        ggtitle("Distance of food pantries from population in need")
j

newfort2 <- merge(fortified,temp2,by.x="id",by.y="GEOID")
names(newfort2)[8] <- "Distance_metric"

k <- ggplot(newfort2) +
        geom_polygon(aes(x=long,y=lat, group = group, fill = Distance_metric)) +
        geom_point(data = pantries, aes(x=lon,y=lat, col = county)) +
        ggtitle("Top 5 most affected census tracts")
k
