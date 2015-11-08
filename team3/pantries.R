library(leaflet)
m=leaflet()
m=addTiles(m)

pantries = read.delim("./pantries.csv",sep=",",header=T,stringsAsFactors=F)
m <- addMarkers(m, lng=pantries$lon, lat=pantries$lat, popup=pantries$foodpantry)
m