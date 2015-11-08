# 2015-11-07 szelenka@gmail.com

# logistics of distribution of fresh food
# seasonality and path of food to the user in need
# 
# farms -> food bank
# food bank -> pantries -> people (i.e. block group)
# 
# department of argriculture for what fresh food is in season at the time

wd = "/Users/szelenka/Documents/_svn/Data4Good/2015-11-07/"
setwd(wd)
source('../load_datacrunch_data.R')
# 
# location of farms
# farmers markets
# food bank, units received, distributed, etc.
# pantries served over time
# median income (dollars per person in household, not SNAP benefits)
# # rdist.earth

library(ggplot2)
library(RColorBrewer)
library(grid)
#library(map)
library(mapproj)
library(gridExtra)
library(stringr)

# filter out to only the counties we care about
counties <- c("wake","durham","orange","johnston")
new.nc_export <- nc_export[tolower(nc_export$County) %in% counties,]
new.foodbank <- foodbank[tolower(foodbank$city) %in% c("raleigh","durham"),]
new.pantries <- pantries[tolower(pantries$county) %in% counties,]
new.grocery <- grocery_store[grocery_store$ZIP.Code %in% unique(new.pantries$zip) & grepl('Grocers|Dairy|Food|Deli',grocery_store$Primary.SIC.Description),]

# correct the foodbank dataset
new.foodbank <- new.foodbank[new.foodbank$food.bank != "Inter-Faith Food Shuttle",]
new.foodbank <- rbind(new.foodbank, data.frame(food.bank='Food Bank of Central & Eastern North Carolina', address=NA, address2=NA, city='Durham', state='NC', zip=NA, lat=35.97745, lng=-78.8715))

# merge lon/lat into a single data.frame
df <- data.frame(Type="Farmers Market", volume = 3, name = new.nc_export$MarketName, x = as.numeric(new.nc_export$x), y = as.numeric(new.nc_export$y))
df <- rbind(df,data.frame(Type="Food Bank", volume = 5,  name = new.foodbank$food.bank, x = as.numeric(new.foodbank$lng), y = as.numeric(new.foodbank$lat)))
df <- rbind(df,data.frame(Type="Food Pantry", volume = 2, name = new.pantries$foodpantry, x = as.numeric(new.pantries$lon), y = as.numeric(new.pantries$lat)))
df <- rbind(df,data.frame(Type="Grocery Store", volume = 1, name = new.grocery$Company.Name, x = as.numeric(new.grocery$Longitude), y = as.numeric(new.grocery$Latitude)))
df$Type <- factor(df$Type)

# adjust for fIPS state code
new.fips <- fips.nc.counties[tolower(fips.nc.counties$County) %in% counties,]
new.fips$StCounty_fips_code <- as.numeric(new.fips$StCounty_fips_code) - 37000
new.fips[9,"StCounty_fips_code"] = "063"
new.census <- censustract[censustract$COUNTYFP %in% new.fips$StCounty_fips_code,]

# load the county map for what we care about
map.data <- map_data("county")
map.counties <- map.data[map.data$region == "north carolina" & map.data$subregion %in% counties,]

# plot the locations of banks, pantries, and grocery stores
food.plot <- ggplot(map.counties) + 
  coord_map( "polyconic" ) + 
  scale_colour_brewer(palette="Dark2") +
  geom_map(
    aes(map_id = region), 
    map = map.counties, 
    fill = "white", 
    color = "grey20", 
    size = 0.25) + 
  expand_limits(
      x = map.counties$long, 
      y = map.counties$lat) + 
  geom_path(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion %in% c("durham","orange"),],
    colour = "#de0079",
    alpha = 0.75) +
  geom_path(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion %in% c("wake","johnston"),],
    colour = "#df9c0a",
    alpha = 0.75) +
  geom_path(
    aes(x=long, y=lat, group=group),
    data = new.census,
    alpha=0.05,
    colour = "blue") +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    plot.background = element_blank(), 
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  labs(
    title = "Location of Fresh and Nutritious Food Sources"
  ) +
  geom_point(
    data = df[df$Type %in% c("Food Bank", "Food Pantry", "Grocery Store"),], 
    aes(x = x, y = y, shape = Type, color=Type),
    alpha = 0.35) +
  geom_point(
    data = df[df$Type == "Food Bank",], 
    aes(x = x, y = y),
    shape = 21,
    color = "black",
    fill = "#1b8f64",
    size = 7) 
ggsave(food.plot, file="food.plot.png", height=4)


# farmers market seasonality availability
fm <- read.csv("FarmersMarketCrops.csv", header=T)
fm <- fm[fm$season != "0",c("marketname","x","y","seasons","season")]
fm$size <- round(10*(fm$seasons / max(fm$seasons)))

# number of farms in county
farms <- read.csv("Ag_census.csv", header=T)
farms$N_farms <- as.numeric(str_replace(farms$N_farms,",",""))
farms$Land_in_farms_acre <- as.numeric(str_replace(farms$Land_in_farms_acre, ",",""))
farms$norm <- farms$Land_in_farms_acre / max(farms$Land_in_farms_acre)

# plot the availability of farms, farmers markets
farm.plot <- ggplot(map.counties) + 
  coord_map( "polyconic" ) + 
  scale_colour_brewer(palette="Dark2") +
  geom_map(
    aes(map_id = region), 
    map = map.counties, 
    fill = "white", 
    color = "grey20", 
    size = 0.25) + 
  expand_limits(
    x = map.counties$long, 
    y = map.counties$lat) + 
  geom_polygon(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion == "durham",],
    fill = "#fddfc5",
    alpha = farms[tolower(farms$County) == "durham","norm"]
  ) +
  geom_polygon(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion == "orange",],
    fill = "#fddfc5",
    alpha = farms[tolower(farms$County) == "orange","norm"]
  ) +
  geom_polygon(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion == "wake",],
    fill = "#fddfc5",
    alpha = farms[tolower(farms$County) == "wake","norm"]
  ) +
  geom_polygon(
    aes(x=long, y=lat, group=subregion),
    data = map.counties[map.counties$subregion == "johnston",],
    fill = "#fddfc5",
    alpha = farms[tolower(farms$County) == "johnston","norm"]
  ) +
  geom_path(
    aes(x=long, y=lat, group=group),
    data = new.census,
    alpha=0.05,
    colour = "blue") +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    plot.background = element_blank(), 
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  labs(
    title = "Farm Density & Location of Farmers Markets with available Fresh Food"
  ) +
  geom_point(
    data = fm, 
    aes(x = x, y = y, size = seasons, color=season),
    alpha = 0.75) 

#plots <- grid.arrange(food.plot, farm.plot)
ggsave(farm.plot, file="farm.plot.png", height=4)
