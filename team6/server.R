server <- function(input, output) {
    output$plot1 <- renderLeaflet({

      sd.subset <- sd.shapes[sd.shapes$schnam == input$school,]
      school.subset <- schools[schools$school.name == input$school,]
      places.subset <- places[places[[input$school]] & places$place.type %in% input$checkboxGroup,]

      ## places.subset <- places.subset[places.subset$place.type %in% input$checkboxGroup,]

      if(nrow(places.subset) == 0){
        leaflet(sd.subset) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~colorFactor(brewer.pal(5, "Set2"), sd.subset$color)(color)
      ) %>%
      addMarkers(lng=~school.subset$longitude,
                 lat=~school.subset$latitude,
                 icon=schoolIcon) %>% addScaleBar(position = "bottomleft")
      } else {
      leaflet(sd.subset) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~colorFactor(brewer.pal(5, "Set2"), sd.subset$color)(color)
      ) %>%
      addMarkers(lng=~school.subset$longitude,
                 lat=~school.subset$latitude,
                 icon=schoolIcon) %>%
      addCircleMarkers(lng=~places.subset$lon,
                 lat=~places.subset$lat,
                 stroke = FALSE, fillOpacity = .9, radius = 5,
                 popup=~paste(places.subset$place.type,
                              "-",
                              places.subset$place.name),
                 color = ~colorFactor(brewer.pal(8, "Set1"), places$place.type)(places.subset$place.type)) %>%
      addScaleBar(position = "bottomleft") %>%
      addLegend("bottomleft", pal = colorFactor(brewer.pal(8, "Set1"), places$place.type), values = ~places.subset$place.type,
                title = "Places",
                opacity = 1
                )
      }
    })

      output$plot2 <- renderLeaflet({

      district.subset <- district.shapes[district.shapes$name == input$district,]
      ## school.subset <- schools[schools$school.name == input$school,]
      places.subset <- places[places[[input$district]] & places$place.type %in% input$checkboxGroup2,]

      ## places.subset <- places.subset[places.subset$place.type %in% input$checkboxGroup,]

      if(nrow(places.subset) == 0){
        leaflet(district.subset) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~colorFactor(brewer.pal(5, "Set2"), district.subset$color)(color) %>%
        addScaleBar(position = "bottomleft")
      ) ## %>%
      ## addMarkers(lng=~school.subset$longitude,
      ##            lat=~school.subset$latitude,
      ##            icon=schoolIcon)
      } else {
      leaflet(district.subset) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~colorFactor(brewer.pal(5, "Set2"), district.subset$color)(color)
      ) %>%
      ## addMarkers(lng=~school.subset$longitude,
      ##            lat=~school.subset$latitude,
      ##            icon=schoolIcon) %>%
      addCircleMarkers(lng=~places.subset$lon,
                       lat=~places.subset$lat,
                       stroke = FALSE, fillOpacity = .9, radius = 5,
                       popup=~paste(places.subset$place.type,
                                    "-",
                                    places.subset$place.name),
                       color = ~colorFactor(brewer.pal(9, "Set1"), places$place.type)(places.subset$place.type)) %>%
      addScaleBar(position = "bottomleft") %>%
      addLegend("bottomleft", pal = colorFactor(brewer.pal(9, "Set1"), places$place.type), values = ~places.subset$place.type,
                title = "Places",
                opacity = 1
                )
      }
    })

    output$schoolStats <- renderText({
      sd.subset <- sd.shapes[sd.shapes$schnam == input$school,]
      school.subset <- schools[schools$school.name == input$school,]

      statsheader <- "<h4>School Stats</h4>"
      distanceheader <- "<h4>Distance Stats</h4>"
      stats <- paste("<span style='font-weight:strong'>",
                           "Total Students",
                           ": </span>", school.subset$total.students)

      stats[2] <- paste("<span style='font-weight:strong'>",
                              "Total Free and Reduced Lunch",
                              ": </span>", school.subset$total.free.and.reduced.lunch)

      stats[3] <- paste("<span style='font-weight:strong'>",
                              "Achievement Score 14-15",
                              ": </span>", school.subset$sp1415.overall.achievement.score.99)

      distances <- paste0("<span style='font-weight:strong'>",
                              "Closest Feeding Site",
                              ": </span>", school.subset$closest.feeding.site, " (", signif(school.subset$distance.to.closest.feeding.site, 1), " miles)")
      distances[2] <- paste0("<span style='font-weight:strong'>",
                              "Closest Church",
                              ": </span>", school.subset$closest.churches, " (", signif(school.subset$distance.to.closest.churches, 1), " miles)")
      distances[3] <- paste0("<span style='font-weight:strong'>",
                              "Closest Convenience Store",
                              ": </span>", school.subset$closest.convenience.stores," (", signif(school.subset$distance.to.closest.convenience.stores, 1), " miles)")
      distances[4] <- paste0("<span style='font-weight:strong'>",
                              "Closest Grocery Store",
                              ": </span>", school.subset$closest.grocery.stores, " (", signif(school.subset$distance.to.closest.grocery.stores, 1), " miles)")
      distances[5] <- paste0("<span style='font-weight:strong'>",
                              "Closest Farmers Market",
                              ": </span>", school.subset$closest.farmers.market, " (", signif(school.subset$distance.to.closest.farmers.market, 1), " miles)")
      distances[6] <- paste0("<span style='font-weight:strong'>",
                              "Closest SNAP Retailer",
                              ": </span>", school.subset$closest.snap.retailers, " (", signif(school.subset$distance.to.closest.snap.retailers, 1), " miles)")
      distances[7] <- paste0("<span style='font-weight:strong'>",
                              "Closest Food Pantry",
                              ": </span>", school.subset$closest.food.pantries, " (", signif(school.subset$distance.to.closest.food.pantries, 1), " miles)")
      distances[8] <- paste0("<span style='font-weight:strong'>",
                              "Closest Bus Stop",
                              ": </span>", school.subset$closest.bus.stops, " (", signif(school.subset$distance.to.closest.bus.stops, 1), " miles)")
      paste(statsheader, paste(stats, collapse="<br>"), distanceheader, paste(distances, collapse="<br>"))
    })


output$lunchPie <- renderPlot({
sd.subset <- sd.shapes[sd.shapes$schnam == input$school,]
school.subset <- schools[schools$school.name == input$school,]

  school1<- school.subset
  slices <- c(school1$total.students-school1$total.free.and.reduced.lunch,school1$total.free.and.reduced.lunch)
  schoolLabels <- c("Full Price","Free & Reduced")
  pct <- round(slices/sum(slices)*100)
  schoolLabels <- paste(schoolLabels,pct)
  schoolLabels <- paste(schoolLabels, "%", sep="")
  pie(slices,schoolLabels,main="Free & Reduced Lunch", col=cols)
})

  output$schoolsDownload <- downloadHandler(
    filename = function() {
      "schools.rda"
    },
    content = function(con) {
      write.csv(schools,con)
    }
  )

  output$placesDownload <- downloadHandler(
    filename = function() {
      "places.rda"
    },
    content = function(con) {
      write.csv(places,con)
    }
  )

  }
