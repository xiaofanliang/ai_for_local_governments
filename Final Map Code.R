library(dplyr)
library(ggplot2)
library(ggmap)
library(readxl)
library(sf)
library(tidyr)
library(leaflet)
library(tmaptools)

### LEAFLET FINAL CODE

data <- read_excel("AI policy analysis_heatmap.xlsx")

detail_size <- c("Low" = 10000, "Medium" = 30000, "High" = 60000)

data$`Level of detail` <- as.character(data$`Level of detail`)

data2 <- data %>%
  mutate(
    CircleSize = unname(detail_size[`Level of detail`]),
    CircleColor = as.character(`Document sentiment`),
    PolicyLink = ifelse(!is.na(Policy) & Policy != "", paste0("<a href='", Policy, "' target='_blank'>Policy: Click Here to Read More</a><br>"), ""),
    PlanLink = ifelse(!is.na(Plan) & Plan != "", paste0("<a href='", Plan, "' target='_blank'>Plan: Click Here to Read More</a>"), "")
  )

leaflet(data2) %>%
  addTiles() %>%
  addCircles(
    lng = ~Longitude, 
    lat = ~Latitude, 
    radius = ~CircleSize,
    color = ~CircleColor,
    fillOpacity = 1,
    popup = ~paste(
      "<b>", City, "</b><br>",
      `Pop Up`, "<br>", "<br>",
      "<b>Application of Technology:</b> ", `Application of technology?`, "<br>",
      "<b>Regulation of Technology:</b> ", `Regulation of Technology?`, "<br>", "<br>",
      PolicyLink, PlanLink
    )
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Note: The Size of Circles represent the Level of Detail of the Policy/Plan Document",
    labels = c("The larger the circle, the higher the level of detail, and vice versa."),
    colors = c("transparent"),
    opacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Document Sentiment",
    colors = c("Red", "Orange", "Yellow", "Green"),
    labels = c("Negative", "Cautious", "Neutral", "Positive"),
    opacity = 1
  )


######### TMAP STATIC MAP

data <- read_excel("AI policy analysis_heatmap.xlsx")

us_shapefile <- st_read("States_shapefile.shp")

data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

detail_size <- c("Low" = 10000, "Medium" = 30000, "High" = 60000)

data_sf <- data_sf %>%
  mutate(
    CircleSize = detail_size[`Level of detail`],
    CircleColor = as.character(`Document sentiment`)
  )

tmap_mode("plot")

tm_shape(us_shapefile) +
  tm_fill(col = "lightblue", border.alpha = 0.7) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_shape(data_sf) +
  tm_bubbles(
    size = "CircleSize",
    col = "CircleColor",
    border.col = "black",
    border.alpha = 0.5,
    alpha = 0.7
  ) +
  tm_text("City", size = 0.6, col = "black", auto.placement = TRUE) +
  tm_compass(
    type = "8star", ##or 4star, arrow
    position = c("right", "bottom"),
    size = 2
  ) +
  tm_scale_bar(
    position = c("right", "bottom"),
    breaks = c(0, 200, 400, 600)
  ) +
  tm_layout(
    main.title = "AI Policy in the US (City Level)",
    main.title.position = "center",
    legend.title.size = 0.8,
    legend.text.size = 0.6,
    legend.position = c("left", "bottom")
  )


