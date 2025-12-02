source("R/03_Normalized_Severity_Scores.R")

library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(RColorBrewer)
library(htmltools)
library(webshot)
options(tigris_use_cache = TRUE)


# Load ZIP Code Tabulation Areas (ZCTAs)
zip_shapes <- tigris::zctas()
zip_shapes <- st_transform(zip_shapes, crs = 4326)


# Filter for Riverside County ZIPs (92501–92883 range)
riverside_zips <- zip_shapes %>%
  filter(ZCTA5CE20 %in% severity_by_zip$ZIP)


map_data <- left_join(riverside_zips, severity_by_zip,
                      by = c("ZCTA5CE20" = "ZIP"))

pal <- colorNumeric(
  palette = brewer.pal(9, "YlOrRd"),
  domain = map_data$Normalized_Severity,
  na.color = "transparent",
  reverse = FALSE
)

opioid_map <- leaflet(map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(Normalized_Severity),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = ~lapply(paste0("ZIP: ", ZCTA5CE20, "<br>", "Normalized Severity Score: ", round(Normalized_Severity, 3)), HTML)
    
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Normalized_Severity,
    title = HTML("Opioid Severity Score<br><small><em>Higher = More Severe</em></small>"),
    labFormat = labelFormat()
  ) %>%
  addControl(
    html = "<strong>Opioid Severity by ZIP Code (Riverside County)</strong>",
    position = "topright"
  )

mapview::mapshot(
  opioid_map,
  file = "figures/Figure_Opioid_Severity_Map.png",
  vwidth = 1600,
  vheight = 1200,
  delay = 0.5
)

