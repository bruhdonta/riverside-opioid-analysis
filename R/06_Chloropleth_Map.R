source("R/04_Normalized_Severity_Scores.R")

library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(RColorBrewer)
library(htmltools)

options(tigris_use_cache = TRUE)

# Ensure ZIPs are 5-character strings (important for joining)
severity_by_zip <- severity_by_zip %>%
  mutate(ZIP = as.character(ZIP))

# Pull ZCTAs (this is large; caching helps)
zip_shapes <- tigris::zctas(year = 2020) %>%
  st_transform(crs = 4326)

# Keep only the ZCTAs present in your severity table
riverside_zips <- zip_shapes %>%
  filter(ZCTA5CE20 %in% severity_by_zip$ZIP)

# Join geometry + severity
map_data <- left_join(
  riverside_zips,
  severity_by_zip,
  by = c("ZCTA5CE20" = "ZIP")
)

# Binned palette is easier to read in print
pal <- colorBin(
  palette = brewer.pal(9, "YlOrRd"),
  domain  = map_data$Normalized_Severity,
  bins    = 7,
  na.color = "transparent"
)

opioid_map <- leaflet(map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(Normalized_Severity),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.75,
    label = ~lapply(
      paste0(
        "ZIP: ", ZCTA5CE20,
        "<br>Normalized severity: ",
        ifelse(is.na(Normalized_Severity), "NA", round(Normalized_Severity, 3))
      ),
      HTML
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#444444",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Normalized_Severity,
    title = HTML("Opioid severity score<br><small><em>Higher = more severe</em></small>"),
    opacity = 1
  ) %>%
  addControl(
    html = "<strong>Opioid Severity by ZIP Code (Riverside County)</strong>",
    position = "topright"
  ) %>%
  fitBounds(
    lng1 = st_bbox(map_data)$xmin,
    lat1 = st_bbox(map_data)$ymin,
    lng2 = st_bbox(map_data)$xmax,
    lat2 = st_bbox(map_data)$ymax
  )

# Exports static PNG for manuscript
# IMPORTANT NOTE: mapshot may require webshot2 and Chrome/Chromium installed.
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

mapview::mapshot(
  opioid_map,
  file = "figures/Figure_Opioid_Severity_Map.png",
  vwidth = 1600,
  vheight = 1200,
  delay = 1
)
