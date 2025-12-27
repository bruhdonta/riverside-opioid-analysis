source(here::here("R", "04_Normalized_Severity_Scores.R"))
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(RColorBrewer)
library(htmltools)
library(stringr)

options(tigris_use_cache = TRUE)

# --- ZIP cleanup (critical) ---
severity_by_zip <- severity_by_zip %>%
  mutate(
    ZIP = as.character(ZIP),
    ZIP = gsub("^ZIP", "", ZIP),
    ZIP = str_pad(ZIP, 5, pad = "0")
  )

zip_shapes <- tigris::zctas(year = 2024) %>%
  st_transform(crs = 4326) %>%
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20))

riverside_zips <- zip_shapes %>%
  filter(ZCTA5CE20 %in% severity_by_zip$ZIP)

map_data <- left_join(riverside_zips, severity_by_zip, by = c("ZCTA5CE20" = "ZIP"))

cat("Rows severity_by_zip:", nrow(severity_by_zip), "\n")
cat("Rows riverside_zips:", nrow(riverside_zips), "\n")
cat("Rows map_data:", nrow(map_data), "\n")
cat("Non-NA Normalized_Severity:", sum(!is.na(map_data$Normalized_Severity)), "\n")

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
      paste0("ZIP: ", ZCTA5CE20, "<br>Normalized severity: ",
             ifelse(is.na(Normalized_Severity), "NA", round(Normalized_Severity, 3))),
      HTML
    ),
    highlightOptions = highlightOptions(weight = 2, color = "#444444", bringToFront = TRUE)
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
  )

# Export (more reliable)
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
htmlwidgets::saveWidget(opioid_map, "figures/opioid_map.html", selfcontained = FALSE)
webshot2::webshot("figures/opioid_map.html", "figures/Figure_Opioid_Severity_Map.png",
                  vwidth = 1600, vheight = 1200, delay = 3)
opioid_map
cat("=== Map Printed ===")
