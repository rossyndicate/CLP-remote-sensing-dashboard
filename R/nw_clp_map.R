nw_clp_map <- function(point_df) {
  leaflet(point_df) %>%
    addTiles() %>%
    addCircleMarkers(
      data = point_df,
      ~lng, ~lat,
      label = ~if_else(!is.na(gnis_name),
                       paste("Waterbody:", gnis_name),
                       paste("Waterbody:", perm)),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      layerId = ~perm,
      color = '#808080',
      fillColor = '#808080',
      fillOpacity = 0.7,
      radius = 8,
      weight = 2
    ) 
}