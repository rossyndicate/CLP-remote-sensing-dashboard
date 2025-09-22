nw_clp_map <- function(point_df, point_list) {
  leaflet(point_df) %>%
    addTiles() %>%
    addCircleMarkers(
      data = points,
      ~lng, ~lat,
      popup = ~paste("Waterbody:", perm),
      layerId = ~perm,
      color = '#808080',
      fillColor = '#808080',
      fillOpacity = 0.7,
      radius = 8,
      weight = 2
    ) 
}