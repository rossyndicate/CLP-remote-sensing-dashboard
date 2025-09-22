update_nw_clp_map <- function(point_df, point_list) {
  leafletProxy("map") %>%
    clearMarkers() %>%
    addCircleMarkers(
      data = points,
      ~lng, ~lat,
      popup = ~paste("Reservoir:", perm),
      layerId = ~perm,
      color = ~ifelse(perm %in% point_list, color, '#808080'),
      fillColor = ~ifelse(perm %in% point_list, color, '#808080'),
      fillOpacity = 0.7,
      radius = 8,
      weight = 2
    )
}