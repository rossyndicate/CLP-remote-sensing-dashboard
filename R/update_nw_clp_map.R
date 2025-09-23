update_nw_clp_map <- function(point_list, lat, lng) {
  leafletProxy("map") %>%
    clearMarkers() %>%
    addCircleMarkers(
      data = points,
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
      color = ~ifelse(perm %in% point_list, color, '#808080'),
      fillColor = ~ifelse(perm %in% point_list, color, '#808080'),
      fillOpacity = 0.7,
      radius = 8,
      weight = 2
    ) %>%
    flyTo(lng = lng, lat = lat, zoom = 10)
}
