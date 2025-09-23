#' @title Update Colorado Waterbody Leaflet Display
#'
#' @description 
#' Updates the leaflet map by clearing existing markers and adding new circle markers
#' with updated styling based on point selection status. Selected points are displayed
#' with their assigned colors while unselected points appear in gray. The map view
#' is also updated to fly to the specified coordinates.
#'
#' @param point_list A character vector containing IDs of currently selected points
#' @param lat Numeric value for the latitude to center the map view on
#' @param lng Numeric value for the longitude to center the map view on
#'
#' @return A leafletProxy object with updated markers and map view
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
