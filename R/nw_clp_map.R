#' @title Create Colorado Waterbody Leaflet Map
#'
#' @description 
#' Creates a leaflet interactive map displaying water body locations as circle markers.
#' Each marker shows waterbody information on hover and can be clicked for selection.
#'
#' @param point_df A data frame containing point locations with required columns:
#'   lng (longitude), lat (latitude), gnis_name (GNIS name, can be NA), 
#'   perm (permanent identifier)
#'
#' @return A leaflet map object with circle markers for each water body location
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