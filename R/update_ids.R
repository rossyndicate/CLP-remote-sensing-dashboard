#' @title Update Point Selection List
#'
#' @description 
#' Toggles the presence of a point ID in a selection list. If the ID is already 
#' in the list, it removes it; if not present, it adds it. This function is used
#' to manage interactive point selection in the Shiny application, allowing users
#' to select/deselect waterbodies by clicking on map markers or plot elements.
#'
#' @param point_list A character vector containing currently selected point IDs
#' @param id A character string representing the point ID to toggle
#'
#' @return A character vector with the updated point selection list
update_ids <- function(point_list, id) {
  
  if (id %in% point_list) {
    point_list <- point_list[!point_list %in% id]
  } else {
    point_list <- c(point_list, id)
  }
  
  return(point_list)
  
}