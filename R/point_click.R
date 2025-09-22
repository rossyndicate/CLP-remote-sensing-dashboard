#' I want this to interact with `selected_points <- reactiveValues()` which
#' will in turn interact with `output$selected_point_info <- renderText` (which
#' will become a renderPlot). This function will interact with both of these with
#' observeEvent as a middle man. The process looks like:
#' make `selected_points` -> `observeEvent(map click)` -> update plots via `output$selected_point_info <- renderText`
#' 
#' Add points if they are not in the list, and remove points if they are in the list.
#' Points can be added or removed with this function or the plotting function

point_click <- function(point_list, id) {
  
  if (id %in% point_list) {
    point_list <- point_list[!point_list %in% id]
  } else {
    point_list <- c(point_list, id)
  }
  
  return(point_list)
  
}