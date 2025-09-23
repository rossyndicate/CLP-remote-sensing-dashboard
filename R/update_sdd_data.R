#' @title Update Secchi Disk Depth Data Selection
#'
#' @description
#' Toggles the inclusion of SDD data for a specific waterbody in the plot data frame.
#' If the waterbody ID is already present in the data frame, it removes that column.
#' If not present, it adds the SDD time series data for that waterbody from the 
#' global sdd_ts_mean dataset.
#'
#' @param plot_df A data frame containing the current SDD plot data with base columns
#'   (date, mission, shape) and additional columns for selected waterbodies
#' @param id A character string representing the waterbody permanent identifier (perm)
#'
#' @return A data frame with the updated SDD data selection
update_sdd_data <- function(plot_df, id){
  
  if (id %in% names(plot_df)) {
    plot_df <- plot_df %>% select(-!!sym(id))
  } else {
    plot_df[[id]] <- sdd_ts_mean[[id]]
  }
  
  return(plot_df)
  
}
