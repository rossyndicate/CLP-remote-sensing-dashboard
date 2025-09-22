update_temp_data <- function(plot_df, id){
  
  if (id %in% names(plot_df)) {
    plot_df <- plot_df %>% select(-!!sym(id))
  } else {
    plot_df[[id]] <- temp_ts[[id]]
  }
  
  return(plot_df)
  
}
