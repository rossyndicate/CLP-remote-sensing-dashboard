update_sdd_data <- function(plot_df, id){
  
  if (id %in% names(plot_df)) {
    plot_df <- plot_df %>% select(-!!sym(id))
  } else {
    plot_df[[id]] <- sdd_ts_mean[[id]]
  }
  
  return(plot_df)
  
}
