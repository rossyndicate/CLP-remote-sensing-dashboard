load_temp_data <- function(sdd_data_path) {
  
  # Try reading in the SDD time series data
  tryCatch({
    sdd <- arrow::read_feather(sdd_data_path, as_data_frame = TRUE)
  }, error = function(e) {
    showNotification("Error loading waterbody data", type = "error")
  })
  
  return(sdd)
  
}