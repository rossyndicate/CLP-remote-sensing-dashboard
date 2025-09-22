load_temp_data <- function(temp_data_path) {
  
  # Try reading in the temperature time series data
  tryCatch({
    temp <- arrow::read_parquet(temp_data_path, as_data_frame = TRUE)
  }, error = function(e) {
    showNotification("Error loading waterbody data", type = "error")
  })
  
  return(temp)
  
}