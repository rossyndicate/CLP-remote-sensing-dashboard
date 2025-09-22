load_point_data <- function(point_data_path) {
  
  # Try reading in the reservoir sf data
  tryCatch({
    points <- sf::st_read(point_data_path)
  }, error = function(e) {
    showNotification("Error loading reservoir data", type = "error")
  })
  
  return(points)
  
}