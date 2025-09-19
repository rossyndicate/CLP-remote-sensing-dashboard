load_data <- function(nw_clp_path, temp_file_path, sdd_file_path) {
  
  # Try reading in the Reservoir Data
  tryCatch({
    nw_clp_points <- read_csv(nw_clp_path, show_col_types = F)
  }, error = function(e) {
    showNotification("Error loading reservoir data", type = "error")
  })
  
  # Try reading in the Temperature Data
  tryCatch({
    clp_temp <- read_feather(temp_file_path)
  }, error = function(e) {
    showNotification("Error loading reservoir data", type = "error")
  })
  
  # Try reading in the SDD Data
  tryCatch({
    clp_sdd <- read_feather(sdd_file_path) # TODO: Data is missing, update when we have the rest of the data
    clp_sdd <- clp_temp
  }, error = function(e) {
    showNotification("Error loading reservoir data", type = "error")
  })
  
}