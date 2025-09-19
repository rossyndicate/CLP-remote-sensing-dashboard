#### Global Variables ####

# Enhanced CLP Remote Sensing Dashboard
# Enhanced package loading for better performance and styling

# Simply load required packages - shinyapps.io will automatically detect and install them
suppressMessages({
  # Date/time handling
  library(zoo)
  library(padr)
  # Data cleaning and utilities
  library(janitor)
  library(broom)
  library(here)
  # Spatial packages
  library(sf)
  library(leaflet)
  library(leaflet.extras) # for additional leaflet functionality
  library(tigris) # for colorado boundary
  # Enhanced visualization
  library(ggpubr)
  library(ggthemes)
  library(scales)
  library(plotly)
  library(ggpmisc)
  library(viridis) # for better color palettes
  library(RColorBrewer)
  # Development tools
  library(devtools)
  # Enhanced Shiny components
  library(shiny)
  library(shinycssloaders) # for loading spinners
  library(shinyTime)
  library(bslib) # for bootstrap themes
  library(shinyWidgets) # for enhanced widgets
  library(shinydashboard)
  library(htmltools)
  library(fontawesome) # for icons
  library(fresh) # for custom themes
  library(readr)
  # Core data manipulation
  library(tidyverse)
  library(DT)
  library(purrr)
  library(data.table)
  library(arrow)
})

#### Enhanced Setup ####

options(shiny.maxRequestSize = 10000 * 1024^2)

# Negate %in% call for easier filtering
`%nin%` = Negate(`%in%`)

# Null-coalescing operator for R (similar to %||% in other languages)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Enhanced color palette for reservoirs
reservoir_colors <- c(
  "#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", 
  "#1abc9c", "#34495e", "#e67e22", "#95a5a6", "#27ae60"
)

# Enhanced temperature and SDD thresholds for data quality
temp_thresholds <- list(
  min_valid = -5,    # Minimum valid temperature (°C)
  max_valid = 35,    # Maximum valid temperature (°C)
  freeze_point = 0,  # Freezing point for reference
  optimal_range = c(15, 25)  # Optimal temperature range for aquatic life
)

sdd_thresholds <- list(
  poor = 1,      # Below 1m = poor clarity
  moderate = 2,  # 1-2m = moderate clarity  
  good = 3,      # 2-3m = good clarity
  excellent = 3  # Above 3m = excellent clarity
)

# Data quality categories
quality_colors <- list(
  "Good" = "#2ecc71",
  "Questionable" = "#f39c12", 
  "No Data" = "#e74c3c",
  "Simulated" = "#9b59b6"
)

# Map styling constants
map_config <- list(
  buffer_distance = 10000,  # 10km buffer in meters
  cluster_max_radius = 50,
  min_zoom = 8,
  max_zoom = 18,
  default_marker_size = 8,
  selected_marker_size = 10
)

# Plot styling constants
plot_config <- list(
  line_width = 1.2,
  point_size = 2,
  point_alpha = 0.7,
  grid_alpha = 0.3,
  max_reservoirs = 10,  # Maximum reservoirs that can be selected
  date_breaks = "2 months"
)

# Custom theme for ggplot
theme_clp <- function() {
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d"),
    plot.caption = element_text(size = 10, color = "#95a5a6", style = "italic"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(alpha = 0.3),
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
}

# Utility function for safe data loading
safe_read_csv <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(data.frame())
  }
  
  tryCatch({
    read_csv(file_path, ...)
  }, error = function(e) {
    warning(paste("Error reading CSV:", e$message))
    data.frame()
  })
}

safe_read_feather <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(data.frame())
  }
  
  tryCatch({
    read_feather(file_path, ...)
  }, error = function(e) {
    warning(paste("Error reading feather:", e$message))
    data.frame()
  })
}

# Function to validate temperature data
validate_temperature <- function(temp_k) {
  temp_c <- temp_k - 273.15
  case_when(
    is.na(temp_k) ~ NA_real_,
    temp_c < temp_thresholds$min_valid | temp_c > temp_thresholds$max_valid ~ NA_real_,
    TRUE ~ temp_c
  )
}

# Function to categorize water clarity based on SDD
categorize_clarity <- function(sdd_m) {
  case_when(
    is.na(sdd_m) ~ "No Data",
    sdd_m < sdd_thresholds$poor ~ "Poor",
    sdd_m < sdd_thresholds$moderate ~ "Moderate", 
    sdd_m < sdd_thresholds$good ~ "Good",
    TRUE ~ "Excellent"
  )
}

# Debug mode (set to FALSE in production)
DEBUG_MODE <- FALSE

if (DEBUG_MODE) {
  cat("CLP Dashboard: Debug mode enabled\n")
  cat("Working directory:", getwd(), "\n")
  cat("Available data files:\n")
  if (dir.exists("data")) {
    cat(paste(" -", list.files("data"), collapse = "\n"), "\n")
  }
}
