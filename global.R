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
  library(rlist)
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
  # Enhanced Shiny components - MINIMAL LOADING FOR DEBUGGING
  library(shiny)
  library(shinycssloaders) 
  library(shinyTime) 
  library(bslib) 
  library(shinyWidgets)
  library(shinydashboard) 
  library(htmltools) 
  library(htmlwidgets) 
  library(shinyjs)
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

# Set up file paths for data
points_path <- here("data", "points.shp")
temp_ts_path <- here("data", "temp_ts.parquet") 
# sdd_ts_path <- here("data", "clp_sdd_rs_estimate_v2024-10-10.feather")
id_color_lookup_path <- here("data", "id_color_lookup.parquet")

# Load the point data sf - DIRECT LOADING
points <- st_read(points_path, quiet = TRUE)

temp_ts <- load_temp_data(temp_ts_path)
temp_plot_df <- temp_ts %>% select(date)
# sdd_ts <- load_sdd_data(sdd_ts_path)

id_color_lookup <- read_parquet(id_color_lookup_path)
