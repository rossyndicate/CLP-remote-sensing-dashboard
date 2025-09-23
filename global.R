#### Global Variables ####
suppressMessages({
  # Data cleaning and utilities
  library(janitor)
  library(here)
  # Spatial packages
  library(sf)
  library(leaflet)
  library(leaflet.extras) 
  # Enhanced visualization
  library(plotly)
  library(viridis) 
  # Enhanced Shiny components 
  library(shiny)
  library(htmltools) 
  # Core data manipulation
  library(tidyverse)
  # Data loading
  library(arrow)
})

#### Enhanced Setup ####
options(shiny.maxRequestSize = 10000 * 1024^2)

## Sourcing the functions
source(here("R", "nw_clp_map.R"))
source(here("R", "plot_temp.R"))
source(here("R", "plot_sdd.R"))
source(here("R", "update_ids.R"))
source(here("R", "update_nw_clp_map.R"))
source(here("R", "update_temp_data.R"))
source(here("R", "update_sdd_data.R"))

# Set up file paths for data
points_path <- here("data", "points.parquet")
temp_ts_path <- here("data", "temp_ts.parquet") 
sdd_ts_path <- here("data", "sdd_ts_mean.parquet")
id_color_lookup_path <- here("data", "id_color_lookup.parquet")
temp_lookup_path <- here("data", "temp_lookup.parquet")
sdd_lookup_path <- here("data", "sdd_lookup.parquet")
temp_plot_df_path <- here("data", "temp_plot_df.parquet")
sdd_plot_df_path <- here("data", "sdd_plot_df.parquet")

# Load the point data and convert it to sf
points <- arrow::read_parquet(points_path, quiet = TRUE) %>% 
  st_as_sf(coords = (c("lng", "lat")),
           crs = 4326,
           remove = FALSE)

# Load the time series data
# Temperature
temp_ts <- arrow::read_parquet(temp_ts_path, quiet = TRUE)
# SDD
sdd_ts_mean <- arrow::read_parquet(sdd_ts_path, quiet = TRUE)

# Load the color lookup table
id_color_lookup <- arrow::read_parquet(id_color_lookup_path, quiet = TRUE)

# Load the look up tables
temp_lookup <- arrow::read_parquet(temp_lookup_path, quiet = TRUE)
sdd_lookup <- arrow::read_parquet(sdd_lookup_path, quiet = TRUE)

# Load the binding tables
temp_plot_df <- arrow::read_parquet(temp_plot_df_path, quiet = TRUE)
sdd_plot_df <- arrow::read_parquet(sdd_plot_df_path, quiet = TRUE) 

# This is to make the shapes on the plotly plots
# Get all unique missions from both datasets
all_missions <- unique(c(temp_plot_df$mission, sdd_plot_df$mission))
n_missions <- length(all_missions)

plotly_shapes <- c("circle", "square", "diamond", "cross", "x")

# Create mission-shape lookup for all missions
mission_shape_lookup <- tibble(
  mission = all_missions,
  shape = rep(plotly_shapes, length.out = n_missions)
)

# Convert mission columns to factors with consistent levels
temp_plot_df <- temp_plot_df %>%
  mutate(mission = factor(mission, levels = all_missions)) %>% 
  left_join(mission_shape_lookup, by = "mission")

sdd_plot_df <- sdd_plot_df %>%
  mutate(mission = factor(mission, levels = all_missions)) %>% 
  left_join(mission_shape_lookup, by = "mission")
