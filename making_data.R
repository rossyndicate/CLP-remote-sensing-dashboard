# set up file paths
NW_CLP_all_points_path <- here("data", "NW_CLP_all_points.csv")
clp_temp_rs_estimate_nocorr_v2024_10_10_path <- here("data", "clp_temp_rs_estimate_nocorr_v2024-10-10.feather")
clp_sdd_rs_estimate_v2024_10_10_path <- here("data", "clp_sdd_rs_estimate_v2024-10-10.feather")

# read data
nw_clp_points <- read_csv(NW_CLP_all_points_path, show_col_types = F)
clp_temp <- read_feather(clp_temp_rs_estimate_nocorr_v2024_10_10_path)
clp_sdd <- read_feather(clp_sdd_rs_estimate_v2024_10_10_path)

# get intersection of points and ts data to get points that are
# shared across both datasets
point_temp_intersect <- intersect(
  unique(clp_temp$permanent_identifier),
  unique(nw_clp_points$permanent_identifier)
)

point_sdd_intersect <- intersect(
  unique(clp_sdd$permanent_identifier),
  unique(nw_clp_points$permanent_identifier)
)

points <- nw_clp_points %>% 
  filter(str_detect(data_group, "CLP"), 
         !is.na(Latitude), !is.na(Longitude), 
         permanent_identifier %in% point_temp_intersect) %>% 
  rename(
    # ID Information
    row_id = rowid, perm = permanent_identifier,
    # Location Information
    lng = Longitude, lat = Latitude
  ) 

temp_ts <- clp_temp %>% 
  select(
    # ID Information
    row_id = rowid, perm = permanent_identifier, mission,
    # Timeseries Information
    date, temp = med_SurfaceTemp
  ) %>% 
  filter(perm %in% point_temp_intersect) %>% 
  distinct() %>% 
  mutate(temp = as.numeric(temp) -273.15) %>% # convert from K to C
  pivot_wider(
    id_cols = c(date, mission),
    names_from = perm,
    values_from = temp,
    values_fn = ~mean(.x, na.rm = TRUE)  
  )

sdd_ts_mean <- clp_sdd %>%
  select(
    # ID Information
    row_id = rowid, perm = permanent_identifier, mission,
    # Timeseries Information
    date, mean
  ) %>% 
  filter(perm %in% point_sdd_intersect) %>% 
  distinct() %>% 
  mutate(temp = as.numeric(mean)) %>% 
  pivot_wider(
    id_cols = c(date, mission),
    names_from = perm,
    values_from = mean,
    values_fn = ~mean(.x, na.rm = TRUE)  
  )

# Make a lookup table for the color for the ids

# Create a color palette with enough colors
n_colors <- length(point_temp_intersect)

# Using viridis colors (you already have viridis loaded)
color_palette <- viridis::viridis(n_colors)

id_color_lookup <- tibble(perm = point_temp_intersect, color = color_palette)

points <- points %>% 
  select(perm, gnis_id, gnis_name, lat, lng) %>%
  left_join(id_color_lookup, by = "perm")

# Look up table for the time series plots names
temp_lookup <- clp_temp %>%
  select(perm = permanent_identifier, gnis_name) %>%
  distinct() %>%
  left_join(id_color_lookup, by = "perm")

sdd_lookup <- clp_sdd %>%
  select(perm = permanent_identifier, gnis_name) %>%
  distinct() %>%
  left_join(id_color_lookup, by = "perm")

# Binding table for the time series plots
temp_plot_df <- temp_ts %>% 
  select(date, mission)

sdd_plot_df <- sdd_ts_mean %>%
  select(date, mission)

# save the data
arrow::write_parquet(points, here("data", "points.parquet"))
arrow::write_parquet(temp_ts, here("data", "temp_ts.parquet"))
arrow::write_parquet(sdd_ts_mean, here("data", "sdd_ts_mean.parquet"))
arrow::write_parquet(id_color_lookup, here("data", "id_color_lookup.parquet"))

arrow::write_parquet(temp_lookup, here("data", "temp_lookup.parquet"))
arrow::write_parquet(sdd_lookup, here("data", "sdd_lookup.parquet"))
arrow::write_parquet(temp_plot_df, here("data", "temp_plot_df.parquet"))
arrow::write_parquet(sdd_plot_df, here("data", "sdd_plot_df.parquet"))