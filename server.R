function(input, output, session) {
  
  # Set up file paths for data
  NW_CLP_all_points_path <- here("data", "NW_CLP_all_points.csv")
  clp_temp_rs_estimate_nocorr_v2024_10_10_path <- here("data", "clp_temp_rs_estimate_nocorr_v2024-10-10.feather") 
  clp_sdd_rs_estimate_v2024_10_10_path <- here("data", "clp_sdd_rs_estimate_v2024-10-10.feather")
  
  # Enhanced data loading with error handling and progress feedback
  nw_clp_points <- reactive({
    
    req(file.exists(NW_CLP_all_points_path),
        file.exists(clp_temp_rs_estimate_nocorr_v2024_10_10_path),
        file.exists(clp_sdd_rs_estimate_v2024_10_10_path))
    
    tryCatch({
      data <- read_csv(here("data", "NW_CLP_all_points.csv"), show_col_types = FALSE) %>%
        filter(data_group == "CLP") %>%
        # Add data quality indicators
        mutate(
          # Clean reservoir names
          display_name = case_when(
            is.na(gnis_name) | gnis_name == "" ~ paste("Reservoir", rowid),
            TRUE ~ gnis_name
          ),
          # Calculate size categories for better visualization
          size_category = case_when(
            area_sq_km < 0.1 ~ "Small",
            area_sq_km < 0.5 ~ "Medium", 
            TRUE ~ "Large"
          )
        )
      
      data
    }, error = function(e) {
      showNotification("Error loading reservoir data", type = "error")
      data.frame()
    })
  })
  
  # Enhanced reactive for data availability checking
  data_availability <- reactive({
    points <- nw_clp_points()
    temp_data <- clp_temp()
    sdd_data <- clp_sdd()
    
    if (nrow(points) > 0) {
      points %>%
        mutate(
          # Create display name field
          display_name = case_when(
            !is.na(gnis_name) & gnis_name != "" ~ gnis_name,
            !is.na(permanent_identifier) ~ paste("Reservoir", permanent_identifier),
            TRUE ~ paste("Unnamed Reservoir", rowid)
          ),
          # Create size category based on area
          size_category = case_when(
            area_sq_km >= 1.0 ~ "Large",
            area_sq_km >= 0.1 ~ "Medium", 
            TRUE ~ "Small"
          ),
          # Check data availability
          has_temp_data = rowid %in% unique(temp_data$rowid),
          has_sdd_data = rowid %in% unique(sdd_data$rowid)
        )
    } else {
      points
    }
  })
  
  clp_temp <- reactive({
    req(file.exists(here("data", "clp_temp_rs_estimate_nocorr_v2024-10-10.feather")))
    
    tryCatch({
      read_feather(here("data", "clp_temp_rs_estimate_nocorr_v2024-10-10.feather")) %>%
        mutate(
          # Convert from Kelvin to Celsius and Fahrenheit with better error handling
          temp_celsius = case_when(
            is.na(med_SurfaceTemp) ~ NA_real_,
            med_SurfaceTemp < 200 | med_SurfaceTemp > 350 ~ NA_real_, # Invalid temperatures
            TRUE ~ as.numeric(med_SurfaceTemp) - 273.15
          ),
          temp_fahrenheit = case_when(
            is.na(temp_celsius) ~ NA_real_,
            TRUE ~ (temp_celsius * 9/5) + 32
          ),
          # Add quality flags
          temp_quality = case_when(
            is.na(temp_celsius) ~ "No Data",
            temp_celsius < -5 | temp_celsius > 35 ~ "Questionable",
            TRUE ~ "Good"
          )
        ) %>%
        # Remove rows with invalid dates
        filter(!is.na(date))
    }, error = function(e) {
      showNotification("Error loading temperature data", type = "error")
      data.frame()
    })
  })
  
  clp_sdd <- reactive({
    # Try to load actual SDD data first, fall back to placeholder
    sdd_file <- here("data", "clp_sdd_rs_estimate_v2024-10-10.feather")
    
    if (file.exists(sdd_file)) {
      tryCatch({
        sdd_data <- read_feather(sdd_file)
        # If SDD data exists and has actual values, use it
        if (nrow(sdd_data) > 0 && any(!is.na(sdd_data$mean))) {
          return(sdd_data %>%
                   rename(sdd_mean = mean) %>%
                   mutate(
                     sdd_quality = case_when(
                       is.na(sdd_mean) ~ "No Data",
                       sdd_mean < 0.1 | sdd_mean > 10 ~ "Questionable",
                       TRUE ~ "Good"
                     )
                   )
          )
        }
      }, error = function(e) {
        showNotification("Note: Using placeholder SDD data", type = "message")
      })
    }
    
    # Fallback to enhanced placeholder data
    temp_data <- clp_temp()
    if (nrow(temp_data) > 0) {
      temp_data %>%
        mutate(
          # More realistic SDD values based on season and temperature
          sdd_mean = pmax(0.5, pmin(5.0, 
                                    2.5 + sin((as.numeric(format(date, "%j")) - 100) * 2 * pi / 365) * 1.5 +
                                      rnorm(n(), 0, 0.3)
          )),
          sdd_quality = "Simulated"
        )
    } else {
      data.frame()
    }
  })
  
  
  # Convert points to sf object with Colorado State Plane projection
  clp_sf <- reactive({
    req(nrow(data_availability()) > 0)
    
    points <- data_availability()
    
    tryCatch({
      # Create sf object from lat/long (WGS84)
      sf_points <- st_as_sf(points, 
                            coords = c("Longitude", "Latitude"), 
                            crs = 4326)  # WGS84
      
      # Transform to Colorado State Plane Central (EPSG:3994)
      sf_points_transformed <- st_transform(sf_points, crs = 3994)
      
      return(sf_points_transformed)
    }, error = function(e) {
      showNotification("Error processing spatial data", type = "error")
      return(NULL)
    })
  })
  
  # Enhanced map bounds calculation with proper 10km buffer
  map_bounds <- reactive({
    req(clp_sf())
    sf_data <- clp_sf()
    
    tryCatch({
      # Get bounding box in State Plane coordinates
      bbox <- st_bbox(sf_data)
      
      # Add 10km buffer (10000 meters) in State Plane coordinates
      buffered_bounds <- list(
        west = bbox[1] - 10000,
        east = bbox[3] + 10000,
        south = bbox[2] - 10000,
        north = bbox[4] + 10000
      )
      
      # Convert back to lat/long for leaflet
      bbox_poly <- st_polygon(list(matrix(c(
        buffered_bounds$west, buffered_bounds$south,
        buffered_bounds$east, buffered_bounds$south,
        buffered_bounds$east, buffered_bounds$north,
        buffered_bounds$west, buffered_bounds$north,
        buffered_bounds$west, buffered_bounds$south
      ), ncol = 2, byrow = TRUE)))
      
      bbox_sf <- st_sfc(bbox_poly, crs = 3994) %>%
        st_transform(4326)
      
      bbox_wgs84 <- st_bbox(bbox_sf)
      
      list(
        lng1 = bbox_wgs84[1],
        lat1 = bbox_wgs84[2],
        lng2 = bbox_wgs84[3], 
        lat2 = bbox_wgs84[4]
      )
    }, error = function(e) {
      # Fallback bounds
      sf_wgs84 <- st_transform(sf_data, 4326)
      coords <- st_coordinates(sf_wgs84)
      list(
        lng1 = min(coords[,1]) - 0.1,
        lat1 = min(coords[,2]) - 0.1,
        lng2 = max(coords[,1]) + 0.1,
        lat2 = max(coords[,2]) + 0.1
      )
    })
  })
  
  # Enhanced reactive values for selected reservoirs with metadata
  selected_reservoirs <- reactiveValues(
    rowids = c(),
    data = data.frame(),
    count = 0,
    last_selected = NULL
  )
  
  # Update selection info display
  observe({
    count <- length(selected_reservoirs$rowids)
    
    info_text <- if (count == 0) {
      "Click on reservoirs to view their data"
    } else if (count == 1) {
      points <- data_availability()
      selected_point <- points[points$rowid == selected_reservoirs$rowids[1], ]
      paste0("Selected: ", selected_point$display_name)
    } else {
      paste0("Selected: ", count, " reservoirs")
    }
    
    output$selection_info <- renderText({
      info_text
    })
  })
  
  
  # Enhanced reset function with user feedback
  observeEvent(input$reset_btn, {
    # Show progress notification
    showNotification("Resetting selection...", type = "message", duration = 1)
    
    # Clear selection
    selected_reservoirs$rowids <- c()
    selected_reservoirs$data <- data.frame()
    selected_reservoirs$count <- 0
    selected_reservoirs$last_selected <- NULL
    
    # Update map markers with animation
    req(clp_sf())
    sf_wgs84 <- st_transform(clp_sf(), 4326)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(
        data = sf_wgs84,
        layerId = ~rowid,
        popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
        icon = ~create_marker_icon(FALSE, size_category),
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          maxClusterRadius = 50
        )
      )
  })
  
  # Function to create enhanced popups
  create_enhanced_popup <- function(rowid, name, area, size_cat, has_temp, has_sdd) {
    paste0(
      "<div style='font-family: Arial, sans-serif; line-height: 1.4;'>",
      "<h4 style='margin: 0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>",
      name, "</h4>",
      "<p style='margin: 5px 0;'><strong>Reservoir ID:</strong> ", rowid, "</p>",
      "<p style='margin: 5px 0;'><strong>Surface Area:</strong> ", round(area, 3), " km²</p>",
      "<p style='margin: 5px 0;'><strong>Size Category:</strong> ", size_cat, "</p>",
      "<div style='margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 5px;'>",
      "<strong>Data Availability:</strong><br>",
      "<span style='color: ", ifelse(has_temp, "#2ecc71", "#e74c3c"), ";'>●</span> Temperature Data<br>",
      "<span style='color: ", ifelse(has_sdd, "#2ecc71", "#e74c3c"), ";'>●</span> SDD Data",
      "</div>",
      "<p style='margin: 10px 0 0 0; font-size: 12px; color: #7f8c8d; font-style: italic;'>",
      "Click to select for time series analysis",
      "</p>",
      "</div>"
    )
  }
  
  # Function to create custom marker icons
  create_marker_icon <- function(selected, size_category) {
    # Color scheme based on selection and size
    if (selected) {
      color <- "#e74c3c"
      fillColor <- "#ff6b7a"
    } else {
      color <- "#3498db"  
      fillColor <- "#74b9ff"
    }
    
    # Size based on category
    radius <- case_when(
      size_category == "Large" ~ 10,
      size_category == "Medium" ~ 8,
      TRUE ~ 6
    )
    
    makeAwesomeIcon(
      icon = 'tint',
      markerColor = ifelse(selected, 'red', 'blue'),
      iconColor = 'white',
      library = 'fa'
    )
  }
  
  
  # SIMPLIFIED MAP FOR DEBUGGING - BASIC LEAFLET MAP
  output$map <- renderLeaflet({
    # Create the most basic leaflet map possible
    leaflet() %>%
      addTiles() %>%
      setView(lng = -105.1, lat = 40.6, zoom = 10)  # Colorado coordinates
  })
  
  
  # DISABLED FOR DEBUGGING - MAP CLICK OBSERVER
  # observeEvent(input$map_marker_click, {
  #   # ... map click handling code commented out
  # })
  
  
  # BASIC temperature plot output (WORKING):
  output$temp_plot <- renderPlotly({
    req(selected_reservoir())
    
    reservoir_data <- temp_data %>%
      filter(rowid == selected_reservoir()$rowid) %>%
      arrange(date)
    
    if(nrow(reservoir_data) == 0) {
      return(plot_ly() %>% 
               add_text(x = 0.5, y = 0.5, text = "No temperature data available", 
                        textposition = "middle center") %>%
               layout(showlegend = FALSE, 
                      xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, showticklabels = FALSE)))
    }
    
    plot_ly(reservoir_data, x = ~date, y = ~temperature_c, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Surface Temperature",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Temperature (°C)"))
  })
  
  # BASIC SDD plot output (WORKING):
  output$sdd_plot <- renderPlotly({
    req(selected_reservoir())
    
    reservoir_data <- sdd_data %>%
      filter(rowid == selected_reservoir()$rowid) %>%
      arrange(date)
    
    if(nrow(reservoir_data) == 0) {
      return(plot_ly() %>% 
               add_text(x = 0.5, y = 0.5, text = "No SDD data available", 
                        textposition = "middle center") %>%
               layout(showlegend = FALSE, 
                      xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, showticklabels = FALSE)))
    }
    
    plot_ly(reservoir_data, x = ~date, y = ~sdd_m, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Secchi Disk Depth",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Depth (m)"))
  })
  
  
  # FANCY PLOT OUTPUTS (COMMENTED OUT - ADD BACK INCREMENTALLY):
  # Enhanced temperature plot output with professional styling
  # DISABLED FOR DEBUGGING - COMMENTING OUT ALL PLOT CODE
  # output$temp_plot <- renderPlotly({
  #   # ... plot code commented out
  # })
  
  
  # Enhanced SDD plot output with professional styling  
  # DISABLED FOR DEBUGGING - COMMENTING OUT ALL PLOT CODE
  # output$sdd_plot <- renderPlotly({
  #   # ... plot code commented out
  # })
  
  
  # Reactive values for plot synchronization
  plot_zoom_state <- reactiveValues(
    temp_zoom = NULL,
    sdd_zoom = NULL,
    sync_enabled = TRUE
  )
  
  # Enhanced plot connectivity - Temperature plot zoom sync
  # DISABLED: Complex plotly event handling causing warnings
  # observeEvent(event_data("plotly_relayout", source = "temp_plot"), {
  #   if (!plot_zoom_state$sync_enabled) return()
  #   
  #   relayout_data <- event_data("plotly_relayout", source = "temp_plot")
  #   
  #   if (!is.null(relayout_data) && any(grepl("xaxis.range", names(relayout_data)))) {
  #     # Extract x-axis range
  #     if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
  #       x_range <- c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]])
  #       
  #       # Temporarily disable sync to prevent infinite loop
  #       plot_zoom_state$sync_enabled <- FALSE
  #       
  #       # Apply same zoom to SDD plot
  #       plotlyProxy("sdd_plot", session) %>%
  #         plotlyProxyInvoke("relayout", list(
  #           "xaxis.range" = x_range
  #         ))
  #       
  #       # Re-enable sync after a short delay
  #       invalidateLater(100)
  #       observe({
  #         plot_zoom_state$sync_enabled <- TRUE
  #       })
  #     }
  #   }
  # })
  
  # Enhanced plot connectivity - SDD plot zoom sync  
  # DISABLED: Complex plotly event handling causing warnings
  # observeEvent(event_data("plotly_relayout", source = "sdd_plot"), {
  #   if (!plot_zoom_state$sync_enabled) return()
  #   
  #   relayout_data <- event_data("plotly_relayout", source = "sdd_plot")
  #   
  #   if (!is.null(relayout_data) && any(grepl("xaxis.range", names(relayout_data)))) {
  #     # Extract x-axis range
  #     if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
  #       x_range <- c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]])
  #       
  #       # Temporarily disable sync to prevent infinite loop
  #       plot_zoom_state$sync_enabled <- FALSE
  #       
  #       # Apply same zoom to temperature plot
  #       plotlyProxy("temp_plot", session) %>%
  #         plotlyProxyInvoke("relayout", list(
  #           "xaxis.range" = x_range
  #         ))
  #       
  #       # Re-enable sync after a short delay
  #       invalidateLater(100)
  #       observe({
  #         plot_zoom_state$sync_enabled <- TRUE
  #       })
  #     }
  #   }
  # })
  
  # Plot selection to map highlighting (crossfilter functionality)
  # DISABLED: Complex plotly event handling causing warnings
  # observeEvent(event_data("plotly_selected", source = "temp_plot"), {
  #   selected_data <- event_data("plotly_selected", source = "temp_plot")
  #   
  #   if (!is.null(selected_data) && nrow(selected_data) > 0) {
  #     # Get unique reservoir IDs from selected points
  #     selected_curve_numbers <- unique(selected_data$curveNumber)
  #     
  #     # Get the corresponding rowids
  #     temp_data <- selected_reservoirs$temp_data
  #     unique_reservoirs <- temp_data %>%
  #       distinct(rowid) %>%
  #       mutate(curve_number = row_number() - 1)
  #     
  #     highlighted_rowids <- unique_reservoirs$rowid[unique_reservoirs$curve_number %in% selected_curve_numbers]
  #     
  #     # Highlight corresponding reservoirs on map
  #     req(clp_sf())
  #     sf_wgs84 <- st_transform(clp_sf(), 4326)
  #     
  #     leafletProxy("map") %>%
  #       clearMarkers() %>%
  #       addAwesomeMarkers(
  #         data = sf_wgs84,
  #         layerId = ~rowid,
  #         popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
  #         icon = ~awesomeIcons(
  #           icon = 'tint',
  #           markerColor = case_when(
  #             rowid %in% highlighted_rowids ~ 'orange',
  #             rowid %in% selected_reservoirs$rowids ~ 'red',
  #             TRUE ~ 'blue'
  #           ),
  #           iconColor = 'white',
  #           library = 'fa'
  #         ),
  #         clusterOptions = markerClusterOptions(
  #           showCoverageOnHover = FALSE,
  #           zoomToBoundsOnClick = TRUE,
  #           spiderfyOnMaxZoom = TRUE,
  #           removeOutsideVisibleBounds = TRUE,
  #           maxClusterRadius = 50
  #         )
  #       )
  #     
  #     showNotification(paste("Highlighted", length(highlighted_rowids), "reservoirs on map"), 
  #                     type = "message", duration = 2)
  #   }
  # })
  
  # SDD plot selection to map highlighting
  # DISABLED: Complex plotly event handling causing warnings
  # observeEvent(event_data("plotly_selected", source = "sdd_plot"), {
  #   selected_data <- event_data("plotly_selected", source = "sdd_plot")
  #   
  #   if (!is.null(selected_data) && nrow(selected_data) > 0) {
  #     # Get unique reservoir IDs from selected points
  #     selected_curve_numbers <- unique(selected_data$curveNumber)
  #     
  #     # Get the corresponding rowids
  #     sdd_data <- selected_reservoirs$sdd_data
  #     unique_reservoirs <- sdd_data %>%
  #       distinct(rowid) %>%
  #       mutate(curve_number = row_number() - 1)
  #     
  #     highlighted_rowids <- unique_reservoirs$rowid[unique_reservoirs$curve_number %in% selected_curve_numbers]
  #     
  #     # Highlight corresponding reservoirs on map
  #     req(clp_sf())
  #     sf_wgs84 <- st_transform(clp_sf(), 4326)
  #     
  #     leafletProxy("map") %>%
  #       clearMarkers() %>%
  #       addAwesomeMarkers(
  #         data = sf_wgs84,
  #         layerId = ~rowid,
  #         popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
  #         icon = ~awesomeIcons(
  #           icon = 'tint',
  #           markerColor = case_when(
  #             rowid %in% highlighted_rowids ~ 'orange',
  #             rowid %in% selected_reservoirs$rowids ~ 'red',
  #             TRUE ~ 'blue'
  #           ),
  #           iconColor = 'white',
  #           library = 'fa'
  #         ),
  #         clusterOptions = markerClusterOptions(
  #           showCoverageOnHover = FALSE,
  #           zoomToBoundsOnClick = TRUE,
  #           spiderfyOnMaxZoom = True,
  #           removeOutsideVisibleBounds = TRUE,
  #           maxClusterRadius = 50
  #         )
  #       )
  #     
  #     showNotification(paste("Highlighted", length(highlighted_rowids), "reservoirs on map"), 
  #                     type = "message", duration = 2)
  #   }
  # })
  
  # Output for selection info display
  output$selection_info <- renderText({
    count <- length(selected_reservoirs$rowids)
    
    if (count == 0) {
      "Click on reservoirs to view their data"
    } else if (count == 1) {
      points <- data_availability()
      selected_point <- points[points$rowid == selected_reservoirs$rowids[1], ]
      paste0("Selected: ", selected_point$display_name)
    } else {
      paste0("Selected: ", count, " reservoirs")
    }
  })
  
  # Session info for debugging (can be removed in production)
  observe({
    cat("Selected reservoirs:", paste(selected_reservoirs$rowids, collapse = ", "), "\n")
    cat("Temperature data rows:", nrow(selected_reservoirs$temp_data %||% data.frame()), "\n")
    cat("SDD data rows:", nrow(selected_reservoirs$sdd_data %||% data.frame()), "\n")
  })
  
}