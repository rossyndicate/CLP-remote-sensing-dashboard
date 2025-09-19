function(input, output, session) {
  
  # Enhanced data loading with error handling and progress feedback
  nw_clp_points <- reactive({
    req(file.exists(here("data", "NW_CLP_all_points.csv")))
    
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
          ),
          # Add data availability status
          has_temp_data = rowid %in% unique(clp_temp()$rowid),
          has_sdd_data = rowid %in% unique(clp_sdd()$rowid)
        )
      
      data
    }, error = function(e) {
      showNotification("Error loading reservoir data", type = "error")
      data.frame()
    })
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
    req(nrow(nw_clp_points()) > 0)
    
    points <- nw_clp_points()
    
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
      points <- nw_clp_points()
      selected_point <- points[points$rowid == selected_reservoirs$rowids[1], ]
      paste0("Selected: ", selected_point$display_name)
    } else {
      paste0("Selected: ", count, " reservoirs")
    }
    
    updateTextOutput(session, "selection_info", value = info_text)
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
  
  
  # Enhanced Leaflet map output with professional styling
  output$map <- renderLeaflet({
    req(clp_sf())
    
    # Transform sf data back to WGS84 for leaflet
    sf_wgs84 <- st_transform(clp_sf(), 4326)
    
    # Create 10km buffer polygon for visualization
    buffer_sf <- st_buffer(clp_sf(), dist = 10000) %>%  # 10km buffer in meters
      st_union() %>%  # Combine all buffers into one polygon
      st_transform(4326)  # Transform to WGS84 for leaflet
    
    # Get bounds for the map
    bounds <- map_bounds()
    
    # Create custom map
    map <- leaflet(options = leafletOptions(
      zoomControl = TRUE,
      minZoom = 8,
      maxZoom = 18,
      preferCanvas = TRUE
    )) %>%
      # Add multiple tile layers
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light",
        options = providerTileOptions(opacity = 0.9)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite",
        options = providerTileOptions(opacity = 0.9)
      ) %>%
      addProviderTiles(
        providers$OpenTopoMap,
        group = "Topographic",
        options = providerTileOptions(opacity = 0.9)
      ) %>%
      
      # Add 10km buffer polygon
      addPolygons(
        data = buffer_sf,
        fillColor = "#3498db",
        fillOpacity = 0.1,
        color = "#2980b9",
        weight = 2,
        opacity = 0.6,
        dashArray = "5,5",
        popup = "10km Analysis Buffer Zone around CLP Reservoirs",
        group = "Buffer Zone"
      ) %>%
      
      # Add reservoir markers with clustering
      addAwesomeMarkers(
        data = sf_wgs84,
        layerId = ~rowid,
        popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
        icon = ~awesomeIcons(
          icon = 'tint',
          markerColor = 'blue',
          iconColor = 'white',
          library = 'fa'
        ),
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          maxClusterRadius = 50,
          iconCreateFunction = JS(
            "function(cluster) {
              var count = cluster.getChildCount();
              var size = count < 10 ? 'small' : count < 100 ? 'medium' : 'large';
              return new L.DivIcon({
                html: '<div><span>' + count + '</span></div>',
                className: 'marker-cluster marker-cluster-' + size,
                iconSize: new L.Point(40, 40)
              });
            }"
          )
        ),
        group = "Reservoirs"
      ) %>%
      
      # Set initial view
      fitBounds(
        lng1 = bounds$lng1,
        lat1 = bounds$lat1,
        lng2 = bounds$lng2,
        lat2 = bounds$lat2
      ) %>%
      
      # Add layer control
      addLayersControl(
        baseGroups = c("Light", "Satellite", "Topographic"),
        overlayGroups = c("Reservoirs", "Buffer Zone"),
        options = layersControlOptions(collapsed = FALSE, position = "topright")
      ) %>%
      
      # Add scale bar
      addScaleBar(position = "bottomleft") %>%
      
      # Add minimap
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = "bottomright",
        width = 120,
        height = 80,
        toggleDisplay = TRUE
      ) %>%
      
      # Add custom CSS for better cluster styling
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          
          // Custom cluster icon styling
          var style = document.createElement('style');
          style.type = 'text/css';
          style.innerHTML = `
            .marker-cluster-small {
              background-color: rgba(52, 152, 219, 0.8);
            }
            .marker-cluster-medium {
              background-color: rgba(46, 204, 113, 0.8);
            }
            .marker-cluster-large {
              background-color: rgba(231, 76, 60, 0.8);
            }
            .marker-cluster {
              border-radius: 50%;
              text-align: center;
              color: white;
              font-weight: bold;
              font-size: 12px;
            }
            .marker-cluster div {
              border-radius: 50%;
              width: 100%;
              height: 100%;
              display: flex;
              align-items: center;
              justify-content: center;
            }
          `;
          document.getElementsByTagName('head')[0].appendChild(style);
        }
      ")
    
    map
  })
  
  
  # Enhanced map click observer with smooth interactions
  observeEvent(input$map_marker_click, {
    clicked_id <- input$map_marker_click$id
    req(clicked_id)
    
    # Toggle selection
    if (clicked_id %in% selected_reservoirs$rowids) {
      # Remove if already selected
      selected_reservoirs$rowids <- selected_reservoirs$rowids[selected_reservoirs$rowids != clicked_id]
      showNotification("Reservoir deselected", type = "message", duration = 1)
    } else {
      # Add if not selected (limit to reasonable number for performance)
      if (length(selected_reservoirs$rowids) >= 10) {
        showNotification("Maximum 10 reservoirs can be selected at once", type = "warning", duration = 2)
        return()
      }
      selected_reservoirs$rowids <- c(selected_reservoirs$rowids, clicked_id)
      selected_reservoirs$last_selected <- clicked_id
      showNotification("Reservoir selected", type = "message", duration = 1)
    }
    
    # Update selected data
    if (length(selected_reservoirs$rowids) > 0) {
      temp_data <- clp_temp()
      sdd_data <- clp_sdd()
      
      temp_selected <- temp_data %>%
        filter(rowid %in% selected_reservoirs$rowids)
      
      sdd_selected <- sdd_data %>%
        filter(rowid %in% selected_reservoirs$rowids)
      
      # Store both datasets
      selected_reservoirs$temp_data <- temp_selected
      selected_reservoirs$sdd_data <- sdd_selected
      selected_reservoirs$count <- length(selected_reservoirs$rowids)
    } else {
      selected_reservoirs$temp_data <- data.frame()
      selected_reservoirs$sdd_data <- data.frame()
      selected_reservoirs$count <- 0
    }
    
    # Update map markers with enhanced styling
    req(clp_sf())
    sf_wgs84 <- st_transform(clp_sf(), 4326)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(
        data = sf_wgs84,
        layerId = ~rowid,
        popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
        icon = ~awesomeIcons(
          icon = 'tint',
          markerColor = ifelse(rowid %in% selected_reservoirs$rowids, 'red', 'blue'),
          iconColor = 'white',
          library = 'fa'
        ),
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          maxClusterRadius = 50
        )
      )
  })
  
  
  # Enhanced temperature plot output with professional styling
  output$temp_plot <- renderPlotly({
    if (length(selected_reservoirs$rowids) == 0) {
      # Enhanced empty state
      p <- ggplot() + 
        geom_text(
          aes(x = 0.5, y = 0.5), 
          label = "Select reservoirs on the map\nto view temperature data", 
          size = 5, 
          color = "#7f8c8d",
          fontface = "italic"
        ) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1) +
        theme(
          plot.background = element_rect(fill = "#f8f9fa", color = NA),
          panel.background = element_rect(fill = "#f8f9fa", color = NA)
        )
      
      return(ggplotly(p) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE)
        ))
    }
    
    plot_data <- selected_reservoirs$temp_data
    
    if (nrow(plot_data) == 0) {
      # No data available message
      p <- ggplot() + 
        geom_text(
          aes(x = 0.5, y = 0.5), 
          label = "No temperature data available\nfor selected reservoirs", 
          size = 5, 
          color = "#e74c3c"
        ) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p))
    }
    
    # Get reservoir names for better labeling
    points_data <- nw_clp_points()
    plot_data <- plot_data %>%
      left_join(points_data %>% select(rowid, display_name), by = "rowid") %>%
      mutate(
        reservoir_label = paste0(display_name, " (", rowid, ")"),
        # Filter out extreme outliers for better visualization
        temp_celsius_clean = ifelse(abs(temp_celsius - median(temp_celsius, na.rm = TRUE)) > 3 * mad(temp_celsius, na.rm = TRUE), 
                                   NA, temp_celsius)
      ) %>%
      filter(!is.na(temp_celsius_clean))
    
    # Create enhanced temperature plot
    p <- ggplot(plot_data, aes(x = date, y = temp_celsius_clean, color = reservoir_label)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(
        size = 2, 
        alpha = 0.7,
        aes(text = paste0(
          "Reservoir: ", display_name, "\n",
          "Date: ", format(date, "%Y-%m-%d"), "\n",
          "Temperature: ", round(temp_celsius_clean, 1), "°C (", round(temp_fahrenheit, 1), "°F)\n",
          "Quality: ", temp_quality
        ))
      ) +
      scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = sec_axis(
          trans = ~ . * 9/5 + 32, 
          name = "Temperature (°F)",
          breaks = scales::pretty_breaks(n = 6)
        ),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      scale_x_date(
        name = "Date",
        date_breaks = "2 months",
        date_labels = "%b %Y"
      ) +
      scale_color_viridis_d(
        name = "Reservoir",
        option = "D",
        begin = 0.1,
        end = 0.9
      ) +
      labs(
        title = "Surface Temperature Time Series",
        subtitle = paste0("Cache La Poudre Reservoirs (", length(unique(plot_data$rowid)), " selected)"),
        caption = "Data source: Remote sensing estimates"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
        plot.subtitle = element_text(size = 12, color = "#7f8c8d"),
        plot.caption = element_text(size = 10, color = "#95a5a6", style = "italic"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.y.right = element_text(color = "#e67e22"),
        axis.text.y.right = element_text(color = "#e67e22"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(alpha = 0.3),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Convert to plotly with enhanced interactions
    ggplotly(p, tooltip = "text", source = "temp_plot") %>%
      layout(
        legend = list(
          orientation = "h", 
          x = 0, 
          y = -0.15,
          font = list(size = 10)
        ),
        margin = list(b = 80),
        hovermode = "closest"
      ) %>%
      event_register("plotly_zoom") %>%
      event_register("plotly_relayout") %>%
      event_register("plotly_selected") %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        displaylogo = FALSE
      )
  })
  
  
  # Enhanced SDD plot output with professional styling
  output$sdd_plot <- renderPlotly({
    if (length(selected_reservoirs$rowids) == 0) {
      # Enhanced empty state
      p <- ggplot() + 
        geom_text(
          aes(x = 0.5, y = 0.5), 
          label = "Select reservoirs on the map\nto view water clarity data", 
          size = 5, 
          color = "#7f8c8d",
          fontface = "italic"
        ) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1) +
        theme(
          plot.background = element_rect(fill = "#f8f9fa", color = NA),
          panel.background = element_rect(fill = "#f8f9fa", color = NA)
        )
      
      return(ggplotly(p) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE)
        ))
    }
    
    plot_data <- selected_reservoirs$sdd_data
    
    if (nrow(plot_data) == 0) {
      # No data available message
      p <- ggplot() + 
        geom_text(
          aes(x = 0.5, y = 0.5), 
          label = "No SDD data available\nfor selected reservoirs", 
          size = 5, 
          color = "#e74c3c"
        ) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p))
    }
    
    # Get reservoir names for better labeling
    points_data <- nw_clp_points()
    plot_data <- plot_data %>%
      left_join(points_data %>% select(rowid, display_name), by = "rowid") %>%
      mutate(
        reservoir_label = paste0(display_name, " (", rowid, ")"),
        # Convert to different depth units for context
        sdd_feet = sdd_mean * 3.28084,
        # Add clarity categories
        clarity_category = case_when(
          sdd_mean < 1 ~ "Poor",
          sdd_mean < 2 ~ "Moderate", 
          sdd_mean < 3 ~ "Good",
          TRUE ~ "Excellent"
        )
      ) %>%
      filter(!is.na(sdd_mean), sdd_mean > 0)
    
    # Determine if this is simulated or real data
    data_type <- if (any(plot_data$sdd_quality == "Simulated")) "Simulated" else "Remote Sensing"
    
    # Create enhanced SDD plot
    p <- ggplot(plot_data, aes(x = date, y = sdd_mean, color = reservoir_label)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(
        size = 2, 
        alpha = 0.7,
        aes(
          shape = clarity_category,
          text = paste0(
            "Reservoir: ", display_name, "\n",
            "Date: ", format(date, "%Y-%m-%d"), "\n",
            "SDD: ", round(sdd_mean, 2), " m (", round(sdd_feet, 1), " ft)\n",
            "Clarity: ", clarity_category, "\n",
            "Data Quality: ", sdd_quality
          )
        )
      ) +
      scale_y_continuous(
        name = "Secchi Disk Depth (meters)",
        sec.axis = sec_axis(
          trans = ~ . * 3.28084, 
          name = "Secchi Disk Depth (feet)",
          breaks = scales::pretty_breaks(n = 6)
        ),
        breaks = scales::pretty_breaks(n = 6),
        limits = c(0, max(plot_data$sdd_mean, na.rm = TRUE) * 1.1)
      ) +
      scale_x_date(
        name = "Date",
        date_breaks = "2 months",
        date_labels = "%b %Y"
      ) +
      scale_color_viridis_d(
        name = "Reservoir",
        option = "C",
        begin = 0.1,
        end = 0.9
      ) +
      scale_shape_manual(
        name = "Water Clarity",
        values = c("Poor" = 16, "Moderate" = 17, "Good" = 15, "Excellent" = 18)
      ) +
      labs(
        title = "Secchi Disk Depth Time Series",
        subtitle = paste0("Cache La Poudre Reservoirs (", length(unique(plot_data$rowid)), " selected) - ", data_type, " Data"),
        caption = "Higher values indicate clearer water. SDD measures water transparency.",
        x = "Date"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
        plot.subtitle = element_text(size = 12, color = "#7f8c8d"),
        plot.caption = element_text(size = 10, color = "#95a5a6", style = "italic"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.y.right = element_text(color = "#9b59b6"),
        axis.text.y.right = element_text(color = "#9b59b6"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(alpha = 0.3),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Add reference lines for clarity categories
    if (data_type == "Simulated") {
      p <- p + 
        geom_hline(yintercept = c(1, 2, 3), linetype = "dashed", alpha = 0.3, color = "#7f8c8d") +
        annotate("text", x = min(plot_data$date), y = c(0.5, 1.5, 2.5, 3.5), 
                label = c("Poor", "Moderate", "Good", "Excellent"), 
                size = 3, alpha = 0.6, hjust = 0)
    }
    
    # Convert to plotly with enhanced interactions
    ggplotly(p, tooltip = "text", source = "sdd_plot") %>%
      layout(
        legend = list(
          orientation = "h", 
          x = 0, 
          y = -0.15,
          font = list(size = 10)
        ),
        margin = list(b = 80),
        hovermode = "closest"
      ) %>%
      event_register("plotly_zoom") %>%
      event_register("plotly_relayout") %>%
      event_register("plotly_selected") %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        displaylogo = FALSE
      )
  })
  
  
  # Reactive values for plot synchronization
  plot_zoom_state <- reactiveValues(
    temp_zoom = NULL,
    sdd_zoom = NULL,
    sync_enabled = TRUE
  )
  
  # Enhanced plot connectivity - Temperature plot zoom sync
  observeEvent(event_data("plotly_relayout", source = "temp_plot"), {
    if (!plot_zoom_state$sync_enabled) return()
    
    relayout_data <- event_data("plotly_relayout", source = "temp_plot")
    
    if (!is.null(relayout_data) && any(grepl("xaxis.range", names(relayout_data)))) {
      # Extract x-axis range
      if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
        x_range <- c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]])
        
        # Temporarily disable sync to prevent infinite loop
        plot_zoom_state$sync_enabled <- FALSE
        
        # Apply same zoom to SDD plot
        plotlyProxy("sdd_plot", session) %>%
          plotlyProxyInvoke("relayout", list(
            "xaxis.range" = x_range
          ))
        
        # Re-enable sync after a short delay
        invalidateLater(100)
        observe({
          plot_zoom_state$sync_enabled <- TRUE
        })
      }
    }
  })
  
  # Enhanced plot connectivity - SDD plot zoom sync
  observeEvent(event_data("plotly_relayout", source = "sdd_plot"), {
    if (!plot_zoom_state$sync_enabled) return()
    
    relayout_data <- event_data("plotly_relayout", source = "sdd_plot")
    
    if (!is.null(relayout_data) && any(grepl("xaxis.range", names(relayout_data)))) {
      # Extract x-axis range
      if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
        x_range <- c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]])
        
        # Temporarily disable sync to prevent infinite loop
        plot_zoom_state$sync_enabled <- FALSE
        
        # Apply same zoom to temperature plot
        plotlyProxy("temp_plot", session) %>%
          plotlyProxyInvoke("relayout", list(
            "xaxis.range" = x_range
          ))
        
        # Re-enable sync after a short delay
        invalidateLater(100)
        observe({
          plot_zoom_state$sync_enabled <- TRUE
        })
      }
    }
  })
  
  # Plot selection to map highlighting (crossfilter functionality)
  observeEvent(event_data("plotly_selected", source = "temp_plot"), {
    selected_data <- event_data("plotly_selected", source = "temp_plot")
    
    if (!is.null(selected_data) && nrow(selected_data) > 0) {
      # Get unique reservoir IDs from selected points
      selected_curve_numbers <- unique(selected_data$curveNumber)
      
      # Get the corresponding rowids
      temp_data <- selected_reservoirs$temp_data
      unique_reservoirs <- temp_data %>%
        distinct(rowid) %>%
        mutate(curve_number = row_number() - 1)
      
      highlighted_rowids <- unique_reservoirs$rowid[unique_reservoirs$curve_number %in% selected_curve_numbers]
      
      # Highlight corresponding reservoirs on map
      req(clp_sf())
      sf_wgs84 <- st_transform(clp_sf(), 4326)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          data = sf_wgs84,
          layerId = ~rowid,
          popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
          icon = ~awesomeIcons(
            icon = 'tint',
            markerColor = case_when(
              rowid %in% highlighted_rowids ~ 'orange',
              rowid %in% selected_reservoirs$rowids ~ 'red',
              TRUE ~ 'blue'
            ),
            iconColor = 'white',
            library = 'fa'
          ),
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            zoomToBoundsOnClick = TRUE,
            spiderfyOnMaxZoom = TRUE,
            removeOutsideVisibleBounds = TRUE,
            maxClusterRadius = 50
          )
        )
      
      showNotification(paste("Highlighted", length(highlighted_rowids), "reservoirs on map"), 
                      type = "message", duration = 2)
    }
  })
  
  # SDD plot selection to map highlighting
  observeEvent(event_data("plotly_selected", source = "sdd_plot"), {
    selected_data <- event_data("plotly_selected", source = "sdd_plot")
    
    if (!is.null(selected_data) && nrow(selected_data) > 0) {
      # Get unique reservoir IDs from selected points
      selected_curve_numbers <- unique(selected_data$curveNumber)
      
      # Get the corresponding rowids
      sdd_data <- selected_reservoirs$sdd_data
      unique_reservoirs <- sdd_data %>%
        distinct(rowid) %>%
        mutate(curve_number = row_number() - 1)
      
      highlighted_rowids <- unique_reservoirs$rowid[unique_reservoirs$curve_number %in% selected_curve_numbers]
      
      # Highlight corresponding reservoirs on map
      req(clp_sf())
      sf_wgs84 <- st_transform(clp_sf(), 4326)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          data = sf_wgs84,
          layerId = ~rowid,
          popup = ~create_enhanced_popup(rowid, display_name, area_sq_km, size_category, has_temp_data, has_sdd_data),
          icon = ~awesomeIcons(
            icon = 'tint',
            markerColor = case_when(
              rowid %in% highlighted_rowids ~ 'orange',
              rowid %in% selected_reservoirs$rowids ~ 'red',
              TRUE ~ 'blue'
            ),
            iconColor = 'white',
            library = 'fa'
          ),
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            zoomToBoundsOnClick = TRUE,
            spiderfyOnMaxZoom = TRUE,
            removeOutsideVisibleBounds = TRUE,
            maxClusterRadius = 50
          )
        )
      
      showNotification(paste("Highlighted", length(highlighted_rowids), "reservoirs on map"), 
                      type = "message", duration = 2)
    }
  })
  
  # Output for selection info display
  output$selection_info <- renderText({
    count <- length(selected_reservoirs$rowids)
    
    if (count == 0) {
      "Click on reservoirs to view their data"
    } else if (count == 1) {
      points <- nw_clp_points()
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