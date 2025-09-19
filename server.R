function(input, output, session) {
  
  # Load and process data
  nw_clp_points <- reactive({
    read_csv(here("data", "NW_CLP_all_points.csv"), show_col_types = FALSE) %>%
      filter(data_group == "CLP")
  })
  
  clp_temp <- reactive({
    read_feather(here("data", "clp_temp_rs_estimate_nocorr_v2024-10-10.feather")) %>%
      mutate(
        # Convert from Kelvin to Celsius and Fahrenheit
        temp_celsius = as.numeric(med_SurfaceTemp) - 273.15,
        temp_fahrenheit = (temp_celsius * 9/5) + 32
      )
  })
  
  clp_sdd <- reactive({
    # SDD data is empty, so we'll create a placeholder structure
    # Using temp data structure but with different values for demonstration
    temp_data <- clp_temp()
    temp_data %>%
      mutate(
        # Placeholder SDD values (normally would come from the SDD file)
        sdd_mean = runif(nrow(.), min = 0.5, max = 5.0)  # Random SDD values for demo
      )
  })
  
  # Convert points to sf object with Colorado State Plane projection
  clp_sf <- reactive({
    points <- nw_clp_points()
    
    # Create sf object from lat/long (WGS84)
    sf_points <- st_as_sf(points, 
                         coords = c("Longitude", "Latitude"), 
                         crs = 4326)  # WGS84
    
    # Transform to Colorado State Plane Central (EPSG:3994)
    sf_points_transformed <- st_transform(sf_points, crs = 3994)
    
    return(sf_points_transformed)
  })
  
  # Calculate map bounds with 10km buffer
  map_bounds <- reactive({
    sf_data <- clp_sf()
    
    # Get bounding box
    bbox <- st_bbox(sf_data)
    
    # Add 10km buffer (10000 meters)
    list(
      west = bbox[1] - 10000,
      east = bbox[3] + 10000,
      south = bbox[2] - 10000,
      north = bbox[4] + 10000
    )
  })
  
  # Reactive values for selected reservoirs
  selected_reservoirs <- reactiveValues(
    rowids = c(),
    data = data.frame()
  )
  
  # Reset function
  observeEvent(input$reset_btn, {
    selected_reservoirs$rowids <- c()
    selected_reservoirs$data <- data.frame()
    
    # Clear map selection (will be implemented with map)
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = st_transform(clp_sf(), 4326),  # Transform back to WGS84 for leaflet
        layerId = ~rowid,
        radius = 6,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        color = "blue",
        fillColor = "lightblue",
        popup = ~paste0(
          "<strong>", ifelse(is.na(gnis_name), "Unnamed Reservoir", gnis_name), "</strong><br>",
          "Row ID: ", rowid, "<br>",
          "Area: ", round(area_sq_km, 3), " km²"
        )
      )
  })
  
  # Leaflet map output
  output$map <- renderLeaflet({
    # Transform sf data back to WGS84 for leaflet
    sf_wgs84 <- st_transform(clp_sf(), 4326)
    
    # Get Colorado state boundary for max extent
    colorado <- states(cb = TRUE) %>%
      filter(NAME == "Colorado") %>%
      st_transform(4326)
    
    co_bounds <- st_bbox(colorado)
    
    leaflet() %>%
      addTiles() %>%
      # Set view to CLP area with buffer
      fitBounds(
        lng1 = min(st_coordinates(sf_wgs84)[,1]) - 0.1,
        lat1 = min(st_coordinates(sf_wgs84)[,2]) - 0.1,
        lng2 = max(st_coordinates(sf_wgs84)[,1]) + 0.1,
        lat2 = max(st_coordinates(sf_wgs84)[,2]) + 0.1
      ) %>%
      # Set max bounds to Colorado
      setMaxBounds(
        lng1 = co_bounds[1],
        lat1 = co_bounds[2], 
        lng2 = co_bounds[3],
        lat2 = co_bounds[4]
      ) %>%
      addCircleMarkers(
        data = sf_wgs84,
        layerId = ~rowid,
        radius = 6,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        color = "blue",
        fillColor = "lightblue",
        popup = ~paste0(
          "<strong>", ifelse(is.na(gnis_name), "Unnamed Reservoir", gnis_name), "</strong><br>",
          "Row ID: ", rowid, "<br>",
          "Area: ", round(area_sq_km, 3), " km²"
        )
      )
  })
  
  # Map click observer
  observeEvent(input$map_marker_click, {
    clicked_id <- input$map_marker_click$id
    
    if (clicked_id %in% selected_reservoirs$rowids) {
      # Remove if already selected
      selected_reservoirs$rowids <- selected_reservoirs$rowids[selected_reservoirs$rowids != clicked_id]
    } else {
      # Add if not selected
      selected_reservoirs$rowids <- c(selected_reservoirs$rowids, clicked_id)
    }
    
    # Update selected data
    if (length(selected_reservoirs$rowids) > 0) {
      temp_data <- clp_temp()
      selected_reservoirs$data <- temp_data %>%
        filter(rowid %in% selected_reservoirs$rowids)
    } else {
      selected_reservoirs$data <- data.frame()
    }
    
    # Update map markers
    sf_wgs84 <- st_transform(clp_sf(), 4326)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = sf_wgs84,
        layerId = ~rowid,
        radius = 6,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        color = ifelse(sf_wgs84$rowid %in% selected_reservoirs$rowids, "red", "blue"),
        fillColor = ifelse(sf_wgs84$rowid %in% selected_reservoirs$rowids, "orange", "lightblue"),
        popup = ~paste0(
          "<strong>", ifelse(is.na(gnis_name), "Unnamed Reservoir", gnis_name), "</strong><br>",
          "Row ID: ", rowid, "<br>",
          "Area: ", round(area_sq_km, 3), " km²"
        )
      )
  })
  
  # Temperature plot output
  output$temp_plot <- renderPlotly({
    if (nrow(selected_reservoirs$data) == 0) {
      # Empty plot when no selection
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5), label = "Select reservoirs on the map to view data", size = 5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p))
    }
    
    plot_data <- selected_reservoirs$data
    
    # Create temperature plot with dual axes
    p <- ggplot(plot_data, aes(x = date, y = temp_celsius, color = factor(rowid))) +
      geom_line(size = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)")
      ) +
      labs(
        title = "Temperature Time Series",
        subtitle = "Selected CLP Reservoirs",
        x = "Date",
        color = "Reservoir ID"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")
      )
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      event_register("plotly_zoom") %>%
      event_register("plotly_relayout")
  })
  
  # SDD plot output (using temperature data as placeholder)
  output$sdd_plot <- renderPlotly({
    if (nrow(selected_reservoirs$data) == 0) {
      # Empty plot when no selection
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5), label = "Select reservoirs on the map to view data", size = 5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p))
    }
    
    # Use SDD placeholder data
    sdd_data <- clp_sdd() %>%
      filter(rowid %in% selected_reservoirs$rowids)
    
    # Create SDD plot
    p <- ggplot(sdd_data, aes(x = date, y = sdd_mean, color = factor(rowid))) +
      geom_line(size = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      labs(
        title = "SDD Time Series (Placeholder Data)",
        subtitle = "Selected CLP Reservoirs",
        x = "Date",
        y = "SDD (meters)",
        color = "Reservoir ID"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      event_register("plotly_zoom") %>%
      event_register("plotly_relayout")
  })
  
  # Observe plotly events for connected plots (basic framework for future enhancement)
  observeEvent(event_data("plotly_zoom", source = "temp_plot"), {
    # Framework for connecting plot zooms - can be enhanced later
    zoom_data <- event_data("plotly_zoom", source = "temp_plot")
    # Future: Apply same zoom to SDD plot
  })
  
  observeEvent(event_data("plotly_zoom", source = "sdd_plot"), {
    # Framework for connecting plot zooms - can be enhanced later  
    zoom_data <- event_data("plotly_zoom", source = "sdd_plot")
    # Future: Apply same zoom to temperature plot
  })
  
}