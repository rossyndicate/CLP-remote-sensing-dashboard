function(input, output, session) {
  
  # Initialize reactiveVals ----
  selected_ids <- reactiveVal(c())
  
  # Initialize plotting data frames ----
  temp_plot_data <- reactiveVal({temp_plot_df})
  sdd_plot_data <- reactiveVal({sdd_plot_df})
  
  # Leaflet map
  output$map <- renderLeaflet ({ 
    nw_clp_map(point_df = points)
  })
  
  # Temperature plot 
  output$temperature_plot <- renderPlotly({
    plot_temp(plot_df = temp_plot_data())
  })
  
  # SDD plot
  output$sdd_plot <- renderPlotly({
    plot_sdd(plot_df = sdd_plot_data())
  })
  
  # Handle map point clicks (heavy lifting of the app is done here) ----
  observeEvent(input$map_marker_click, {
    
    point_id <- input$map_marker_click$id
    clicked_lat <- input$map_marker_click$lat
    clicked_lng <- input$map_marker_click$lng
    
    updated_ids <- update_ids(
      point_list = selected_ids(),
      id = point_id
    ) 
    
    if (length(updated_ids) < 6) {
      
      updated_temp_data <- update_temp_data(
        plot_df = temp_plot_data(),
        id = point_id
      )
      
      updated_sdd_data <- update_sdd_data(
        plot_df = sdd_plot_data(),
        id = point_id
      )
      
      selected_ids(updated_ids)
      
      temp_plot_data(updated_temp_data)
      
      sdd_plot_data(updated_sdd_data)
      
      update_nw_clp_map(
        point_list = selected_ids(),
        lat = clicked_lat,
        lng = clicked_lng
      )
      
    } else{
      # Show a message saying that you have maxed out your selection
      showNotification("Max selection hit!\nUnselect sites or reset selections.", type = "message", duration = 2)
    }
  })  
  
  # Reset button ----
  observeEvent(input$reset_btn, {
    # Reset selected_ids to empty
    selected_ids(c())
    
    # Reset temp_plot_data to original temp_plot_df
    temp_plot_data(temp_plot_df)
    
    # Reset sdd_plot_data to original sdd_plot_df
    sdd_plot_data(sdd_plot_df)
    
    # Reset Map
    output$map <- renderLeaflet ({ 
      nw_clp_map(point_df = points)
    })
    
    # Show a brief confirmation message
    showNotification("Selections reset!", type = "message", duration = 2)
  })
}
