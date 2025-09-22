function(input, output, session) {
  
  # Initialize reactiveVals
  selected_ids <- reactiveVal(c())
  
  # Initialize empty plotting dataframe, based on the inputs for temp_ts
  temp_plot_data <- reactiveVal({temp_plot_df})
  
  # Reset button functionality
  observeEvent(input$reset_btn, {
    # Reset selected_ids to empty
    selected_ids(c())
    
    # Reset temp_plot_data to original temp_plot_df
    temp_plot_data(temp_plot_df)
    
    # Reset Map
    output$map <- renderLeaflet ({ 
      nw_clp_map(point_df = points)
    })
    
    # Optional: Show a brief confirmation message
    showNotification("Selections reset!", type = "message", duration = 2)
  })
  
  # leaflet plot
  output$map <- renderLeaflet ({ 
    nw_clp_map(point_df = points)
  })
  
  # Handle map point clicks
  observeEvent(input$map_marker_click, {
    
    point_id <- input$map_marker_click$id
    clicked_lat <- input$map_marker_click$lat
    clicked_lng <- input$map_marker_click$lng
    
    updated_ids <- point_click(
      point_list = selected_ids(),
      id = point_id
    ) 
    
    updated_temp_data <- update_temp_data(
      plot_df = temp_plot_data(),
      id = point_id
    )
    
    selected_ids(updated_ids)
    
    temp_plot_data(updated_temp_data)
    
    update_nw_clp_map(
      point_df = points,
      point_list = selected_ids(),
      lat = clicked_lat,
      lng = clicked_lng
    )
  })  
  
  # Temperature plot
  output$temperature_plot <- renderPlotly({
    
    plot_temp(
      plot_df = temp_plot_data(),
      plot_check = length(selected_ids()) != 0
    )
    
  })
  
}
