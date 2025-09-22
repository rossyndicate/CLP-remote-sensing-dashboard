plot_temp <- function(plot_df, plot_check) {
  
  if (plot_check) {
    p <- plot_df %>%
      pivot_longer(cols = -date, names_to = "point", values_to = "temperature") %>% 
      left_join(id_color_lookup, by = "point") %>% 
      plot_ly(x = ~date, y = ~temperature, color = ~ point, colors = ~color,
              type = 'scatter', mode = 'markers') %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "Date"),
        yaxis = list(title = "Temperature (°C)")
      )
  } else {
    # Create an empty plotly plot with message
    p <- plot_ly() %>%
      add_annotations(
        text = "Click a point on the map to select it",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE
      )
  }
  
  return(p)
}