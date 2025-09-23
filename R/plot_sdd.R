plot_sdd <- function(plot_df) {
  
  has_data <- ncol(plot_df) > 3
  
  if (has_data) {
    p <- plot_df %>%
      pivot_longer(cols = -c(date, mission, shape), names_to = "perm", values_to = "sdd") %>% 
      left_join(sdd_lookup, by = "perm") %>% 
      plot_ly(x = ~date, y = ~sdd, 
              color = ~ifelse(is.na(gnis_name), perm, gnis_name), 
              colors = ~color,
              symbol = ~mission,
              type = 'scatter', mode = 'markers',
              hovertemplate = paste(
                "<b>Perm:</b> %{customdata}<br>",
                "<b>Date:</b> %{x}<br>",
                "<b>Temperature:</b> %{y}°C<br>",
                "<extra></extra>"
              ),
              customdata = ~perm) %>%
      layout(
        showlegend = TRUE,
        xaxis = list(title = "Date"),
        yaxis = list(title = "SDD (m)"),
        legend = list(
          orientation = "h",  # Horizontal orientation
          x = 0.5,           # Center horizontally
          xanchor = "center", # Anchor at center
          y = -0.2           # Position below the plot
        ),
        margin = list(l = 50, r = 20, t = 20, b = 80)
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