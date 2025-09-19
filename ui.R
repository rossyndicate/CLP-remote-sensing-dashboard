fluidPage(
  titlePanel("Cache La Poudre Remote Sensing Dashboard"),
  
  fluidRow(
    # Left column - 50% width for map
    column(
      width = 6,
      div(
        style = "height: 80vh; border: 1px solid #ddd; padding: 10px;",
        h4("CLP Reservoirs Map"),
        leafletOutput("map", height = "90%")
      )
    ),
    
    # Right column - 50% width for plots
    column(
      width = 6,
      div(
        style = "height: 80vh; border: 1px solid #ddd; padding: 10px; overflow-y: auto;",
        h4("Time Series Data"),
        actionButton("reset_btn", "Reset Selection", class = "btn-primary", style = "margin-bottom: 15px;"),
        br(),
        
        # Temperature plot
        div(
          style = "margin-bottom: 20px;",
          h5("Temperature Data"),
          plotlyOutput("temp_plot", height = "350px")
        ),
        
        # SDD plot (using temp data for now)
        div(
          style = "margin-bottom: 20px;",
          h5("SDD Data (Placeholder - using temp data)"),
          plotlyOutput("sdd_plot", height = "350px")
        )
      )
    )
  )
)