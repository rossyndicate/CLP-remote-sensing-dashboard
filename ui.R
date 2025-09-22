fluidPage(
  # Include custom CSS
  includeCSS("www/custom.css"),
  
  # Header row with styling
  fluidRow(
    class = "dashboard-header",
    column(6, h1("Cache La Poudre Remote Sensing Dashboard", class = "dashboard-title")),
    column(6, 
           actionButton("reset_btn", "Reset Selections", class = "reset-button"))
  ),
  
  # Main content row
  fluidRow(
    column(6, 
           div(class = "content-card",
               div(class = "map-container",
                   leafletOutput("map", height = "400px")))),
    column(6,
           div(class = "content-card plot-section",
               h3("Selected Point", class = "plot-title",
                  tags$i(class = "fas fa-thermometer-half")),
               plotlyOutput("temperature_plot"))) 
  )
)