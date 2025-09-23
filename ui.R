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
                   leafletOutput("map", height = "600px")))),
    column(6,
           div(class = "content-card dashboard-plots",  # First plot card
               div(class = "plot-section",
                   h3("Temperature", class = "plot-title",
                      tags$i(class = "fas fa-thermometer-half")),
                   plotlyOutput("temperature_plot"))),
           div(class = "content-card dashboard-plots",  # Second plot card  
               div(class = "plot-section",
                   h3("SDD", class = "plot-title",
                      tags$i(class = "fas fa-thermometer-half")),
                   plotlyOutput("sdd_plot"))))
  )
)