fluidPage(
  # Enhanced styling with custom theme
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c3e50",
    primary = "#3498db",
    secondary = "#95a5a6",
    success = "#2ecc71",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto Slab")
  ),
  
  # Custom CSS for enhanced styling
  tags$head(
    tags$style(HTML("
      .main-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      .panel-card {
        background: white;
        border-radius: 15px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        border: none;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      .panel-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 12px 35px rgba(0,0,0,0.12);
      }
      .map-panel {
        height: 85vh;
        padding: 20px;
        overflow: hidden;
      }
      .plots-panel {
        height: 85vh;
        padding: 20px;
        overflow-y: auto;
      }
      .section-title {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 15px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .reset-btn {
        background: linear-gradient(45deg, #e74c3c, #c0392b);
        border: none;
        border-radius: 25px;
        padding: 10px 20px;
        color: white;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(231, 76, 60, 0.3);
      }
      .reset-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(231, 76, 60, 0.4);
        background: linear-gradient(45deg, #c0392b, #a93226);
      }
      .plot-container {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
        border-left: 4px solid #3498db;
      }
      .loading-spinner {
        text-align: center;
        padding: 40px;
        color: #7f8c8d;
      }
    "))
  ),
  
  # Enhanced header
  div(class = "main-header",
    div(
      style = "display: flex; align-items: center; justify-content: space-between;",
      div(
        h1("Cache La Poudre Remote Sensing Dashboard", 
           style = "margin: 0; font-size: 2.2rem; font-weight: 300;"),
        p("Interactive visualization of water quality parameters across CLP reservoirs", 
          style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 1.1rem;")
      ),
      div(
        tags$i(class = "fas fa-satellite", style = "font-size: 3rem; opacity: 0.7;")
      )
    )
  ),
  
  fluidRow(
    # Left column - Enhanced map panel
    column(
      width = 6,
      div(
        class = "panel-card map-panel",
        h4(class = "section-title",
           tags$i(class = "fas fa-map-marked-alt", style = "color: #3498db;"),
           "Interactive Reservoir Map"
        ),
        div(
          style = "height: calc(100% - 60px); position: relative;",
          withSpinner(
            leafletOutput("map", height = "100%"),
            type = 6,
            color = "#3498db",
            size = 0.8
          ),
          # Map legend
          div(
            style = "position: absolute; bottom: 10px; right: 10px; background: rgba(255,255,255,0.95); 
                     padding: 10px; border-radius: 8px; font-size: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
            tags$div(
              tags$span(style = "display: inline-block; width: 12px; height: 12px; background: #3498db; 
                                border-radius: 50%; margin-right: 8px; border: 2px solid white;"),
              "Available Reservoirs"
            ),
            tags$div(style = "margin-top: 5px;",
              tags$span(style = "display: inline-block; width: 12px; height: 12px; background: #e74c3c; 
                                border-radius: 50%; margin-right: 8px; border: 2px solid white;"),
              "Selected Reservoirs"
            )
          )
        )
      )
    ),
    
    # Right column - Enhanced plots panel
    column(
      width = 6,
      div(
        class = "panel-card plots-panel",
        h4(class = "section-title",
           tags$i(class = "fas fa-chart-line", style = "color: #2ecc71;"),
           "Time Series Analysis"
        ),
        
        # Control section
        div(
          style = "margin-bottom: 20px; text-align: center;",
          actionButton("reset_btn", 
                      tags$span(tags$i(class = "fas fa-undo"), " Reset Selection"), 
                      class = "reset-btn"),
          div(
            textOutput("selection_info"), 
            style = "margin-top: 10px; font-size: 14px; color: #7f8c8d;"
          )
        ),
        
        # Temperature plot container
        div(
          class = "plot-container",
          h5(class = "section-title",
             tags$i(class = "fas fa-thermometer-half", style = "color: #e67e22;"),
             "Surface Temperature"
          ),
          withSpinner(
            plotlyOutput("temp_plot", height = "350px"),
            type = 6,
            color = "#e67e22",
            size = 0.6
          )
        ),
        
        # SDD plot container
        div(
          class = "plot-container",
          h5(class = "section-title",
             tags$i(class = "fas fa-eye", style = "color: #9b59b6;"),
             "Secchi Disk Depth (Water Clarity)"
          ),
          withSpinner(
            plotlyOutput("sdd_plot", height = "350px"),
            type = 6,
            color = "#9b59b6",
            size = 0.6
          )
        )
      )
    )
  )
)