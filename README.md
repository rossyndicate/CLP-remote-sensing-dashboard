# Remote Sensing Dashboard for the Cache la Poudre Watershed

**Enhanced Interactive Shiny Dashboard** displaying historical remote sensing data with estimated water quality parameters for NASA proposal demonstration.

## Features

### 🗺️ Interactive Mapping
- **Professional multi-layer map** with satellite, topographic, and light themes
- **10km analysis buffer zone** visualization around CLP reservoirs
- **Enhanced popups** with detailed reservoir information and data availability
- **Smart clustering** for better performance with many data points
- **Minimap and scale bar** for easy navigation

### 📊 Advanced Data Visualization
- **Synchronized plots** with connected zoom/pan functionality
- **Dual-axis temperature display** (Celsius and Fahrenheit)
- **Water clarity categories** for SDD measurements
- **Crossfilter-style selection** - select points on plots to highlight on map
- **Enhanced tooltips** with detailed information

### 🔧 Robust Data Processing
- **Graceful error handling** for missing or invalid data
- **Data quality indicators** and validation
- **Temperature conversion** with quality flags
- **SDD data processing** with fallback to realistic simulated data

### 💫 Professional UI/UX
- **Modern Bootstrap 5 theme** with custom CSS styling
- **Responsive design** for different screen sizes
- **Loading spinners** for all data operations
- **Visual feedback** for user actions
- **Professional gradient styling** suitable for stakeholder presentations

## Quick Start

### Prerequisites
- R (>= 4.0.0)
- Required R packages (automatically installed on first run)

### Running the Dashboard
```r
# In R console, navigate to the dashboard directory
setwd("path/to/CLP-remote-sensing-dashboard")

# Run the dashboard
shiny::runApp()
```

### Data Files
- `data/NW_CLP_all_points.csv` - Reservoir locations and metadata
- `data/clp_temp_rs_estimate_nocorr_v2024-10-10.feather` - Temperature estimates
- `data/clp_sdd_rs_estimate_v2024-10-10.feather` - Water clarity (SDD) estimates

## Usage

1. **Select reservoirs** by clicking on map markers
2. **View time series** for temperature and water clarity
3. **Zoom and pan** plots - they stay synchronized
4. **Select data points** on plots to highlight corresponding reservoirs
5. **Switch map layers** using the layer control
6. **Reset selection** using the reset button

## Technical Details

- Built with **Shiny framework** using modular `ui.R`, `server.R`, and `global.R` structure
- Uses **leaflet** for interactive mapping with custom clustering
- Uses **plotly** for interactive charts with synchronized behavior
- Uses **sf** for spatial data processing with Colorado State Plane projection
- **Bootstrap 5** theme with custom CSS for professional appearance

## Enhancement Summary

This version includes significant improvements over the basic prototype:
- ✅ Professional visual design suitable for NASA proposal
- ✅ Smooth, responsive interactivity between map and plots
- ✅ Advanced data connectivity and crossfilter functionality
- ✅ Robust error handling and data validation
- ✅ Enhanced user experience with loading indicators and feedback

---

**Repository License:** MIT

**Contact:** B Steele (b dot steele at colostate dot edu)

The code in this repository is covered by the MIT use license. We request that all downstream uses of this work be available to the public when possible.
