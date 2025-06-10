---
editor_options: 
  markdown: 
    wrap: 72
---

# RAP Pro - Robust Autonomous Pressure and Inertial Devices (RAPID) analysis suite

A Shiny web application for processing and analyzing hydraulic passage
sensor data from RAPID sensors.

## 🎯 What This App Does

-   **Process raw sensor data** (.IMP and .HIG binary files) into CSV
    format
-   **Analyze pressure, acceleration, and rotation** time series data
-   **Delineate passage events** and calculate fish passage metrics
-   **Generate interactive visualizations** and exportable plots
-   **Calculate barotrauma risk metrics** for fish passage assessment
-   **Calculate srike and collision risk metrics** for fish passage
    assessment

## 📋 Prerequisites

-   **R** (version 4.4.0 or higher) - Download from
    [CRAN](https://cran.r-project.org/)
-   **RStudio** (recommended) - Download from
    [RStudio](https://posit.co/download/rstudio-desktop/)
-   **Git** (for cloning) - Download from [Git](https://git-scm.com/)

## 🚀 Quick Start

### 1. Clone the Project

Use RStudio to clone the project:
`File > New Project > Version Control > Git`
(<https://github.com/JNorman-hull/uoh-hifi-sensor.git>)

### 2. Setup R Environment

``` r
# In RStudio console:

# Install renv (if not already installed)
install.packages("renv")

# Restore the project packages
renv::restore()

# When prompted "Do you want to proceed? [Y/n]:", type 'Y'
# Wait for all R packages to install (this may take several minutes)

```

Note: RStudio may also show a yellow banner at the top saying "This project uses renv" with a "Restore" button - you can click that instead of typing the command.

### 3. Setup Python Environment

```r
# In RStudio console:
source("setup_python.R")

# This will:
# - Install miniconda (if needed)
# - Create 'rap_pro' Python environment  
# - Install required packages (numpy, pandas, scipy)
# - Test the setup
```

### 4. Run the App

``` r
# Open app.R in RStudio and click "Run App"
# Or run:
source("app.R")
```

The app will open in your browser at `http://localhost:xxxx`

## 📁 Data Setup

### Raw Sensor Data

1.  Create folder: `./raw_sens_data/`
2.  Add your RAPID sensor files:
    -   `.IMP` files (pressure, acceleration, rotation data)
    -   `.HIG` files (high-frequency acceleration data)
    -   Files should be named like: `B61-0703140718.IMP` and
        `B61-0703140718.HIG`

### Example Data Structure

```         
uoh-hifi-sensor/
├── raw_sens_data/           # Your raw sensor files
│   ├── B61-0703140718.IMP
│   ├── B61-0703140718.HIG
│   └── B62-0703140412.IMP
├── processed_sens_data/     # Generated automatically
└── shiny/                   # App files
    ├── app.R               # Main app file
    └── ...
```

## 🔧 App Workflow

1.  **Process Raw Data** - Convert binary files to CSV format
2.  **Add Deployment Info** - Add experimental metadata\
3.  **Delineate Passages** - Mark regions of interest in time series
4.  **Analyze Instruments** - Calculate pressure, acceleration, rotation
    metrics
5.  **Export Results** - Generate plots and summary data

## 🛠️ Troubleshooting

### R Package Issues

``` r
# If packages fail to install, try:
options(install.packages.compile.from.source = "never")
renv::restore()
```

### Python Environment Issues

``` r
# Check Python setup:
library(reticulate)
conda_list()  # Should show 'rap_pro' environment

# Recreate if needed:
conda_remove("rap_pro")
source("setup_python.R")
```

### App Won't Start

``` r
# Check all dependencies:
renv::status()
reticulate::py_config()

# Restart R session:
# Ctrl+Shift+F10 (or Session > Restart R)
```

## 📊 Using the App

### 1. Process Raw Data

-   Go to **"Sensor processing"** tab
-   Select sensors from the raw data index
-   Click **"Process Selected Sensors"**
-   Monitor progress in the processing log

### 2. Add Deployment Information

-   Select processed sensors
-   Fill in experimental details (site, pump type, flow conditions)
-   Save configuration for reuse

### 3. Time Series Analysis

-   **Delineation**: Mark passage regions (ingress, nadir, outgress)
-   **Normalization**: Normalize time series for comparison
-   **Visualization**: Interactive plots with export options

### 4. Instrument Analysis

-   **Pressure**: Calculate barotrauma metrics (RPC, LRPC)
-   **Acceleration**: Detect strikes and collision events
-   **Rotation**: Analyze rotational forces during passage

## 🎛️ Configuration

The app uses configuration files in `./shiny/config/`: -
`roi_config.txt` - Delineation templates - `pres_config.txt` - Pressure
analysis parameters\
- `acc_config.txt` - Acceleration analysis parameters -
`deployment_config.txt` - Deployment templates

## 📈 Output Data

Processed data is saved in `./processed_sens_data/`: - **CSV files** -
Time series data (minimal and full versions) - **Index files** - Sensor
metadata and processing status - **Instrument data** - Calculated
metrics and summary statistics


**Ready to analyze fish passage data!** 🐟📊
