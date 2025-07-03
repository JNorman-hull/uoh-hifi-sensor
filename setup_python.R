#' Setup Python Environment for RAP Pro
#' Run this once after cloning the project and establishing r environment

library(reticulate)

## Configuration
python_env_name <- "rap_pro"
python_version <- "3.10.14"

cat("Setting up Python environment for RAP Pro...\n")

# Check if miniconda is installed (safer approach)
miniconda_exists <- tryCatch({
  miniconda_path()
  TRUE
}, error = function(e) {
  FALSE
})

if (!miniconda_exists) {
  cat("Installing miniconda...\n")
  install_miniconda(force = TRUE)
}

# Check if environment exists
envs <- conda_list()
if (!python_env_name %in% envs$name) {
  cat("Creating conda environment:", python_env_name, "\n")
  conda_create(envname = python_env_name, python_version = python_version)
} else {
  cat("Environment", python_env_name, "already exists\n")
}

# Install Python packages
cat("Installing Python packages...\n")
if (file.exists("requirements.txt")) {
  cat("Installing from requirements.txt...\n")
  py_install(packages = NULL, 
             requirements = "requirements.txt",
             envname = python_env_name, 
             method = "conda")
} else {
  cat("Installing core packages...\n")
  py_install(packages = c("numpy==1.24.3", "pandas==2.0.3", "scipy==1.11.1"), 
             envname = python_env_name, 
             method = "conda")
}

# Test the environment
cat("Testing Python setup...\n")
use_condaenv(python_env_name, required = TRUE)

tryCatch({
  py_run_string("import numpy, pandas, scipy; print('Python packages loaded successfully')")
  cat("✓ Python environment setup complete!\n")
  cat("You can now run the app with app.R\n")
}, error = function(e) {
  cat("Warning: Could not test Python packages, but environment should be ready\n")
  cat("Error:", e$message, "\n")
})