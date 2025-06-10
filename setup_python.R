#' Setup Python Environment for RAP Pro
#' Run this once after cloning the project and establishing r environment

library(reticulate)

# Configuration
python_env_name <- "rap_pro"
python_version <- "3.10.14"

cat("Setting up Python environment for RAP Pro...\n")

# Check if miniconda is installed
if (!miniconda_installed()) {
  cat("Installing miniconda...\n")
  install_miniconda()
}

# Check if environment exists
envs <- conda_list()
if (!python_env_name %in% envs$name) {
  cat("Creating conda environment:", python_env_name, "\n")
  conda_create(envname = python_env_name, python_version = python_version)
} else {
  cat("Environment", python_env_name, "already exists\n")
}

# Install Python packages from requirements.txt
if (file.exists("requirements.txt")) {
  cat("Installing Python packages from requirements.txt...\n")
  py_install(packages = NULL, 
             requirements = "requirements.txt",
             envname = python_env_name, 
             method = "conda")
} else {
  cat("requirements.txt not found, installing core packages...\n")
  py_install(packages = c("numpy==1.24.3", "pandas==2.0.3", "scipy==1.11.1"), 
             envname = python_env_name, 
             method = "conda")
}

# Test the environment
use_condaenv(python_env_name, required = TRUE)
cat("Testing Python setup...\n")
py_run_string("import numpy, pandas, scipy; print('Python packages loaded successfully')")

cat("✓ Python environment setup complete!\n")
cat("You can now run the app with app.R\n")