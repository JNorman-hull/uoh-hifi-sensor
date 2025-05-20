# RAPID python import script

reticulate::source_python('./scripts/RAPID_import.py')

#Python installation####
#Run only for first time installation 

#Install reticulate to use python within R
install.packages("reticulate")

#check package operation
library(reticulate)

#install miniconda
install_miniconda()

#Verify python installation
py_config()

#installing required python packages for txt > csv conversion

py_install("jupyter_client")
py_install("matplotlib")
py_install("pandas")
py_install("quaternion")
py_install("opencv")
py_install("statsmodels")

py_install("pillow")
#2. Python operation####

