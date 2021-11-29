pkgs <- c(
  "h2o",       #High performance machine learning
  "lime",      #Explaining black-box models
  "recipes",   #Creating ML preprocessing steps
  "tidyverse", #Set of pkgs for data science:dplyr, ggplot2, purrr,tidyr...
  "tidyquant", #Financial time series pkg - Used for theme_tq ggplot2 theme
  "glue",      #Pasting text
  "cowplot",   #Handling multiple ggplots
  "GGally",    #Data understanding - visulaizations
  "skimr",     #Data understanding - summary information
  "fs",        # Working with the file system - directory structure
  "readxl",    # Reading Excel files
  "writexl"    # Writing to Excel files
)

install.packages(pkgs)