library(fs)

make_project_dir <- function(){
  
  dir_names <- c(
    "00_Data",
    "00_Scripts",
    "01_Business_Understanding",
    "02_Data_Understanding",
    "03_Data_Preparation",
    "04_Modeling",
    "05_Evaluation",
    "06_Deployment")
  
  dir_create(dir_names)
  dir_ls()
  
}
