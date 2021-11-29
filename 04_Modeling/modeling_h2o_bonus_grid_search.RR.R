#BONUS: GRID SEARCH & CV ----
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)

path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

#Processing Pipeline 
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

#Machine Learning Preprocessing

recipe_obj <- recipe(Attrition~., data = train_readable_tbl) %>%
  step_zv(all_numeric_predictors()) %>%
  step_num2factor(JobLevel, levels = c('1','2','3','4','5','6')) %>%
  step_num2factor(StockOptionLevel, levels=c('0','1','2','3'), transform = function(x) x+1) %>%
  prep()

train_tbl <- bake(recipe_obj,new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj,new_data = test_readable_tbl)

split_h2o <- h2o.splitFrame(as.h2o(train_tbl),ratios = c(0.85), seed = 1234 )
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o),y)




h2o.init()
deeplearning_h20 <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_grid_1_AutoML_1_20211111_103409_model_1")

deeplearning_h20

h2o.performance(deeplearning_h20, newdata = as.h2o(test_tbl))

deeplearning_grid_01 <- h2o.grid(
  algorithm = "deeplearning", 
  grid_id = "deeplearning_grid_01",
  
  #h2o.deeplearning()
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  
  hyper_params = list(
    epochs = c(10, 50, 100),
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20))
    
  )
)

h2o.getGrid(grid_id = "deeplearning_grid_01", sort_by = "auc" , decreasing = TRUE)

deeplearning_grid_01_model_6 <- h2o.getModel("deeplearning_grid_01_model_6")

deeplearning_grid_01_model_6 %>% h2o.auc(train = TRUE, valid = TRUE, xval = TRUE)

deeplearning_grid_01_model_6 %>%
  h2o.performance(newdata = as.h2o(test_tbl))
