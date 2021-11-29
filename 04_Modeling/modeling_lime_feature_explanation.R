# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_numeric_predictors()) %>%
  step_num2factor(JobLevel, levels = c('1','2','3','4','5','6')) %>%
  step_num2factor(StockOptionLevel, levels=c('0','1','2','3'), transform = function(x) x+1) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----
h2o.init()

automl_leader <- h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409")

automl_leader

# 3. LIME ----


# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as_tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl  

test_tbl %>%
  slice(5) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
   model           =  automl_leader,
   bin_continuous  = TRUE,
   n_bins          = 4,
   quantile_bins   = TRUE
  )

explainer

explanation <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5)

explanation %>%
  as_tibble() %>%
  select(feature:prediction)


plot_features(explanation = explanation, ncol = 1)

# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5)

explanation %>%
  as_tibble()


plot_features(explanation = explanation, ncol = 4)

plot_explanations(explanation = explanation)
