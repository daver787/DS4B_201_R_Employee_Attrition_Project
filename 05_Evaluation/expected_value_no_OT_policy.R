# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# NO OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


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
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, levels = c('1','2','3','4','5','6')) %>%
    step_num2factor(StockOptionLevel, levels = c('0','1','2','3'), transform = function(x) x+1) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409")

automl_leader

#3. Expected Value ----

# 3.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as_tibble() %>%
  bind_cols(
    test_tbl %>% 
      select(EmployeeNumber, MonthlyIncome, OverTime)
  )

predictions_with_OT_tbl

ev_with_OT_tbl <- predictions_with_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
                                               n                       = 1,
                                              salary                   = MonthlyIncome *12,
                                              net_revenue_per_employee = 250000
                                              )
      ) %>%
  mutate(
    cost_of_policy_change = 0
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) + 
      No * cost_of_policy_change
  )

ev_with_OT_tbl

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarize(total_expected_attrition_cost_0 = sum(expected_attrition_cost))

# 3.2 Calculating Expected Value Without OT ----

test_without_OT_tbl <- test_tbl %>%
  mutate(OverTime = fct_recode(OverTime, "No" = "Yes"))

test_without_OT_tbl

predictions_without_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>%
  as_tibble() %>%
  bind_cols(
    test_tbl %>% 
      select(EmployeeNumber, MonthlyIncome, OverTime),
    test_without_OT_tbl %>%
      select(OverTime)
    ) %>%
  rename(
    OverTime_0 = OverTime...6,
    OverTime_1 = OverTime...7
  )

predictions_without_OT_tbl

avg_overtime_pct <- 0.10

ev_without_OT_tbl <- predictions_without_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n                       = 1,
      salary                   = MonthlyIncome *12,
      net_revenue_per_employee = 250000
    )
  ) %>%
  mutate(
    cost_of_policy_change = case_when(
      OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_overtime_pct * attrition_cost,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) + 
      No * cost_of_policy_change
  )
  
ev_without_OT_tbl

total_ev_without_OT_tbl <- ev_without_OT_tbl %>%
  summarize(total_expected_attrition_cost_1 = sum(expected_attrition_cost))

total_ev_with_OT_tbl

total_ev_without_OT_tbl

# 3.3 Savings Calculation ----

bind_cols(
  total_ev_with_OT_tbl,
  total_ev_without_OT_tbl
) %>%
  mutate(
    savings     = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
    pct_savings = savings / total_expected_attrition_cost_0
  ) 







