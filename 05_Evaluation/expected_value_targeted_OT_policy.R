# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

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


# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
  h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
  h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as_tibble()


rates_by_threshold_tbl %>%
  glimpse()

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  gather(key = key, value = value, tnr:tpr, factor_key = TRUE) %>%
  mutate(key = fct_reorder2(key, threshold, value)) %>%
  ggplot(aes(x = threshold, value, color = key)) +
  geom_point() +
  geom_smooth() +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "right") +
  labs(
    title = "Expected Rates",
    y     = "Value",
    x     = "Threshold"
  )



# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

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


total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarize(total_expected_attrition_cost_0 = sum(expected_attrition_cost))

total_ev_with_OT_tbl

# 4.2 Calculating Expected Value With Targeted OT ----

max_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

max_f1_tbl

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold


test_targeted_OT_tbl <- test_tbl %>%
  add_column(Yes = predictions_with_OT_tbl$Yes) %>% 
  mutate(
    OverTime = case_when(
      Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
      TRUE ~ OverTime
    )
  ) 
  select(-Yes)



predictions_targeted_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_targeted_OT_tbl))%>%
  as_tibble() %>%
  bind_cols(test_tbl %>% select(EmployeeNumber, MonthlyIncome, OverTime),
            test_targeted_OT_tbl %>% select(OverTime))%>%
  rename(OverTime_0 = OverTime...6,
         OverTime_1 = OverTime...7)

predictions_targeted_OT_tbl

avg_overtime_pct <- 0.10


ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n                       = 1,
      salary                   = MonthlyIncome *12,
      net_revenue_per_employee = 250000
    ) ) %>%
  mutate(
    cost_of_policy_change = case_when(
      OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    cb_tn = cost_of_policy_change,
    cb_fp = cost_of_policy_change,
    cb_tp = cost_of_policy_change + attrition_cost,
    cb_fn = cost_of_policy_change + attrition_cost,
    expected_attrition_cost = 
      Yes * (tpr * cb_tp + fnr * cb_fn) + 
      No * (tnr * cb_tn + fpr * cb_fp)
      
  )
      
ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
  summarize(
    total_expected_attrition_cost_1 = sum(expected_attrition_cost)
  )

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
          total_ev_with_OT_tbl,
          total_ev_targeted_OT_tbl
          ) %>%
  mutate(savings     = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
         pct_savings = savings / total_expected_attrition_cost_0
         )


savings_tbl


# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

data <- test_tbl
h2o_model <- automl_leader



calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1){
  
  data_0_tbl <- as_tibble(data)
  
  #4. Expected Value
  
  #4.1 Calculating Expected Value with OT
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata =as.h2o(data_0_tbl)) %>%
    as_tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = 250000
      )
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(expected_attrition_cost = 
             Yes * (attrition_cost + cost_of_policy_change) +
             No * (cost_of_policy_change)
      )
    
  total_ev_0_tbl <- ev_0_tbl %>%
    summarize(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes)
  
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as_tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>% 
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime...6,
      OverTime_1 = OverTime...7
    )
  
  avg_overtime_pct <- 0.10
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n                        = 1,
        salary                   = MonthlyIncome * 12,
        net_revenue_per_employee = 250000
        
      )
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_1 == "No" & OverTime_0 =="Yes" ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_fn = attrition_cost + cost_of_policy_change,
      cb_tp = attrition_cost + cost_of_policy_change,
      expected_attrition_cost = Yes * (tpr * cb_tp + fnr * cb_fn) +
        No * (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  #4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings     = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

#No OT Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 0,
                                 tnr = 0, fnr = 0, tpr = 1, fpr = 1)


# Do Nothing Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 1, tnr = 1, fnr = 1, tpr = 0, fpr = 0)
#Threshold @ Max F1

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1))


max_f1_savings <- calculate_savings_by_threshold(test_tbl, automl_leader,
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               fnr = max_f1_tbl$fnr,
                               fpr = max_f1_tbl$fpr,
                               tpr = max_f1_tbl$tpr)


# 5.2 Optimization ----
smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
  select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        threshold = threshold,
        tnr = tnr,
        fnr = fnr,
        fpr = fpr,
        tpr = tpr
      ), .f =  partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader))
  
)

rates_by_threshold_optimized_tbl

rates_by_threshold_optimized_tbl %>%
  ggplot(aes(x = threshold, y = savings)) +
  geom_line(color =palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +

  #Optimal point and label
  geom_point(shape = 21, size = 5, color = palette_light()[[3]], data = 
               rates_by_threshold_optimized_tbl %>% filter(savings == max(savings))) +
  
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -1, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl %>% filter(savings == max(savings))) +
  
  #F1 Max
  geom_vline(xintercept = max_f1_tbl$threshold,color =palette_light()[[5]], size = 2 ) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
           color = palette_light()[[1]]) +
  
  
  # No OT Policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]], data = 
               rates_by_threshold_optimized_tbl %>% filter(threshold == min(threshold))) +
  
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>% filter(threshold == min(threshold))) +
  
  
  # Do Nothing Policy
  
  geom_point(shape = 21, size = 5, color = palette_light()[[2]], data = 
               rates_by_threshold_optimized_tbl %>% filter(threshold == max(threshold))) +
  
  geom_label(aes(label = scales::dollar(round(savings,0))),
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>% filter(threshold == max(threshold))) +

  
  #Aesthetics
  theme_tq() +
  expand_limits(x = c(-.1,1.1), y = 8e5) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0,1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Optimization Results: Expected Savings Maximized At 13.2%",
    x     = "Threshold (%)",
    y     = "Savings"
  )
  
  
# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

# 6.2 Sensitivity Analysis ----
