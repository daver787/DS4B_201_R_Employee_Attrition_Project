# DATA PREPARATION ----
# Machine Readable ----


# Setup -----

#Libraries
library(readxl)
library(recipes)
library(tidyverse)
library(tidyquant)

# Data 
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline ----
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)


# Plot Faceted Histogram Function ----

data <- train_raw_tbl
plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette_light()[[3]],
                            color ="white", scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE)
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  
  if (fct_rev){
    data_factored <- data_factored %>%
      mutate(key =fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins= bins, fill = fill, color = color) +
    facet_wrap(~key, ncol = ncol, scale = scale) +
    theme_tq()
  
  return(g)
  
}

train_raw_tbl %>%
  select(Attrition, everything()) %>%
  plot_hist_facet(bins = 10, ncol = 5, fct_rev = TRUE)

# Data Preprocessing with Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())
  
recipe_obj

# 2. Transformations ----
skewed_features_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = TRUE) %>%
  arrange(desc(value)) %>%
  filter(value >=0.8) %>%
  filter(!key %in% c("JobLevel", "StockOptionLevel")  ) %>%
  pull(key) %>%
  as.character()
  
skewed_features_names

factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_features_names)%>%
  step_mutate_at(factor_names,fn = as.factor)

recipe_obj %>%
  prep()%>%
  bake(new_data = train_readable_tbl) %>%
  select(skewed_features_names) %>%
  plot_hist_facet()

# 3. Center and Scale ----

train_readable_tbl %>%
  select_if(is.numeric) %>%
  plot_hist_facet() #before transformation

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_features_names)%>%
  step_mutate_at(factor_names,fn = as.factor) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

prepared_recipe <- recipe_obj %>% prep()
recipe_obj$steps[[4]] #before prep
prepared_recipe$steps[[4]] #after prep

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select_if(is.numeric) %>%
  plot_hist_facet() # after transformation
# 4. Dummy Variables ----
recipe_obj %>% prep() %>% bake(new_data = train_readable_tbl) %>%
  select(contains("JobRole")) %>%
  plot_hist_facet() # before dummying variables


recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_features_names)%>%
  step_mutate_at(factor_names,fn = as.factor) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())
  

recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(contains("JobRole")) %>%
  plot_hist_facet(ncol = 3) #after dummying the features



recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(contains("JobRole")) %>%
  glimpse()

# Final Recipe ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_features_names)%>%
  step_mutate_at(factor_names,fn = as.factor) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep() 
  #filter(number == step_no) call tidy on recipe_obj$steps[[step_no]]

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

train_tbl %>% glimpse()
test_tbl %>% glimpse()

# Correlation Analysis ----

data <- train_tbl
feature_expr<- quo(Attrition)


get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE){
  
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.factor, as.numeric) %>%
      cor(use = use) %>%
      as_tibble() %>%
      mutate(feature = names(.)) %>%
      select(feature, !!feature_expr) %>%
      filter(!(feature == feature_name)) %>%
      mutate_if(is.character, as_factor)
    
    if (fct_reorder){
      data_cor <- data_cor %>%
        mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
        arrange(feature)
    }
    
    if (fct_rev){
      data_cor <- data_cor %>%
        mutate(feature = fct_rev(feature)) %>%
        arrange(feature)
    }
    
    return(data_cor)
  
}


train_tbl %>% get_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)


data <- train_tbl

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]){
  
  feature_expr <- enquo(target)
  
  data_cor <- data %>%
    get_cor(!!feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) > 0 ~ "Positive",
      TRUE                  ~ "Negative"
    ) %>% as.factor())
  
  g <- data_cor %>%
      ggplot(aes(x = !!feature_expr, y = feature)) +
      geom_point(aes(color = Correlation), size = size) +
      geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
      geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
      expand_limits(x = c(-1,1)) +
      theme_tq() +
      scale_color_manual(values = c(color_neg, color_pos))
  
  if(include_lbl){
    g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
  }
    
    return(g)
}

train_tbl %>%
  select(Attrition, contains("JobRole")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

# Correlation Evaluation ----

#1. Descriptive Features: age, gender, marital status
train_tbl %>%
  select(Attrition, Age, contains("Gender"), contains("MaritalStatus"), NumCompaniesWorked, contains("Over18"), DistanceFromHome) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#2. Employment Features: department, job role, job level
train_tbl %>%
  select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel
train_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#4. Survey Results: Satisfaction level, WorkLifeBalance
train_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#5. Performance Data: Job Involvement, Performance Rating
train_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#6. Work-Life Features
train_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#7. Training and Education
train_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)

#8. Time- Based Features: Years at company, Years in current role   
train_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_cor(target = Attrition, fct_reorder = TRUE, fct_rev = TRUE)


