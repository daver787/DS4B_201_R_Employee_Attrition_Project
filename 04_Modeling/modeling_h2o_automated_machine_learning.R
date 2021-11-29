#H2O MODELING ----

#1. Setup ----

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
  step_num2factor(StockOptionLevel, levels = c('0','1','2','3'), transform = function(x) x+1) %>%
  prep()
  
train_tbl <- bake(recipe_obj,new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj,new_data = test_readable_tbl)

#2. Modeling ----

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl),ratios = c(0.85), seed = 1234 )
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o),y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5,
  keep_cross_validation_models = TRUE
)

typeof(automl_models_h2o)
slotNames(automl_models_h2o)

automl_models_h2o@leaderboard
automl_models_h2o@leader

h2o.getModel( "GLM_1_AutoML_1_20211111_103409")
h2o.getModel("DeepLearning_grid_1_AutoML_1_20211111_103409_model_1")
h2o.getModel("StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409")



automl_models_h2o@leaderboard %>%
  as_tibble() %>%
  slice(3) %>%
  pull(model_id) %>%
  
  h2o.getModel()

extract_h2o_model_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE){
  
 model_name <-  h2o_leaderboard %>%
    as_tibble() %>%
    slice(n) %>%
    pull(model_id)
 if(verbose){
   message(model_name)
 }
 
 return(model_name)
  
}

automl_models_h2o@leaderboard %>%
  extract_h2o_model_by_position(2) %>%
  h2o.getModel()

h2o.getModel("DeepLearning_grid_1_AutoML_1_20211111_103409_model_1") %>%
h2o.saveModel(path = "04_Modeling/h2o_models/" )

h2o.getModel( "GLM_1_AutoML_1_20211111_103409") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/" )

h2o.getModel("StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/" )

h2o.loadModel(path="04_Modeling/h2o_models/DeepLearning_grid_1_AutoML_1_20211111_103409_model_1")
  
# Making predictions

stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409")

stacked_ensemble_h2o

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_grid_1_AutoML_1_20211111_103409_model_1")
deeplearning_h2o
deeplearning_h2o@allparameters
h2o.cross_validation_models(deeplearning_h2o)
h2o.auc(deeplearning_h2o, train = T, valid = T, xval = T)


test_tbl
predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))
typeof(predictions)

predictions_tbl <- predictions %>% as_tibble()


# 3. Visualizing The Leaderboard ----

data_transformed_tbl <- automl_models_h2o@leaderboard %>%
  as_tibble() %>%
  mutate(model_type = str_split(model_id,"_", simplify =TRUE)[,1]) %>%
  slice(1:10) %>%
  rownames_to_column() %>%
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>%
  pivot_longer( cols = -c(model_id, model_type, rowname), names_to ="key" , values_to = "value") %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev(),
         key = as_factor(key))

data_transformed_tbl %>%
 filter(!(key %in% c('aucpr','mse', 'rmse','mean_per_class_error'))) %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2)), hjust = "inward") +
  facet_wrap(~key, scales = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(
    title    = "H2O Leaderboard Metrics",
    subtitle = paste0("Ordered by: auc"),
    y        = "Model Position, Model ID",
    x        = ""
  )
  

h2o_leaderboard <- automl_models_h2o@leaderboard  
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                 n_max = 20, size = 4, include_lbl = TRUE){
  
  order_by <- tolower(order_by[1])
  
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    mutate(model_type = str_split(model_id,"_", simplify =TRUE)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", model_id) %>% as.character(model_id) %>% as.factor())
    
  
  #Transformation
  if(order_by == "auc"){
  data_transformed_tbl <-  leaderboard_tbl %>%
    slice(1:n_max) %>%
    mutate(
      model_id   = as_factor(model_id) %>% reorder(auc),
      model_type = as.factor(model_type)
    ) %>%
    pivot_longer( cols = -c(model_id, model_type, rowname), names_to ="key" , values_to = "value") %>%
    mutate(key = as_factor(key))
  } else if(order_by == "logloss"){
    data_transformed_tbl <-  leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      pivot_longer( cols = -c(model_id, model_type, rowname), names_to ="key" , values_to = "value") %>%
      mutate(key = as_factor(key))
  } else{
    stop(paste0("order_by = ", order_by,"'is not a permitted option.'"))
  }
  
  g <- data_transformed_tbl %>%
    filter(!(key %in% c('aucpr','mse', 'rmse','mean_per_class_error'))) %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(
      title    = "H2O Leaderboard Metrics",
      subtitle = paste0("Ordered by: ", toupper(order_by)),
      y        = "Model Position, Model ID",
      x        = ""
    )
  
  if(include_lbl){
    g <- g +geom_label(aes(label = round(value, 2)), hjust = "inward")
  }
  
  return(g)

}

automl_models_h2o@leaderboard %>%
  plot_h2o_leaderboard(order_by = "logloss")


#4. Assessing Performance ----
h2o.init()
deeplearning_h2o <- h2o.loadModel(path ="04_Modeling/h2o_models/DeepLearning_grid_1_AutoML_1_20211111_103409_model_1")
stacked_ensemble_h2o <- h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_2_AutoML_1_20211111_103409")
glm_h2o <- h2o.loadModel(path = "04_Modeling/h2o_models/GLM_1_AutoML_1_20211111_103409")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o)
performance_h2o %>%
  slotNames()
performance_h2o@metrics

# Classifier Summary Metrics

h2o.auc(performance_h2o)
h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)
h2o.giniCoef(performance_h2o)
h2o.logloss(performance_h2o)

h2o.confusionMatrix(stacked_ensemble_h2o)
h2o.confusionMatrix(performance_h2o)

# Precision vs. Recall graph
performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as_tibble()

performance_tbl
--Precision =TP/(TP+FP)
--Recall = TP/(FN+TP)
--F1 =2*(precision*recall)/(precision+recall)

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  theme_tq() +
  labs(
    title = "Precision vs. Recall",
    y     = "value"
  )


# ROC Plot
#path <-  "04_Modeling/h2o_models/DeepLearning_grid_1_AutoML_1_20211111_103409_model_1"

load_model_performance_metrics <- function(path, test_tbl){
  model_h2o <- h2o.loadModel(path = path)
  perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()


model_metrics_tbl %>%
  mutate(path = str_split(path, '/',simplify = TRUE)[,3] %>% as_factor()) %>%
  mutate(auc = auc %>% round(3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(
    title    ="ROC Plot",
    subtitle = "Performance of 3 Top Performing Model"
  )

# Precision vs Recall

model_metrics_tbl %>%
  mutate(path = str_split(path, '/',simplify = TRUE)[,3] %>% as_factor()) %>%
  mutate(auc = auc %>% round(3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(
    title    ="Precison vs Recall Plot",
    subtitle = "Performance of 3 Top Performing Model"
  )

#Gain & Lift

ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes,Attrition) %>%
  arrange(desc(Yes))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarize(
    cases     = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  mutate( group = row_number()) %>%
  select(group, cases, responses) %>%
  mutate(
    cumulative_responses        = cumsum(responses),
    percentage_responses        = responses / sum(responses),
    gain                        = cumsum(percentage_responses),
    cumulative_percentage_cases = cumsum(cases)/ sum(cases),
    lift                        = gain /cumulative_percentage_cases,
    gain_baseline               = cumulative_percentage_cases,
    lift_baseline               = gain_baseline / cumulative_percentage_cases
    
  ) 

calculated_gain_lift_tbl %>%
  glimpse()

gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as_tibble()

gain_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain = cumulative_capture_rate) %>%
  gather(key = key, value = value, gain, baseline)


gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Gain Chart",
    x = "Cummulative Data Fraction",
    y = "Gain"
  )


lift_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  gather(key = key, value = value, lift, baseline)

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Lift Chart",
    x = "Cummulative Data Fraction",
    y = "Lift"
  )

# 5. Performance Evaluation ----

# data to use when building functions
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by = "auc"
max_models <- 4
size <- 1.5


plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  #Inputs
  
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as_tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress()
  
  # 1. Model Metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl){
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as_tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest(metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      
      auc = auc %>%
        round(3) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id)),
      
      logloss = logloss %>%
        round(4) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id))
        
    )
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes(fpr, tpr, color = model_id, linetype = !!order_by_expr)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical")
  
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes(recall, precision, color = model_id, linetype = !!order_by_expr)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Precision vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position= "none")
  
  #2. Gain / Lift
  get_gain_lift <- function(model_id, test_tbl){
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  
    perf_h2o %>%
      h2o.gainsLift() %>%
      as_tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
    }
  
    gain_lift_tbl <- leaderboard_tbl %>%
      mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
      unnest(metrics) %>%
      mutate(model_id = as_factor(model_id) %>%
               fct_reorder(!! order_by_expr, .desc = ifelse(order_by =="auc", TRUE, FALSE)),
             auc = auc %>%
               round(3) %>%
               as.character() %>%
               as_factor() %>%
               fct_reorder(as.numeric(model_id)),
             logloss = logloss %>%
               round(4) %>%
               as.character() %>%
               as_factor() %>%
               fct_reorder(as.numeric(model_id))
             
             ) %>%
      rename(
        gain = cumulative_capture_rate,
        lift = cumulative_lift
      )
    
 
    # 2A. Gain PLot
    
    p3 <- gain_lift_tbl %>%
      ggplot(aes(cumulative_data_fraction, gain, color = model_id, linetype = !! order_by_expr )) +
      geom_line(size = size) +
      geom_segment(x = 0, y = 0, xend = 1, yend = 1,
                   color = "black", size = size) +
      theme_tq() +
      scale_color_tq() +
      expand_limits(x = c(0,1), y = c(0,1)) +
      labs(
        title = "Gain",
        x     = "Cumulative Data Fraction",
        y     = "Gain"
      ) +
      theme(legend.position = "none")
      
    p4 <-  gain_lift_tbl %>%
      ggplot(aes(cumulative_data_fraction, lift, color = model_id, linetype = !! order_by_expr )) +
      geom_line(size = size) +
      geom_segment(x = 0, y = 1, xend = 1, yend = 1,
                   color = "black", size = size) +
      theme_tq() +
      scale_color_tq() +
      expand_limits(x = c(0,1), y = c(0,1)) +
      labs(
        title = "Lift",
        x     = "Cumulative Data Fraction",
        y     = "Lift"
      ) +
      theme(legend.position = "none")
      
    #Combine using cowplot
    p_legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    
    p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
    
    p_title <- ggdraw() +
      draw_label("H2O Model Metrics", size = 18, fontface = "bold",
                 colour = palette_light()[[1]])
    
    p_subtitle <- ggdraw() +
      draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,
                 colour = palette_light()[[1]])
    
    ret <- plot_grid(p_title, p_subtitle, p, p_legend, ncol = 1, rel_heights = c(0.05,0.05,1,0.05*max_models))
    
    h2o.show_progress()
    
    return(ret)
     

} 

# Test the function
automl_models_h2o@leaderboard %>%
  plot_h2o_performance(newdata = test_tbl, order_by = "auc", max_models = 5)

