# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 4: H2O MODELING ----


library(h2o)
library(glue)
library(cowplot)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
    
    model_name <- h2o_leaderboard %>%
        as.tibble() %>%
        slice(n) %>%
        pull(model_id)
    
    if (verbose) message(model_name)
    
    return(model_name)
    
}


# Visualize the H2O leaderboard to help with model selection
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
    
    # Setup inputs
    order_by <- tolower(order_by[[1]])
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as.tibble() %>%
        mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
        rownames_to_column(var = "rowname") %>%
        mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
    
    # Transformation
    if (order_by == "auc") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(auc),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value, 
                   -c(model_id, model_type, rowname), factor_key = T)
        
    } else if (order_by == "logloss") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
        
    } else {
        stop(paste0("order_by = '", order_by, "' is not a permitted option."))
    }
    
    # Visualization
    g <- data_transformed_tbl %>%
        ggplot(aes(value, model_id, color = model_type)) +
        geom_point(size = size) +
        facet_wrap(~ key, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Leaderboard Metrics",
             subtitle = paste0("Ordered by: ", toupper(order_by)),
             y = "Model Postion, Model ID", x = "")
    
    if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
    
    return(g)
    
}


# Convert a leaderboard into a Performance Diagnostic Dashboard
# containing an ROC Plot, Precision vs Recall Plot, Gain Plot, and Lift Plot
plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
    
    # Inputs
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as.tibble() %>%
        slice(1:max_models)
    
    newdata_tbl <- newdata %>%
        as.tibble()
    
    order_by <- tolower(order_by[[1]])
    order_by_expr <- rlang::sym(order_by)
    
    h2o.no_progress()
    
    # 1. Model metrics
    
    get_model_performance_metrics <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
        
        perf_h2o %>%
            h2o.metric() %>%
            as.tibble() %>%
            select(threshold, tpr, fpr, precision, recall)
        
    }
    
    model_metrics_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
        unnest() %>%
        mutate(
            model_id = as_factor(model_id) %>% 
                fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
            auc  = auc %>% 
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
        ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        theme_tq() +
        scale_color_tq() +
        labs(title = "ROC", x = "FPR", y = "TPR") +
        theme(legend.direction = "vertical")
    
    # 1B. Precision vs Recall
    
    p2 <- model_metrics_tbl %>%
        ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
        theme(legend.position = "none")
    
    
    # 2. Gain / Lift
    
    get_gain_lift <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
        
        perf_h2o %>%
            h2o.gainsLift() %>%
            as.tibble() %>%
            select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
        
    }
    
    gain_lift_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
        unnest() %>%
        mutate(
            model_id = as_factor(model_id) %>% 
                fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
            auc  = auc %>% 
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
    
    # 2A. Gain Plot
    
    p3 <- gain_lift_tbl %>%
        ggplot(aes_string("cumulative_data_fraction", "gain", 
                          color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                     color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0, 1), y = c(0, 1)) +
        labs(title = "Gain",
             x = "Cumulative Data Fraction", y = "Gain") +
        theme(legend.position = "none")
    
    # 2B. Lift Plot
    
    p4 <- gain_lift_tbl %>%
        ggplot(aes_string("cumulative_data_fraction", "lift", 
                          color = "model_id", linetype = order_by)) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                     color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0, 1), y = c(0, 1)) +
        labs(title = "Lift",
             x = "Cumulative Data Fraction", y = "Lift") +
        theme(legend.position = "none")
    
    
    # Combine using cowplot
    p_legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    
    p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
    
    p_title <- ggdraw() + 
        draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
                   colour = palette_light()[[1]])
    
    p_subtitle <- ggdraw() + 
        draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
                   colour = palette_light()[[1]])
    
    ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                     ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
    
    h2o.show_progress()
    
    return(ret)
    
}