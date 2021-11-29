# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 3: DATA PREPARATION ----
# data_processing_pipeline.R ----


library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

# Processing pipeline for the HR Data
process_hr_data_readable <- function(data, definitions_tbl) {
    
    definitions_list <- definitions_tbl %>%
        fill(...1, .direction = "down") %>%
        filter(!is.na(...2)) %>%
        separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
        rename(column_name = ...1) %>%
        mutate(key = as.numeric(key)) %>%
        mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
        split(.$column_name) %>%
        map(~ select(., -column_name)) %>%
        map(~ mutate(., value = as_factor(value))) 
    
    
    for (i in seq_along(definitions_list)) {
        list_name <- names(definitions_list)[i]
        colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
    }
    
    data_merged_tbl <- list(HR_Data = data) %>%
        append(definitions_list, after = 1) %>%
        reduce(left_join) %>%
        select(-one_of(names(definitions_list))) %>%
        set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
        select(sort(names(.))) %>%
        mutate_if(is.character, as.factor) %>%
        mutate(
            BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
            MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
        )
    
    return(data_merged_tbl)
    
}