# DATA PREPARATION ----
# Human Readable ----

library(readxl)
library(tidyverse)
library(tidyquant)

path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Tidying the Data ----

train_raw_tbl %>%
  glimpse()
definitions_raw_tbl %>% View()

definitions_tbl <- definitions_raw_tbl %>%
  fill(...1, .direction = "down") %>%
  filter(!is.na(...2)) %>%
  separate(...2,into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = ...1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = ""))

definitions_tbl

definitions_list <- definitions_tbl %>%
  split(.$column_name) %>%
  map(~select(.,-column_name)) %>%
  map(~mutate(., value = as_factor(value)))

definitions_list
definitions_list[[1]]
definitions_list[["Education"]]

for (i in seq_along(definitions_list)){
  
  list_name <- names(definitions_list)[i]
  
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name,"_value"))
  
}

data_merged_tbl <- list(HR_Data = train_raw_tbl) %>%
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  select(-any_of(names(definitions_list))) %>%
  set_names(str_replace_all(names(.), pattern = "_value", "")) %>%
  select(sort(names(.)))

data_merged_tbl %>%
  glimpse()

data_processed_tbl <- data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel","Travel_Rarely", "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
  )

data_processed_tbl %>%
  select_if(is.factor) %>%
  map(levels)


# Processing Pipeline ----

processed_hr_data_readable <- function(data, definitions_tbl){
  
  definitions_list <- definitions_tbl  %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2,into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~select(.,-column_name)) %>%
    map(~mutate(., value = as_factor(value)))
  
  for (i in seq_along(definitions_list)){
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name,"_value"))
    
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-any_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel","Travel_Rarely", "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
    )
  
 return(data_merged_tbl)
  
}


train_raw_tbl %>% processed_hr_data_readable(definitions_raw_tbl) %>%
  glimpse()
 
