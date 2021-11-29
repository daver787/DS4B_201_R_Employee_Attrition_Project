# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 3: DATA PREPARATION ----
# plot_hist_facet.R ----

library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
    
    data_factored <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        gather(key = key, value = value, factor_key = TRUE) 
    
    if (fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}
