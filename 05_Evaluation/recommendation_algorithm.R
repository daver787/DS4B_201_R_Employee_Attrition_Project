# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or laters is installed. If not restart & install.packages("recipes") to update.


# Data
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



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----
train_readable_tbl %>% glimpse()

factor_names <- c("JobLevel", "StockOptionLevel")
 
# Recipe

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_discretize(all_numeric_predictors(),options = list(min_unique = 1 ) )%>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()


recipe_obj

train_corr_tbl <-  bake(recipe_obj, new_data = train_readable_tbl)

train_corr_tbl %>% glimpse()

tidy(recipe_obj)

tidy(recipe_obj, number = 3)

tidy(recipe_obj, number = 1)

# 2.2 Correlation Visualization ----

#Manipulate Data
train_corr_tbl %>% glimpse()


corr_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>%
  select(-Attrition_No) %>%
  get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE) %>%
  filter(abs(Attrition_Yes) >= corr_level ) %>%
  mutate(
    relationship = case_when(
      Attrition_Yes > 0 ~ "Supports",
      TRUE ~ "Contradicts"
    )
  ) %>%
  mutate(feature_text = as.character(feature)) %>%
  separate(feature_text, into = "feature_base",sep = "_", extra = "drop") %>%
  mutate(feature_base = as_factor(feature_base) %>% fct_rev())
  

length_unique_groups <- correlation_results_tbl %>%
  pull(feature_base) %>%
  unique() %>%
  length()

#Create Visualization
correlation_results_tbl %>%
  # mutate(level = as.numeric(feature_base))
  ggplot(aes(x = Attrition_Yes, y = feature_base, color = relationship)) +
  geom_point() +
  geom_label(aes(label = feature), vjust = -0.5) +
  expand_limits(x = c(-0.3, 0.3), y = c(1, length_unique_groups + 1)) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title    = "Correlation Analysis: Recommendation Strategy Development",
    subtitle = "Discretizing features to help identify a strategy"
  )

# 3.0 Recommendation Strategy Development Worksheet ----




# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----

#Years at Company
# YAC -High-Likely to Stay /YAC -Low-Likely to leave
# Tie promotion if low to advance faster/MEntor if YAC low

#TotalWorkingYears
# TWY - High -More likely to stay/TWY -Low -<ore likely to leave
# Tie low TWY to traning & formation/mentorship

#YearsInCurrentRole
# More time in current role related to lower attrition
# Incentivize specialize or promote / Mentorship role

#JobInvolvement
# High JI - Likely to stay/Low JI-Likely to leave
# Create personal development plan if low/ High Seek Leadership Role

#JobSatisfaction
# Low JS - More likely to leave/ High JS -More likely to stay
# Low: Create personal development plan/ High: Mentorship roles

#PerformanceRating
# Low Personal Development Plan/ High Seek Leadership or Mentorship Roles


# Good,Better,Best Approach

# (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating

# (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears

# (Best Case 1) Seek Mentorship Roles: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction

# (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating


train_readable_tbl %>%
  select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole,
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    personal_development_strategy =case_when(
      # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating
      PerformanceRating == 1 |
        JobSatisfaction == 1 |
        JobInvolvement <= 2                             ~ "Create Personal Development Plan",
      
      # (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears
      YearsAtCompany < 3 |
        TotalWorkingYears <6                            ~ "Promote Training and Formation", 
      
      # (Best Case 1) Seek Mentorship Roles: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
      (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
        PerformanceRating >= 3 & JobSatisfaction == 4   ~ "Seek Mentorship Role",
      
      # (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
      JobInvolvement >= 3 & 
        JobSatisfaction >= 3 &
        PerformanceRating >= 3                         ~ "Seek Leadership Role",
     
       #Catch All
      TRUE                                             ~ "Retain and Mantain"
    )
  ) 


train_readable_tbl %>%
  pull(PerformanceRating) %>%
  levels() 

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms,"TotalWorking"))

# 4.2 Professional Development (Promotion Readiness) ----

#JobLevel
# Employees with Job Level 1 are leaving / Job Level 2 staying
# Promote faster for high performers

# Years at Company
# YAC - High- Likely to stay/ YAC - Low - Likely to leave
# Tie promotion if low to advance faster / Mentor if YAC low


# YearsInCurrentRole
# More time in current role related to lower attrition
# Incentivize specialize or promote

#Addtional Features
# JobInvolvement - Important for promotion readiness, incentivizes involvement for leaders
#and early promotion
# JobSatisfaction - Important for specialization, incentivizes satisfaction for mentors
# PerformanceRating - Important for any promotion

# Good Better Best Approach

#Ready for Rotation: YearsInCurrentRole, JobSatisfaction(Low)

#Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating

train_readable_tbl %>%
  select(JobLevel, YearsInCurrentRole,
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    professional_development_strategy =case_when(
      # Ready for Rotation: YearsInCurrentRole, JobSatisfaction(Low)
        YearsInCurrentRole >= 2 &
          JobSatisfaction <= 2      ~ "Ready for Rotation",
      # Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 1 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 3 &
          PerformanceRating >= 3   ~ "Ready for Promotion",
      
      # Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 2 &
        YearsInCurrentRole >= 2 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3   ~ "Ready for Promotion",
      
      #Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 3 &
        YearsInCurrentRole >= 3 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3   ~ "Ready for Promotion",
      
      #Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 4 &
        YearsInCurrentRole >= 4 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3   ~ "Ready for Promotion",
      #Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
      YearsInCurrentRole >= 4 &
        JobSatisfaction >= 4 &
        PerformanceRating >= 3   ~"Incentivize Specialization" ,
        
      # Catch all
      
      TRUE                                             ~ "Retain and Mantain"
    )
  )

tidy(recipe_obj, number = 3) %>% 
  filter(str_detect(terms,"Distance"))




# 4.3 Work Environment Strategy ----

# OverTime - Employees that are working OverTime are more likely to leave/Employees not working OverTime more likely to stay
#Reduce OverTime where possible- work life balance

# EnvironmentSatisfaction -Employees with low environment satisfaction more likely to leave
# Improve workplace environment - review job assignment after period of time in current role

# WorkLifeBalance- Employees with bad work life balance more likely to leave
# Improve worklife balance

# BusinessTravel - Employees with more business travel more likely to leave/Employees with less business travel more likely to stay
# Reduce business travel where possible

# DistanceFromHome- Employees that live far from workplace more likely to leave
# Monitor worklife balance

#Addtional Features
# YearsInCurrentRole - Important for reviewing a job assignment is to give sufficient time in a role(min 2 years)
# JobInvolvement - Not included , but important in keeping work environment satisfaction (target Medium and Low)
  
# Good, Better, Best Approach
# Improve Work-Life Balance: OverTime, WorkLifeBalance
# Monitor BusinessTravel: BusinessTravel, DistanceFromHome, WorkLifeBalance
# Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
# JobInvolvement  


#Implement Strategy into Code

  train_readable_tbl %>%
    select(OverTime, EnvironmentSatisfaction,WorkLifeBalance, BusinessTravel, DistanceFromHome,
           YearsInCurrentRole, JobInvolvement) %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate(
      work_environment_strategy =case_when(
        # Improve Work-Life Balance: OverTime, WorkLifeBalance
        OverTime == 2 |
          WorkLifeBalance == 1     ~ "Improve Work-Life Balance",
        # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
       (BusinessTravel == 3 |
          DistanceFromHome >= 10) &
          WorkLifeBalance == 2     ~ "Monitor Business Travel",
        
        # Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
        EnvironmentSatisfaction == 1 &
          YearsInCurrentRole >= 2  ~ "Review Job Assignment",
        
        # Promote Job Engagement: JobInvolvement
        JobInvolvement <= 2        ~ "Promote Job Engagement",
        
        # Catch all
        TRUE                       ~ "Retain and Mantain"
      )
    ) %>% count(work_environment_strategy)

# 5.0 Recommendation Function

data <- train_readable_tbl

data %>%
  select(EmployeeNumber)

employee_number <- 19

recommend_strategies <- function(data,employee_number) {
  data %>%
    filter(EmployeeNumber == employee_number) %>%
    mutate_if(is.factor, as.numeric) %>%
    # Personal Development Strategy
    mutate(
      personal_development_strategy =case_when(
        # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating
        PerformanceRating == 1 |
          JobSatisfaction == 1 |
          JobInvolvement <= 2                             ~ "Create Personal Development Plan",
        
        # (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears
        YearsAtCompany < 3 |
          TotalWorkingYears <6                            ~ "Promote Training and Formation", 
        
        # (Best Case 1) Seek Mentorship Roles: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
        (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
          PerformanceRating >= 3 & JobSatisfaction == 4   ~ "Seek Mentorship Role",
        
        # (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
        JobInvolvement >= 3 & 
          JobSatisfaction >= 3 &
          PerformanceRating >= 3                         ~ "Seek Leadership Role",
        
        #Catch All
        TRUE                                             ~ "Retain and Mantain"
      )
    ) %>%
    #select(EmployeeNumber, personal_development_strategy)
  #Professional Development Strategy
    mutate(
      professional_development_strategy =case_when(
        # Ready for Rotation: YearsInCurrentRole, JobSatisfaction(Low)
        YearsInCurrentRole >= 2 &
          JobSatisfaction <= 2      ~ "Ready for Rotation",
        # Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 1 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 3 &
          PerformanceRating >= 3   ~ "Ready for Promotion",
        
        # Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 2 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3   ~ "Ready for Promotion",
        
        #Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 3 &
          YearsInCurrentRole >= 3 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3   ~ "Ready for Promotion",
        
        #Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 4 &
          YearsInCurrentRole >= 4 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3   ~ "Ready for Promotion",
        #Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
        YearsInCurrentRole >= 4 &
          JobSatisfaction >= 4 &
          PerformanceRating >= 3   ~"Incentivize Specialization" ,
        
        # Catch all
        
        TRUE                                             ~ "Retain and Mantain"
      )
    ) %>%
    #select(EmployeeNumber, professional_development_strategy)
  # WorkEnvironment Strategy
    mutate(
      work_environment_strategy =case_when(
        # Improve Work-Life Balance: OverTime, WorkLifeBalance
        OverTime == 2 |
          WorkLifeBalance == 1     ~ "Improve Work-Life Balance",
        # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
        (BusinessTravel == 3 |
           DistanceFromHome >= 10) &
          WorkLifeBalance == 2     ~ "Monitor Business Travel",
        
        # Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
        EnvironmentSatisfaction == 1 &
          YearsInCurrentRole >= 2  ~ "Review Job Assignment",
        
        # Promote Job Engagement: JobInvolvement
        JobInvolvement <= 2        ~ "Promote Job Engagement",
        
        # Catch all
        TRUE                       ~ "Retain and Mantain"
      )
    ) %>%
    select(EmployeeNumber,personal_development_strategy, professional_development_strategy, work_environment_strategy)

}

# Test the Function

train_readable_tbl %>%
  recommend_strategies(1)

test_readable_tbl %>%
  recommend_strategies(228)
