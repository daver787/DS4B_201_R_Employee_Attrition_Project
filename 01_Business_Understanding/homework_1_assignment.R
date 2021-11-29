# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----
dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, Revenue_Average) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  plot_attrition(Department, JobRole, .value = cost_of_attrition, units ="M")# Job Role with highest attrition is Sales: Sales Executive

 #Alternative solution is to join after the aggregation has been done

dept_jobrole_tbl %>%
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  plot_attrition(Department, JobRole, .value = cost_of_attrition, units ="M")# Job Role with highest attrition is Sales: Sales Executive



# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----
#Cost of attrition is $2,276,492

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----

dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, Revenue_Average) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  arrange(desc(cost_of_attrition)) %>%
  mutate(cum_revenue_lost = cumsum(cost_of_attrition)) %>%
  mutate(cum_revenue_lost_pct = cum_revenue_lost/sum(cost_of_attrition)) %>%
  select(Department, JobRole, n, cum_revenue_lost, cum_revenue_lost_pct) %>%
  slice(1:4) %>%

#Solution from Matt

dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, Revenue_Average) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  arrange(desc(cost_of_attrition)) %>%
  mutate(row_num = row_number()) %>%
  mutate(is_top_4 = case_when(
    row_num <= 4 ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  group_by(is_top_4) %>%
  summarize(
    total_attrition_cost = sum(cost_of_attrition)
  ) %>%
  ungroup() %>%
  mutate(total_attrition_pct = total_attrition_cost / sum(total_attrition_cost))


# Q4. Which Department has the highest total cost of attrition? ----

dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, Revenue_Average) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  group_by(Department) %>%
  summarize(department_attrition_cost = sum(cost_of_attrition)) %>%
  ungroup() %>%
  arrange(desc(department_attrition_cost)) %>%
  slice(1)

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----

dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department", "JobRole" = "JobRole")) %>%
  count(Department, JobRole, Attrition, Salary_Average, Revenue_Average) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition,"Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = Salary_Average, net_revenue_per_employee = Revenue_Average)) %>%
  group_by(Department) %>%
  summarize(department_attrition_cost = sum(cost_of_attrition)) %>%
  arrange(desc(department_attrition_cost)) %>%
  mutate(cum_revenue_lost = cumsum(department_attrition_cost)) %>%
  mutate(cum_revenue_lost_pct = cum_revenue_lost/sum(department_attrition_cost))
  
