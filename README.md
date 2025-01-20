# DS4B_201_R_Employee_Attrition_Project

## Overview
This project is part of the DS4B 201 class by Matt Dancho. It focuses on analyzing and predicting employee attrition using various data science techniques in R. The goal is to provide insights into factors contributing to employee turnover and to build predictive models for attrition.

## Technologies Used
- **Programming Languages & Libraries:**
  - **R:** Core programming language for data analysis and modeling.
  - **tidyverse:** A collection of R packages for data manipulation and visualization.
  - **ggplot2:** For creating visualizations.
  - **caret:** For building and evaluating predictive models.
  - **randomForest:** For implementing random forest models.
  - **xgboost:** For implementing gradient boosting models.
  - **Shiny:** For building interactive web applications.
  
- **Visualization Tools:**
  - **ggplot2:** To create detailed and informative visualizations.

## Summary of Work Done
- **Data Collection:** Gathered and pre-processed employee data for analysis.
- **Exploratory Data Analysis (EDA):** Conducted EDA to understand the data and identify patterns.
- **Modeling:** Built predictive models, including logistic regression, decision trees, random forests, and gradient boosting models.
- **Model Evaluation:** Evaluated model performance using metrics such as accuracy, precision, recall, and ROC-AUC.
- **Reporting:** Created reports and visualizations to present findings.
- **Interactive Applications:** Developed Shiny applications for interactive data exploration.

## Outcome
The project demonstrates the application of data science techniques to analyze and predict employee attrition. It showcases the use of R for data manipulation, modeling, and visualization. The Shiny applications provide an accessible way for non-technical users to explore the analysis and gain insights.

## Repository Structure
- `data/`: Contains raw and processed data files.
- `scripts/`: Contains R scripts for data processing, modeling, and visualization.
- `reports/`: Contains generated reports and visualizations.
- `shiny_app/`: Contains the Shiny application for interactive data exploration.

## How to Use
1. **Clone the repository:**
   ```bash
   git clone https://github.com/daver787/DS4B_201_R_Employee_Attrition_Project.git
   cd DS4B_201_R_Employee_Attrition_Project
   ```
2. **Install required packages:**
   Ensure you have R installed, then use the following command to install necessary packages:
   ```R
   install.packages(c("tidyverse", "ggplot2", "caret", "randomForest", "xgboost", "shiny"))
   ```
3. **Run the scripts:**
   Execute the R scripts in the `scripts/` directory for data processing, modeling, and visualization.
4. **Launch the Shiny app:**
   Navigate to the `shiny_app/` directory and run the Shiny application:
   ```R
   shiny::runApp('shiny_app')
   ```

## Contributing
Contributions are welcome! Please fork the repository and create a pull request with your changes.

## License
This project is licensed under the MIT License.

[Explore the project](https://github.com/daver787/DS4B_201_R_Employee_Attrition_Project)
