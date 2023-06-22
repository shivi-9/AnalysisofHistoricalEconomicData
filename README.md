# Analysis of Historical Economic Data

## Problem Statement
The goal of this project is to analyze a dataset of historical economic data and identify the variables that can be used to predict the output per capita (ypp). The dataset contains information on various economic indicators such as years of schooling, average age, labor force participation rate, and population for different countries and years. By examining the relationships between these variables and ypp, we aim to determine which factors have a significant impact on economic output.

## Dependencies
The following R packages are used in this project:
* tidyverse
* leaps
* modelr
  
Make sure these packages are installed in your R environment before running the code.

## Requirements
To replicate the analysis, you need the following:
* Dataset: Obtain the dataset "historical_econ.csv" from the source mentioned in the code (http://www.jerrydwyer.com/growth/index.html). Make sure the dataset is saved in the same directory as your R script.
* R Environment: Install R or RStudio, which provides an R environment to run the code.

## Installation/Usage
1. Install R or RStudio: Download and install R from the official website (https://www.r-project.org/) or RStudio from the official website (https://www.rstudio.com/products/rstudio/download/).
2. Install Required Packages: Open R or RStudio and install the required packages by running the following command:
```
install.packages(c("tidyverse", "leaps", "modelr"))
```
3. Prepare the Dataset: Obtain the "historical_econ.csv" dataset from the provided source and save it in your working directory.
4. Run the Analysis: Copy the code provided in the project and paste it into your R or RStudio environment. Make sure the working directory is set correctly to the location where the dataset is saved. Execute the code step by step or all at once to perform the analysis.
5. Interpret the Results: Once the code is executed, you will obtain visualizations and regression summaries that explore the relationships between variables and their predictive power for ypp. Analyze the results to understand the significance of each variable in predicting economic output per capita.

Note: The code includes data preprocessing, exploratory analysis, variable selection using forward and backward selection methods, and cross-validation for model evaluation.
