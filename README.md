# Multiclass Customer Status Prediction 📊

This project implements a complete data science pipeline to predict customer status using various classification algorithms. It focuses on identifying key factors that influence customer retention and behavior.

## 🔍 Project Overview

*   **Goal**: Predict the "Status" of a customer based on demographic and behavioral data.
*   **Methodology**: Comprehensive cleaning, Exploratory Data Analysis (EDA), and Machine Learning modeling.
*   **Outcome**: Achieved high accuracy using Random Forest and Gradient Boosting Machine (GBM) models.

## 📈 Key Features

*   **Data Integration**: Merging multiple datasets to create a unified view of the customer.
*   **Feature Engineering**: Creating new variables to enhance model performance.
*   **Exploratory Data Analysis (EDA)**: Visualizing distributions and correlations using `ggplot2`.
*   **Model Comparison**: Evaluating Decision Trees, Random Forest, and GBM based on Accuracy, Precision, and Recall.

## 🛠️ Tech Stack

*   **Language**: R
*   **Libraries**: `tidyverse`, `caret`, `randomForest`, `gbm`, `ggplot2`
*   **Reporting**: R Markdown

## 📊 Results

The **Random Forest** model yielded the best performance, effectively handling class imbalances and non-linear relationships in the data. Detailed performance metrics and confusion matrices can be found in the generated HTML report.

## 🚀 How to Run

1.  Open `project_main.Rmd` in RStudio.
2.  Ensure you have the required packages installed:
    ```r
    install.packages(c("tidyverse", "caret", "randomForest"))
    ```
3.  Click **Knit** to generate the full analysis report.

## 📄 License

This project is open-source.
