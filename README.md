# Formula 1 World Championship Data Analysis and Modeling (1950-2020)

This project focuses on exploratory data analysis and predictive modeling using the Formula 1 World Championship dataset, spanning from 1950 to 2020. The goal is to analyze race results and predict outcomes such as podium finishes using historical data.

## Project Overview

- Data loading and integration from multiple CSV files covering races, results, drivers, constructors and circuits.
- Data preprocessing including cleaning, handling missing data and feature engineering.
- Creation of new target variable `podium_finish` indicating whether a driver finished in the top 3 positions.
- Exploratory Data Analysis (EDA) with visualizations of podium distributions, driver and constructor statistics and race performance trends.
- Feature selection and data preparation including scaling and class balancing.
- Implementation and comparison of classification models:
  - Logistic Regression,
  - K-Nearest Neighbors (KNN) with cross-validation,
  - Random Forest with hyperparameter tuning.
- Evaluation of model performance with accuracy, precision, recall, F1-score, ROC curves and confusion matrices.
- Variable importance analysis for the Random Forest model.

## Dataset

The dataset is obtained from Kaggle:  
> **Formula 1 World Championship 1950-2020**  
> [https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020)

The data includes detailed historical records of Formula 1 races, drivers, constructors, circuits, race results, qualifying and more, making it ideal for deep analysis of performance trends and predictive modeling.

## Libraries and Tools

- R packages: `tidyverse`, `tidymodels`, `caret`, `pheatmap`, `vip`, `pROC`, `gridExtra`, `naniar`, `MLmetrics`, `reshape2`  
- Data processing and visualization tools for EDA and model evaluation.

## Results Summary

| Model                 | Accuracy | Precision | Recall | F1 Score |
|-----------------------|----------|-----------|--------|----------|
| Logistic Regression   | (to be filled)  | (to be filled)   | (to be filled) | (to be filled)  |
| KNN (optimal k)       | (to be filled)  | (to be filled)   | (to be filled) | (to be filled)  |
| Random Forest         | (to be filled)  | (to be filled)   | (to be filled) | (to be filled)  |
