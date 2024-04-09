# Predicting Prenatal Depression with ML Models

This repository contains the code and analyses for the research project titled "Predicting prenatal depression and assessing model bias using machine learning models." The study leverages Electronic Medical Records (EMR) to predict perinatal depression (PND) in early pregnancy, focusing on racial/ethnic minority women. The goal is to understand the effectiveness of ML models in predicting PND and to assess the potential biases in these models, particularly against low-income minority women.

## Files in the Repository

### `DataPreprocessingCleaningUp.Rmd`

This R Markdown file is dedicated to the preprocessing of Electronic Medical Records (EMR) data. It involves cleaning, transforming, normalizing, and handling missing values to prepare the dataset for machine learning analysis.

### `EMR predict PND (ML pipeline).ipynb`

A Jupyter Notebook that encompasses the ML pipeline for predicting perinatal depression. It includes the training and evaluation of various machine learning models, performance metrics assessment, and visualization of the results. The notebook serves as a core component of the analysis, showcasing the predictive capabilities and the exploration of biases within the models.

### `proportion_test.R`

An R script focused on statistical testing of social demographic features, as outlined in Tables S6 and S7 of the manuscript. It provides a statistical foundation to the study, enabling a quantitative assessment of the relationships among the social demographic variables within the dataset.

## Study Highlights

- The research aims to address the prevalence of PND among racial/ethnic minority women, utilizing ML models on EMR data.
- The analysis reveals moderate success in predicting PND but highlights a performance bias against low-income minority women.
- SHAP values and various bias metrics are used to interpret the models and assess their fairness.

For more details on the methodology, results, and conclusions of the study, please refer to the manuscript.
