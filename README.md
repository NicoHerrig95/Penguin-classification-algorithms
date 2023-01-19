# Penguin classification using ML :penguin:  

## Within this project I compare the performance of different ML classification algorithms on the palmers penguin data set. 

## Overview

By now, following algorithms are compared:

-   **k-Nearest-Neighbours algorithm**

-   **Logistic Regression (logit model)**

-   **Random Forest classification algorithm**

Whats next?:

-   Adding more classification algorithms (i.e. XGBoost, Naive Bayes, SVM)
-   Analysis of algorithmic performance for different train/test splits

## Data

The analysed data set (`penguins`) contains measurements of different penguin species (Adélie, Chinstrap and Gentoo) in the Palmer Archipelago. The data comes in form of a 344x8 data frame and is part of the `palmerpenguins` library. For further information about the data, please inspect through `?penguins` after installing the `palmerpenguins` library in R.

## Code and Scripts

*The whole project is completely written in R. The R packages on which each script depends are stated at the beginning of each script.*

**functions.R**: This script contains several functions which need to be sourced for running both the data wrangling script and the main classification script.

**penguins_datawrangling.R**: The datawrangling script loads and manipulates the penguin data set and predicts the sex of penguins for several observations where this feature is latent with a kNN algorithm. The script generates the final data set **penguins_sex.csv** which contains 342 observations.

**penguins_species_classification.R**: The main script contains the classification algorithms as well as a basic explanatory data analysis to give an overview about the data set. The trainset / testset split in the script is the default 80/20. Further, the script provides a model assessment for each algorithm through the `confusionMatrix` function. 

## Results
The accuracy values of each algorithm can be found in the **results_accuracy.csv**. file.
