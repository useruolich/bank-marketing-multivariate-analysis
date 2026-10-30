# Bank marketing: multivariate analysis and classification in R

Course project for *Applied Multivariate Statistics* at TU Dresden (2025). The goal is to understand the structure of a bank's telemarketing data and to predict whether a client subscribes to a term deposit.

## Data

UCI *Bank Marketing* dataset (Moro, Cortez and Rita, 2014), file `bank-full.csv` (45,211 records, 17 variables, semicolon-separated). Download it from the UCI Machine Learning Repository and place it in `data/`. The data file is not included.

## What the script does

`R/bank_marketing_analysis.R`, run from the repository root:

1. **Preparation.** The records are in chronological order, so a calendar date is reconstructed from the order of months (May 2008 to November 2010) and used to derive the weekday of each contact. The columns `contact` and `poutcome` (many unknown values) and observations with unknown `job` or `education` are removed.
2. **Descriptive statistics** and scatterplot matrices, for the original and for transformed numeric variables.
3. **Principal component analysis** and **maximum-likelihood factor analysis** (with and without varimax rotation).
4. **Classification** on a 90/10 train/test split (seed 123): decision tree, logistic regression (with month, with weekday, with both), linear and quadratic discriminant analysis, random forest. Models are compared by confusion matrices and ROC curves.

## Requirements

R with `dplyr`, `ggplot2`, `summarytools`, `factoextra`, `caret`, `corrplot`, `ggcorrplot`, `FactoMineR`, `plotly`, `gridExtra`, `tidyverse`, `MASS`, `GGally`, `rpart`, `rpart.plot`, `randomForest`, `pROC`, `patchwork`. The script installs missing packages itself.

## Status

Course project, kept as submitted apart from replacing a local path with a relative one. The script has not been re-run after this change.

## Reference

Moro, S., Cortez, P., Rita, P. (2014). A data-driven approach to predict the success of bank telemarketing. *Decision Support Systems*, 62, 22-31.
