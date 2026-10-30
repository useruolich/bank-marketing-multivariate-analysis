# Install and load necessary packages
packages_to_install <- c(
  "dplyr",
  "ggplot2",
  "summarytools",
  "factoextra",
  "caret",
  "corrplot",
  "ggcorrplot",
  "FactoMineR",
  "plotly",
  "gridExtra",
  "tidyverse",
  "MASS",
  "GGally",
  "rpart",
  "rpart.plot",
  "randomForest",
  "pROC",
  "patchwork"
)
install_and_load <- function(package_list) {
  for (package_name in package_list) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      install.packages(package_name, dependencies = TRUE)
    }
    library(package_name, character.only = TRUE)
  }
}
install_and_load(packages_to_install)
# Expects bank-full.csv in the data/ folder (see README); run from the repository root
bank_full <- read.csv("data/bank-full.csv", sep = ";")

cat("Number of rows in bank_full:", nrow(bank_full), "\n")
cat("\nStructure of bank_full before column renaming:\n")
str(bank_full)
data_column <- c("age", "job", "marital", "education", "default",
                 "balance", "housing", "loan", "contact", "day",
                 "month", "duration", "campaign", "pdays", "previous",
                 "poutcome", "y")
colnames(bank_full) <- data_column
cat("\nStructure of bank_full after column renaming:\n")
str(bank_full)
cat("\nDimensions of bank_full:", dim(bank_full), "\n")
data <- bank_full
cat("\nHead of data after initial loading and renaming:\n")
head(data)
# Weekday Column Creation and Categorization
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
month_to_num <- c(
  "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
  "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12
)
data$month_num <- sapply(tolower(data$month), function(m) month_to_num[m])
data$year_assigned <- NA_integer_
current_year <- 2008
prev_month_num <- 0
for (i in 1:nrow(data)) {
  current_month_num <- data$month_num[i]
  
  if (i == 1) {
    data$year_assigned[i] <- current_year
  } else {
    if (current_month_num < prev_month_num) {
      current_year <- current_year + 1
    }
    data$year_assigned[i] <- current_year
  }
  prev_month_num <- current_month_num
}
data$full_date_assigned <- as.Date(
  paste(data$year_assigned, data$month_num, data$day, sep = "-"),
  format = "%Y-%m-%d"
)
start_filter_date <- as.Date("2008-05-01")
end_filter_date <- as.Date("2010-11-30")
data$full_date_assigned[data$full_date_assigned > end_filter_date | data$full_date_assigned < start_filter_date] <- NA_Date_
data$weekday <- format(data$full_date_assigned, "%A")
data$weekday <- factor(data$weekday, levels = weekday_levels, ordered = TRUE)
cat("\nStructure of 'weekday' column after re-factoring with ordered levels:\n")
str(data$weekday)
cat("\nFrequency table of 'weekday' column:\n")
print(table(data$weekday, useNA = "ifany"))
cat("\nNumber of NAs in full_date_assigned:", sum(is.na(data$full_date_assigned)), "\n")
cat("\nNumber of NAs in weekday:", sum(is.na(data$weekday)), "\n")
# Remove columns "contact" and "poutcome" due to a large number of unknown values
data <- data %>%
  dplyr::select(-contact, -poutcome)
cat("\nHead of data after removing 'contact' and 'poutcome':\n")
head(data)
cat("Dimensions of data after removing 'contact' and 'poutcome':", dim(data), "\n")
# Check for "unknown" and "other" values for "job" and "education" variables and remove observations with these values
data <- data %>% filter(job != "unknown", education != "unknown")
col_vec <- colnames(data)
cat("\nColumn names after filtering 'unknown' jobs and education:\n")
print(col_vec)
cat("Dimensions of data after filtering 'unknown' values:", dim(data), "\n")
data <- data %>% dplyr::select(-c(full_date_assigned))

df_raw_for_processing <- data %>%
  dplyr::select(
    "age", "balance", "duration", "campaign", "pdays", "previous",
    "default", "housing", "loan",
    "job", "marital", "education", "month",
    "weekday",
    "y"
  )
cat("\nDimensions of the pre-processed data frame (df_raw_for_processing):\n")
print(dim(df_raw_for_processing))
cat("\nHead of the pre-processed data frame (df_raw_for_processing):\n")
print(head(df_raw_for_processing))
cat("\nStructure of the pre-processed data frame (df_raw_for_processing):\n")
print(str(df_raw_for_processing))


################################################################################
# --- Descriptive Statistics ---
################################################################################

numeric_vars_for_desc_stats <- c("age", "balance", "duration", "campaign", "pdays", "previous")
cat("\nDescriptive Statistics for Selected Numeric Variables:\n")
print(descr(df_raw_for_processing %>% dplyr::select(all_of(numeric_vars_for_desc_stats))))

set.seed(123)
numeric_cols_for_plot <- c("age", "balance", "duration", "campaign", "pdays", "previous")
df_for_plot_original <- df_raw_for_processing %>%
  mutate(
    y = as.factor(y)
  ) %>%
  dplyr::select(y, all_of(numeric_cols_for_plot)) %>%
  filter(complete.cases(.))

set.seed(123)
df_samp_original <- sample_n(df_for_plot_original, 3500)

# --- Scatterplot Matrix 1: Original Numeric Variables ---
df_jit_original <- df_samp_original %>%
  mutate(across(all_of(numeric_cols_for_plot), ~ jitter(.x, factor = 1)))

plot_original <- ggpairs(
  df_jit_original,
  columns = c("y", numeric_cols_for_plot),
  mapping = aes(color = y),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1.2)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
  upper = list(continuous = "blank"),
  title = "Scatterplot Matrix (Original Numeric Variables)"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 10)
  ) +
  scale_color_discrete(name = "Term Deposit")

# --- Scatterplot Matrix 2: Custom Transformed Numeric Variables ---
df_custom_transformed <- df_samp_original
transform_cols_log10_custom <- c("age", "duration", "campaign")
df_custom_transformed <- df_custom_transformed %>%
  mutate(
    across(all_of(transform_cols_log10_custom), ~ log10(.x + 1))
  )

df_custom_transformed <- df_custom_transformed %>%
  mutate(
    balance = log(balance + 1)
  )

numeric_cols_to_plot_custom <- c("age", "duration", "campaign", "balance")
df_jit_custom <- df_custom_transformed %>%
  mutate(across(all_of(numeric_cols_to_plot_custom), ~ jitter(.x, factor = 1)))

plot_custom_transformed <- ggpairs(
  df_jit_custom,
  columns = c("y", numeric_cols_to_plot_custom),
  mapping = aes(color = y),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1.2)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
  upper = list(continuous = "blank"),
  title = "Scatterplot Matrix (Log10: Age, Duration, Campaign; loge: Balance)"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 10)
  ) +
  scale_color_discrete(name = "Term Deposit")

print(plot_original)
print(plot_custom_transformed)

# Basic data exploration
cat("\nBasic Data Exploration:\n")
head(data)
tail(data)
str(data)
summary(data)
dim(data)
glimpse(data)

#categorical
cat_vars <- c("job", "marital", "education", "default", "housing", "loan", "month", "weekday")

for (var in cat_vars) {
  cat("\n=================================================\n")
  cat(paste("Proportion of 'yes' for variable:", var, "\n"))
  cat("=================================================\n")
  
  prop_yes <- data %>%
    group_by_at(var) %>%
    summarise(
      Count = n(),
      Subscribed = sum(y == "yes"),
      Proportion_yes = round(mean(y == "yes") * 100, 2)
    ) %>%
    arrange(desc(Proportion_yes))
  
  print(prop_yes)
}

# Plot function
plot_yes_proportion <- function(var) {
  data %>%
    group_by_at(var) %>%
    summarise(Proportion_yes = mean(y == "yes")) %>%
    ggplot(aes_string(x = var, y = "Proportion_yes")) +
    geom_bar(stat = "identity", fill = "darkorange") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Proportion of 'Yes' by", var),
         y = "Proportion Subscribed",
         x = var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots
prop_yes_plots <- lapply(cat_vars, plot_yes_proportion)

# Show plots in two sets
gridExtra::grid.arrange(grobs = prop_yes_plots[1:4], ncol = 2, top = "Subscription Proportion by Category (Set 1)")
gridExtra::grid.arrange(grobs = prop_yes_plots[5:8], ncol = 2, top = "Subscription Proportion by Category (Set 2)")


################################################################################
# --- Principal Component Analysis (PCA) ---
################################################################################


dim(df_raw_for_processing)
selected_data = df_raw_for_processing[, c("age", "balance", "duration", "campaign", "pdays", "previous")]
normalized_data = scale(selected_data)
data.pca =  prcomp(normalized_data)
data.pca
summary(data.pca)
scree_plot <- fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 40),
                       xlab = "Principal Components", ylab = "Cumulative Percentage",
                       main = "Cumulative Percentage of PCA") +
  theme_classic()
print(scree_plot)
cum_plot <- plot(cumsum(data.pca$sdev^2)/sum(data.pca$sdev^2),
                 type = 'b',
                 cex=.75,
                 xlab = "Number of components",
                 ylab = "% variance explained",
                 main = "Cumulative Percentage of PCA")
abline(h = 0.91,col = "red",lty = 2)
abline(v = 5,col = "blue", lty = 2)

# Plot for PC2 vs PC3
p1 <- fviz_pca_var(data.pca, axes = c(1, 2), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), label = "var", repel = TRUE) +
  ggtitle("PC1 vs PC2")
# Plot for PC1 vs PC3
p2 <- fviz_pca_var(data.pca, axes = c(1, 3), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), label = "var", repel = TRUE) +
  ggtitle("PC1 vs PC3")
# Plot for PC2 vs PC3
p3 <- fviz_pca_var(data.pca, axes = c(2, 3), geom = c("arrow", "text"),
                   col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),label = "var", repel = TRUE) +
  ggtitle("PC2 vs PC3")

combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)
#Contributions of each numerical variable
fviz_cos2(data.pca, choice = "var", axes = 1:3)
#Correlation between variables with PC
r.pc.x <- cor(normalized_data, data.pca$x)
r.pc.x
par(mfrow =c(1,3))
plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
     col = "red",
     xlab = "cor with PC1", ylab = "cor with PC2",
     main = "PC1 vs PC2",asp = 1)
text(x=r.pc.x[,c(1,2)], labels = colnames(normalized_data), cex = 1, col = "blue")
plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
     col = "red",
     xlab = "cor with PC1", ylab = "cor with PC3",
     main = "PC1 vs PC3",asp = 1)
text(x=r.pc.x[,c(1,3)], labels = colnames(normalized_data), cex = 1, col = "blue")
plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
     col = "red",
     xlab = "cor with PC2", ylab = "cor with PC3",
     main = "PC2 vs PC3",asp = 1)
text(x=r.pc.x[,c(2,3)], labels = colnames(normalized_data), cex = 1, col = "blue")
par(mfrow =c(1,1))


################################################################################
# --- Factor Analysis (FA) ---
################################################################################


options(digits=3)

dim(df_raw_for_processing)
selected_data = df_raw_for_processing[, c("age", "balance", "duration", "campaign", "pdays", "previous")]
normalized_data = scale(selected_data)
get_d = function(k, p){
  return(0.5*(k-p)^2 - 0.5*(p+k))
}
X <- as.matrix(normalized_data)
head(X)
cor(X)
n <- nrow(X)
p <- ncol(X)

d <- get_d(k=1:6, p=p)
d

#1)The principal component method-----------------------------------------------
eigen <- eigen(cov(X))
plot(eigen$values)

Q <- eigen$vectors %*% diag(sqrt(eigen$values))
Q

Q <- Q[,1:2]
Q

psi <- diag(cov(X) - Q %*% t(Q))
psi

rot.varimax <- varimax(Q)
Q.rot <- rot.varimax$loadings
Q.rot

psi.rot <- diag(cov(X) - Q.rot %*% t(Q.rot))
psi.rot
#)Maximum likelihood method-----------------------------------------------------
k <- 2
d <- d[k]
d

fa.data <- factanal(~., 
                    factors = k,
                    rotation = "none",
                    scores = "regression", 
                    data = data.frame(X))
fa.data
fa.data$loadings %*% t(fa.data$loadings) + diag(fa.data$uniquenesses)
cor(X)

apply(fa.data$loadings^2, 1, sum) 
1 - apply(fa.data$loadings^2, 1, sum)

#Likelihood ratio test (Recall: Wilk)
Q <- fa.data$loadings
t_stat <- n * log(det(Q %*% t(Q) + diag(fa.data$uniquenesses)) / det(cor(X)))
t_stat

# Improve the test statistics (Bartlett 1954)
t_stat <- (n-1-(2*p+4*k+5)/6) * log(det(Q %*% t(Q) + diag(fa.data$uniquenesses)) / det(cor(X)))
t_stat
pchisq(t_stat, df = d, lower.tail = FALSE)

fa.data.varimax <- factanal(~., 
                            factors = k,
                            rotation = "varimax",
                            scores = "regression", 
                            data = data.frame(X))
fa.data.varimax$uniquenesses
fa.data.varimax$loadings

apply(fa.data.varimax$loadings^2, 1, sum)
fa.data.varimax$uniquenesses + apply(fa.data.varimax$loadings^2, 1, sum)
fa.data.varimax$loadings %*% t(fa.data.varimax$loadings) + diag(fa.data.varimax$uniquenesses)
fa.data.varimax$correlation
cor(X)

apply(fa.data.varimax$loadings^2, 2, sum)
apply(fa.data.varimax$loadings^2, 2, sum)/
  sum(c(apply(fa.data.varimax$loadings^2, 2, sum), fa.data.varimax$uniquenesses))


#test for number of factors
n * log((det(fa.data.varimax$loadings %*% t(fa.data.varimax$loadings)
             + diag(fa.data.varimax$uniquenesses)))/det(cor(X)))
(n-1-((2*p+4*k+5)/6)) * log((det(fa.data.varimax$loadings %*% t(fa.data.varimax$loadings) +
                                   diag(fa.data.varimax$uniquenesses)))/det(cor(X)))
pchisq(7.66, df = d, lower.tail = FALSE) 

# Visualization
par(mfrow=c(1,2))
# par(mfrow=c(2,2))
plot(cbind(cos((0:360)/180*pi), sin((0:360)/180*pi)),
     type = 'l',
     lty = 'dotted',
     xlab = 'Factor1 ', 
     ylab = 'Factor2',
     main = "No rotation",
     repel = TRUE)
abline(h=0)
abline(v=0)
text(fa.data$loadings[,1:2],
     labels=colnames(X),
     col="black")

plot(cbind(cos((0:360)/180*pi),sin((0:360)/180*pi)),
     type="l",
     lty="dotted",
     xlab = "Factor 1",
     ylab = "Factor 2",
     main="Varimax",
     repel = TRUE)
abline(h = 0)
abline(v = 0)
text(fa.data.varimax$loadings[,1:2],
     labels=colnames(X),
     col="black")


cbind(fa.data$loadings[,1], fa.data.varimax$loadings[,1])


################################################################################
# --- Prediction tasks ---
################################################################################

# Convert to proper data types
df_raw_for_processing <- as.data.frame(df_raw_for_processing)
numeric_cols <- c("age", "balance", "duration", "campaign", "pdays", "previous")

for (col in numeric_cols) {
  df_raw_for_processing[[col]] <- as.numeric(df_raw_for_processing[[col]])
}

categorical_cols <- c("default", "housing", "loan", "job", "marital", "education", "month", "weekday")

for (col in categorical_cols) {
  df_raw_for_processing[[col]] <- as.factor(df_raw_for_processing[[col]])
}

df_raw_for_processing$y <- as.factor(df_raw_for_processing$y)

# Data Splitting
set.seed(123)
inTrain <- createDataPartition(
  y = df_raw_for_processing$y,
  p = 0.90,                    
  list = FALSE                
)

training <- df_raw_for_processing[inTrain, ]
testing <- df_raw_for_processing[-inTrain, ]

# 1) Decision Tree --------------------------------------------------------------
cat("\n--- Decision Tree Model for Classification ('y') ---\n")
tree_model <- rpart(y ~ ., data = training, method = "class")

# Visualize decision tree
rpart.plot(tree_model,
           fallen.leaves = TRUE,
           type = 3,
           extra = 101,
           box.palette = "GnBu",
           shadow.col = "lightgrey",
           branch.lty = 1,
           cex = 0.8,
           split.cex = 1.0,
           tweak = 1.2,
           under = TRUE,
           border.col = "darkgrey",
           nn = FALSE
)

# Make predictions
predictions <- predict(tree_model, newdata = testing, type = "class")
cat("\nDecision Tree Predictions (first 10):\n")
print(head(predictions, 10))

# Model Evaluation
cat("\nDecision Tree Model Evaluation (Confusion Matrix):\n")
confusionMatrix(predictions, testing$y)

predict_probabilities <- predict(tree_model, newdata = testing, type = "prob")
cat("\nDecision Tree Prediction Probabilities (first 10 rows):\n")
print(head(predict_probabilities, 10))

# 2) Logistic Regression --------------------------------------------------------
### MONTH + WEEKDAY
cat("\n--- Logistic Regression Model for Classification ('y') ---\n")
logistic_model <- glm(y ~ age + job + marital + education + default + balance + 
                        housing + loan + month + duration + campaign + pdays + 
                        previous + weekday,
                      data = training, family = "binomial")

cat("\nLogistic Regression Summary:\n")
print(summary(logistic_model))

logistic_pred_probs <- predict(logistic_model, newdata = testing, type = "response")
cat("\nLogistic Regression Prediction Probabilities (first 10):\n")
print(head(logistic_pred_probs, 10))

logistic_pred_classes <- ifelse(logistic_pred_probs > 0.5, "yes", "no")
logistic_pred_classes <- factor(logistic_pred_classes, levels = levels(testing$y))

cat("\nLogistic Regression Model Evaluation (Confusion Matrix):\n")
confusionMatrix(logistic_pred_classes, testing$y)

# ROC Curve
cat("\nPlotting ROC curve for Logistic Regression...\n")
roc_obj_logistic <- roc(testing$y, logistic_pred_probs)
plot(roc_obj_logistic, main = "ROC Curve for Logistic Regression")
auc_logistic <- auc(roc_obj_logistic)
cat("\nAUC for Logistic Regression:", auc_logistic, "\n")

### BY MONTH ONLY

cat("\n--- Logistic Regression Model for Classification ('y') — Using MONTH only ---\n")

# Fit the logistic regression model using MONTH instead of WEEKDAY
logistic_model_month <- glm(y ~ age + job + marital + education + default + balance + 
                              housing + loan + month + duration + campaign + pdays + 
                              previous,
                            data = training, family = "binomial")

cat("\nLogistic Regression Summary:\n")
print(summary(logistic_model_month))

# Predict probabilities on test data
logistic_pred_probs_month <- predict(logistic_model_month, newdata = testing, type = "response")
cat("\nLogistic Regression Prediction Probabilities (first 10):\n")
print(head(logistic_pred_probs_month, 10))

# Convert probabilities to class labels using threshold 0.5
logistic_pred_classes_month <- ifelse(logistic_pred_probs_month > 0.5, "yes", "no")
logistic_pred_classes_month <- factor(logistic_pred_classes_month, levels = levels(testing$y))

# Confusion Matrix
cat("\nLogistic Regression Model Evaluation (Confusion Matrix):\n")
confusionMatrix(logistic_pred_classes_month, testing$y)

# ROC Curve
cat("\nPlotting ROC curve for Logistic Regression (Month Only)...\n")
roc_obj_logistic_month <- roc(testing$y, logistic_pred_probs_month)
plot(roc_obj_logistic_month, main = "ROC Curve for Logistic Regression (Month Only)")
auc_logistic_month <- auc(roc_obj_logistic_month)
cat("\nAUC for Logistic Regression (Month Only):", auc_logistic_month, "\n")

### BY WEEKDAY

cat("\n--- Logistic Regression Model for Classification ('y') — Using WEEKDAY only ---\n")

# Fit the logistic regression model using WEEKDAY instead of MONTH
logistic_model_weekday <- glm(y ~ age + job + marital + education + default + balance + 
                                housing + loan + weekday + duration + campaign + pdays + 
                                previous,
                              data = training, family = "binomial")

cat("\nLogistic Regression Summary:\n")
print(summary(logistic_model_weekday))

# Predict probabilities on test data
logistic_pred_probs_weekday <- predict(logistic_model_weekday, newdata = testing, type = "response")
cat("\nLogistic Regression Prediction Probabilities (first 10):\n")
print(head(logistic_pred_probs_weekday, 10))

# Convert probabilities to class labels using threshold 0.5
logistic_pred_classes_weekday <- ifelse(logistic_pred_probs_weekday > 0.5, "yes", "no")
logistic_pred_classes_weekday <- factor(logistic_pred_classes_weekday, levels = levels(testing$y))

# Confusion Matrix
cat("\nLogistic Regression Model Evaluation (Confusion Matrix):\n")
confusionMatrix(logistic_pred_classes_weekday, testing$y)

# ROC Curve
cat("\nPlotting ROC curve for Logistic Regression (Weekday Only)...\n")
roc_obj_logistic_weekday <- roc(testing$y, logistic_pred_probs_weekday)
plot(roc_obj_logistic_weekday, main = "ROC Curve for Logistic Regression (Weekday Only)")
auc_logistic_weekday <- auc(roc_obj_logistic_weekday)
cat("\nAUC for Logistic Regression (Weekday Only):", auc_logistic_weekday, "\n")
#COVARIANCES BETWEEN YES AND NO

# Ensure numeric features are selected
numeric_cols <- c("age", "balance", "duration", "campaign", "pdays", "previous")

# Split by class
data_yes <- df_raw_for_processing %>% filter(y == "yes") %>% dplyr::select(all_of(numeric_cols))
data_no  <- df_raw_for_processing %>% filter(y == "no")  %>% dplyr::select(all_of(numeric_cols))

# Compute covariance matrices
cov_yes <- cov(data_yes, use = "complete.obs")
cov_no  <- cov(data_no, use = "complete.obs")

cat("\nCovariance Matrix for 'yes':\n")
print(round(cov_yes, 2))

cat("\nCovariance Matrix for 'no':\n")
print(round(cov_no, 2))


# Optional: correlation matrices instead of covariance
cor_yes <- cor(data_yes, use = "complete.obs")
cor_no <- cor(data_no, use = "complete.obs")

# Install if needed
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

par(mfrow = c(1, 2))  # Side-by-side plots

corrplot(cor_yes, method = "color", title = "'yes' Correlation Matrix", mar = c(0,0,2,0))
corrplot(cor_no, method = "color", title = "'no' Correlation Matrix", mar = c(0,0,2,0))

par(mfrow = c(1, 1))  # Reset


# 3) Linear Discriminant Analysis (LDA) ----------------------------------------
cat("\n--- Linear Discriminant Analysis (LDA) Model for Classification ('y') ---\n")
lda_model <- lda(y ~ ., data = training)
lda_pred <- predict(lda_model, newdata = testing)
lda_class <- lda_pred$class

cat("\nLDA Predictions (first 10):\n")
print(head(lda_class, 10))

# Model Evaluation
cat("\nLDA Model Evaluation (Confusion Matrix):\n")
cm_lda <- confusionMatrix(lda_class, testing$y)
print(cm_lda)

lda_pred_posterior_yes <- lda_pred$posterior[, "yes"]

# ROC Curve
cat("\nPlotting ROC curve for LDA...\n")
lda_roc <- roc(testing$y, lda_pred_posterior_yes)
plot(lda_roc, main = "ROC Curve - LDA")
auc_lda <- auc(lda_roc)
cat("\nAUC for LDA:", auc_lda, "\n")

# Prediction counts
cat("\n--- Number of 'yes' and 'no' predictions by LDA model ---\n")
print(table(lda_class))

cat("\n--- Breakdown of LDA classifications from Confusion Matrix ---\n")
print(cm_lda$table)

cat("\nLDA: Predicted 'no' count:", cm_lda$table["no", "no"] + cm_lda$table["no", "yes"], "\n")
cat("LDA: Predicted 'yes' count:", cm_lda$table["yes", "no"] + cm_lda$table["yes", "yes"], "\n")

cat("\n--- Actual number of 'yes' and 'no' in the test data ---\n")
print(table(testing$y))

# 4)Quadratic Discriminant Analysis (QDA) --------------------------------------

cat("\n--- Quadratic Discriminant Analysis (QDA) Model for Classification ('y') ---\n")

# Train QDA model
qda_model <- qda(y ~ ., data = training)

# Predict class and posterior probabilities
qda_pred <- predict(qda_model, newdata = testing)
qda_class <- qda_pred$class
qda_pred_posterior_yes <- qda_pred$posterior[, "yes"]

# Show sample predictions
cat("\nQDA Predictions (first 10):\n")
print(head(qda_class, 10))

# Confusion Matrix
cat("\nQDA Model Evaluation (Confusion Matrix):\n")
cm_qda <- confusionMatrix(qda_class, testing$y)
print(cm_qda)

# ROC Curve
cat("\nPlotting ROC curve for QDA...\n")
qda_roc <- roc(testing$y, qda_pred_posterior_yes)
plot(qda_roc, main = "ROC Curve - QDA")
auc_qda <- auc(qda_roc)
cat("\nAUC for QDA:", auc_qda, "\n")

# Prediction counts
cat("\n--- Number of 'yes' and 'no' predictions by QDA model ---\n")
print(table(qda_class))

cat("\n--- Breakdown of QDA classifications from Confusion Matrix ---\n")
print(cm_qda$table)

cat("\nQDA: Predicted 'no' count:", cm_qda$table["no", "no"] + cm_qda$table["no", "yes"], "\n")
cat("QDA: Predicted 'yes' count:", cm_qda$table["yes", "no"] + cm_qda$table["yes", "yes"], "\n")

cat("\n--- Actual number of 'yes' and 'no' in the test data ---\n")
print(table(testing$y))


# 4) Random Forest --------------------------------------------------------
cat("\n--- Random Forest Model for Classification ('y') ---\n")
set.seed(123)
rf_model <- randomForest(y ~ ., data = training, ntree = 500, importance = TRUE)

cat("\nRandom Forest Model Summary:\n")
print(rf_model)

# Make predictions
rf_pred_class <- predict(rf_model, newdata = testing)
rf_pred_prob <- predict(rf_model, newdata = testing, type = "prob")[, "yes"]

cat("\nRandom Forest Predictions (first 10):\n")
print(head(rf_pred_class, 10))

# Confusion Matrix
cat("\nRandom Forest Model Evaluation (Confusion Matrix):\n")
confusionMatrix(rf_pred_class, testing$y)

# Variable Importance
cat("\nVariable Importance:\n")
print(importance(rf_model))
varImpPlot(rf_model, main = "Variable Importance - Random Forest")

# ROC Curve and AUC
cat("\nPlotting ROC curve for Random Forest...\n")
roc_rf <- roc(testing$y, rf_pred_prob)
plot(roc_rf, main = "ROC Curve - Random Forest")
auc_rf <- auc(roc_rf)
cat("\nAUC for Random Forest:", auc_rf, "\n")

################################################################################
# --- Prediction Evaluations ---
################################################################################

# Define evaluation function
evaluate_model <- function(true, pred_class, pred_prob = NULL, positive_class = "yes") {
  cm <- confusionMatrix(pred_class, true, positive = positive_class)
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]
  accuracy <- cm$overall["Accuracy"]
  auc_val <- if (!is.null(pred_prob)) auc(roc(true, pred_prob)) else NA
  
  return(data.frame(Accuracy = accuracy,
                    Precision = precision,
                    Recall = recall,
                    F1_Score = f1,
                    AUC = auc_val))
}

# Evaluate all models
dt_results <- evaluate_model(testing$y, predictions, predict_probabilities[, "yes"])
log_results <- evaluate_model(testing$y, logistic_pred_classes, logistic_pred_probs)
lda_results <- evaluate_model(testing$y, lda_class, lda_pred_posterior_yes)
qda_results <- evaluate_model(testing$y, qda_class, qda_pred_posterior_yes)
rf_results <- evaluate_model(testing$y, rf_pred_class, rf_pred_prob)

# Combine into summary table
model_eval_summary <- rbind(
  "Decision Tree" = dt_results,
  "Logistic Regression" = log_results,
  "LDA" = lda_results,
  "QDA" = qda_results,
  "Random Forest" = rf_results
)

# Round and print
model_eval_summary <- round(model_eval_summary, 3)
cat("\nModel Evaluation Summary:\n")
print(model_eval_summary)

# Visual comparison of model performance
performance_plot <- model_eval_summary %>%
  rownames_to_column(var = "Model") %>%
  gather(key = "Metric", value = "Value", -Model) %>%
  ggplot(aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison",
       y = "Value",
       x = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(performance_plot)

