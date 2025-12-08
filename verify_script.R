## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(lubridate)
library(corrplot)
library(gridExtra)
library(scales) # For formatting numbers


## ----load_data----------------------------------------------------------------
# Define file paths (assuming current working directory)
# Common function to read CSV
read_data <- function(filename) {
  read_csv(filename, show_col_types = FALSE)
}

# Load Train Data
demographics_train <- read_data("customer_demographics_train.csv")
mrr_train <- read_data("customer_monthly_recurring_revenue_train.csv")
region_train <- read_data("customer_region_and_industry_train.csv")
revenue_train <- read_data("customer_revenue_history_train.csv")
satisfaction_train <- read_data("customer_satisfaction_scores_train.csv")
status_train <- read_data("customer_status_level_train.csv") # TARGET
newsletter_train <- read_data("newsletter_engagement_train.csv")
bugs_train <- read_data("product_bug_reports_train.csv")
support_train <- read_data("support_ticket_activity_train.csv")


## ----merge_data---------------------------------------------------------------
# Rename CUS ID
demographics_train <- demographics_train %>% rename(`Customer ID` = `CUS ID`)

# List of dataframes to merge
list_dfs <- list(
  status_train, demographics_train, mrr_train, region_train,
  revenue_train, satisfaction_train, newsletter_train,
  bugs_train, support_train
)

# Merge all into one master dataframe
df_train <- list_dfs %>% reduce(full_join, by = "Customer ID")

cat("Merged Dataset Dimensions:", dim(df_train)[1], "rows and", dim(df_train)[2], "columns.\n")


## ----date_conversion----------------------------------------------------------
# Clean Currency Columns
clean_currency <- function(x) {
  as.numeric(gsub("[$,]", "", x))
}

df_train$`Survey Date` <- ymd(df_train$`Survey Date`)
df_train$`Response Date` <- ymd(df_train$`Response Date`)
df_train$MRR <- clean_currency(df_train$MRR)
df_train$`Total Revenue` <- clean_currency(df_train$`Total Revenue`)


## ----target_dist--------------------------------------------------------------
ggplot(df_train, aes(x = Status, fill = Status)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Customer Status")


## ----num_dist, fig.height=8, fig.width=12-------------------------------------
p1 <- ggplot(df_train, aes(x = MRR)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::dollar) +
  theme_minimal() +
  labs(title = "Monthly Recurring Revenue (MRR)", y = "Number of Customers")

p2 <- ggplot(df_train, aes(x = `Customer Age (Months)`)) +
  geom_histogram(bins = 30, fill = "seagreen", color = "white") +
  theme_minimal() +
  labs(title = "Customer Tenure (Months)", y = "Number of Customers")

# Fixed scientific notation using scale_x_continuous(labels = comma)
p3 <- ggplot(df_train, aes(x = `Total Revenue`)) +
  geom_histogram(bins = 30, fill = "purple", color = "white") +
  scale_x_log10(labels = scales::dollar) +
  theme_minimal() +
  labs(title = "Total Revenue", y = "Number of Customers")

p4 <- ggplot(df_train, aes(x = `Help Ticket Count`)) +
  geom_histogram(bins = 30, fill = "orange", color = "white") +
  theme_minimal() +
  labs(title = "Help Ticket Count", y = "Number of Customers")

grid.arrange(p1, p2, p3, p4, ncol = 2)


## ----boxplots, fig.height=6, fig.width=10-------------------------------------
p_mrr <- ggplot(df_train, aes(x = Status, y = MRR, fill = Status)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  labs(title = "MRR by Status")

p_tenure <- ggplot(df_train, aes(x = Status, y = `Customer Age (Months)`, fill = Status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tenure by Status")

grid.arrange(p_mrr, p_tenure, ncol = 2)


## ----correlation, fig.height=10, fig.width=10---------------------------------
# Select numeric columns and remove ID-like or year columns
nums <- df_train %>%
  select_if(is.numeric) %>%
  select(-c(`Year`, `Quarter`)) %>%
  na.omit() # Remove rows with NAs for correlation

# Compute correlation matrix
M <- cor(nums)

# Plot with adjusted text size (tl.cex) to fix "text margin" error
corrplot(M,
  method = "color", type = "upper",
  tl.col = "black", tl.srt = 45, tl.cex = 0.6,
  addCoef.col = "black", number.cex = 0.6,
  diag = FALSE
)


## ----missing_analysis---------------------------------------------------------
# Visualize missingness
missing_counts <- colSums(is.na(df_train))
missing_dist <- data.frame(Column = names(missing_counts), Missing_Count = missing_counts)
missing_dist <- missing_dist[missing_dist$Missing_Count > 0, ]
missing_dist <- missing_dist[order(-missing_dist$Missing_Count), ]
rownames(missing_dist) <- NULL

knitr::kable(missing_dist, caption = "Missing Values Breakdown")

ggplot(missing_dist, aes(x = reorder(Column, Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Missing Values Count by Column", x = "Column", y = "Count of Missing Values")


## ----imputation---------------------------------------------------------------
# Define imputation functions
impute_zero <- function(x) replace_na(x, 0)
impute_unknown <- function(x) replace_na(x, "Unknown")
impute_median <- function(x) replace_na(x, median(x, na.rm = TRUE))

df_clean <- df_train

# Impute Zero
df_clean$`Company Newsletter Interaction Count` <- impute_zero(df_clean$`Company Newsletter Interaction Count`)
df_clean$`Product Bug Task Count` <- impute_zero(df_clean$`Product Bug Task Count`)
df_clean$`Help Ticket Count` <- impute_zero(df_clean$`Help Ticket Count`)
df_clean$`Help Ticket Lead Time (hours)` <- impute_zero(df_clean$`Help Ticket Lead Time (hours)`)

# Financials
df_clean$MRR <- impute_zero(df_clean$MRR)
df_clean$`Total Revenue` <- impute_zero(df_clean$`Total Revenue`)

# Impute Median
df_clean$`How likely are you to recommend insider to a friend or colleague` <- impute_median(df_clean$`How likely are you to recommend insider to a friend or colleague`)
df_clean$`Please rate the overall quality of our products` <- impute_median(df_clean$`Please rate the overall quality of our products`)
df_clean$`Please rate the usability of the panel` <- impute_median(df_clean$`Please rate the usability of the panel`)
df_clean$`How would you rate the value you gain from our company` <- impute_median(df_clean$`How would you rate the value you gain from our company`)

# Impute Categorical
df_clean$Region <- impute_unknown(df_clean$Region)
df_clean$Vertical <- impute_unknown(df_clean$Vertical)
df_clean$Subvertical <- impute_unknown(df_clean$Subvertical)
df_clean$`Customer Level` <- impute_unknown(df_clean$`Customer Level`)
df_clean$`How frequently are you using our platform` <- impute_unknown(df_clean$`How frequently are you using our platform`)

# Drop rows missing Target
df_clean <- df_clean[!is.na(df_clean$Status), ]

# Verify no missing values remaining in key columns
sum(is.na(df_clean))


## ----duplicates---------------------------------------------------------------
# Check for duplicates
original_rows <- nrow(df_clean)
df_clean <- distinct(df_clean)
final_rows <- nrow(df_clean)

cat("Removed", original_rows - final_rows, "duplicate rows.\n")


## ----feature_eng--------------------------------------------------------------
df_features <- df_clean

# Time-based features
df_features$`Response Time (Days)` <- as.numeric(difftime(df_features$`Response Date`, df_features$`Survey Date`, units = "days"))

# Interaction Ratios (Normalize by Tenure)
tenure_adj <- df_features$`Customer Age (Months)` + 1
df_features$`Tickets Per Month` <- df_features$`Help Ticket Count` / tenure_adj
df_features$`Bugs Per Month` <- df_features$`Product Bug Task Count` / tenure_adj
df_features$`Revenue Per Month` <- df_features$`Total Revenue` / tenure_adj

# Text Length
df_features$`Understanding Score Length` <- str_length(df_features$`Please rate your understanding of our reporting capabilities in the panel`)

# Handle NAs (created if dates were missing)
df_features$`Response Time (Days)` <- replace_na(df_features$`Response Time (Days)`, 0)
df_features$`Understanding Score Length` <- replace_na(df_features$`Understanding Score Length`, 0)

glimpse(df_features)


## ----encoding-----------------------------------------------------------------
df_final <- df_features

# Convert to Factors
df_final$Status <- as.factor(df_final$Status)
df_final$Region <- as.factor(df_final$Region)
df_final$Vertical <- as.factor(df_final$Vertical)
# Subvertical has >50 levels, which causes rpart to hang (2^50 splits). We exclude it.
# df_final$Subvertical <- as.factor(df_final$Subvertical)

df_final$`Customer Level` <- as.factor(df_final$`Customer Level`)
df_final$`How frequently are you using our platform` <- as.factor(df_final$`How frequently are you using our platform`)

# Remove identifiers and high-cardinality columns not suitable for trees
df_final$`Customer ID` <- NULL
df_final$Subvertical <- NULL
df_final$`Survey Date` <- NULL
df_final$`Response Date` <- NULL

# Check class balance again on final dataset
summary(df_final$Status)


## ----pca_viz, fig.height=5, fig.width=12--------------------------------------
library(cluster)

# 1. Prepare Data (Clean & Scale)
df_clustering <- df_final %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0)) %>% # Remove constant columns
  drop_na()

cluster_features <- scale(df_clustering)

# 2. Perform PCA
pca_res <- prcomp(cluster_features, center = TRUE, scale. = TRUE)
df_pca <- as.data.frame(pca_res$x)

# Join Status back for visualization
# Ensure row alignment with the dropped NAs
df_clustering$Status <- df_final$Status[as.numeric(rownames(df_clustering))]
df_pca$Status <- df_clustering$Status

# 3. Visualize
# Before PCA: Visualizing Raw Correlated Features
p_raw <- ggplot(df_clustering, aes(x = `Total Revenue`, y = MRR, color = Status)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Before PCA: Raw Features")

# After PCA: Visualizing Principal Components
p_pca <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Status)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "After PCA: Principal Components")

grid.arrange(p_raw, p_pca, ncol = 2)


## ----kmeans, fig.height=6, fig.width=10---------------------------------------
# Reuse the scaled data from PCA step (cluster_features)

# --- Perform k-Means ---
set.seed(123)
kmeans_model <- kmeans(cluster_features, centers = 3, nstart = 25)

# Add cluster labels to PCA data for visualization
df_pca$Cluster <- as.factor(kmeans_model$cluster)

# Visualize Clusters using PCA coordinates
ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal() +
  scale_color_discrete(labels = c("1: Onboarding (New)", "2: Retained (Core)", "3: Churn (Risk)")) +
  labs(
    title = "Customer Segments (k-Means)",
    subtitle = "Visualized on first 2 Principal Components"
  )


## ----smote_prep, message=FALSE, warning=FALSE---------------------------------
library(smotefamily)
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)

# Prepare Data for SMOTE (Numeric only + Target)
# We need to one-hot encode factors first
# Simple One-Hot Encoding for Model Matrix
df_model_prep <- df_final %>% drop_na()
nums <- df_model_prep %>% select(where(is.numeric))
cats <- df_model_prep %>% select(where(is.factor), -Status)

# Create dummy variables for categorical predictors
# using model.matrix which converts factors to dummies
cats_dummies <- as.data.frame(model.matrix(~ . - 1, data = cats))

# Combine
df_smote_ready <- cbind(nums, cats_dummies, Status = df_model_prep$Status)

# Sanitize names for modeling (rpart doesn't like spaces)
names(df_smote_ready) <- make.names(names(df_smote_ready))

# Split Train/Val (80/20) BEFORE SMOTE
set.seed(123)
train_idx <- sample(seq_len(nrow(df_smote_ready)), size = 0.8 * nrow(df_smote_ready))
train_raw <- df_smote_ready[train_idx, ]
val_set <- df_smote_ready[-train_idx, ]

# Apply SMOTE to TRAINING set only
# SMOTE takes numeric inputs. 'Status' is the target.
# We use smote_family::SMOTE.
# dup_size = 0 means auto-detect amount needed.
smote_result <- SMOTE(
  X = train_raw[, -which(names(train_raw) == "Status")],
  target = train_raw$Status
)

# smotefamily returns a list. $data contains the balanced dataset.
train_balanced <- smote_result$data
# Rename class column back to 'Status' (SMOTE renames it to 'class')
names(train_balanced)[names(train_balanced) == "class"] <- "Status"
train_balanced$Status <- as.factor(train_balanced$Status)

cat("Original Train Size:", nrow(train_raw), "\n")
cat("Balanced Train Size:", nrow(train_balanced), "\n")
table(train_balanced$Status)


## ----model_dt_balanced, message=FALSE, warning=FALSE--------------------------
# 1. Train the Model
tree_model <- rpart(Status ~ ., data = train_balanced, method = "class")

# 2. Prune the Tree (for better generalization)
# cp = 0.005 allows the tree to be complex enough to capture 'Churn'
pruned_viz <- prune(tree_model, cp = 0.005)

# 3. Evaluate on Validation Set (Unseen Data)
val_preds_tree <- predict(pruned_viz, newdata = val_set, type = "class")

# 4. Generate Confusion Matrix
cm_tree <- confusionMatrix(val_preds_tree, as.factor(val_set$Status))

# Display Accuracy and Stats
print(cm_tree$overall["Accuracy"])
print(cm_tree$table)

# 5. Visualize Rules
rpart.plot(pruned_viz,
  type = 4,
  extra = 104,
  shadow.col = "gray",
  nn = TRUE,
  main = "Decision Tree Rules (Pruned)"
)

# 6. Visualize Predictions (Confusion Matrix Heatmap)
cm_tree_df <- as.data.frame(cm_tree$table)
ggplot(cm_tree_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "#fee6ce", high = "#e6550d") + # Orange theme
  labs(
    title = "Decision Tree Performance: Confusion Matrix",
    subtitle = "Prediction Accuracy Heatmap",
    x = "Actual Status",
    y = "Predicted Status"
  ) +
  theme_minimal()


## ----model_multi_balanced, message=FALSE, warning=FALSE-----------------------
# 1. Train the Model
# trace=FALSE hides the iteration log
multi_model <- multinom(Status ~ ., data = train_balanced, trace = FALSE)

# 2. Evaluate on Validation Set (Unseen Data)
# We use the 'val_set' we created before SMOTE to test real-world performance
val_preds <- predict(multi_model, newdata = val_set)

# 3. Generate Confusion Matrix
cm <- confusionMatrix(val_preds, as.factor(val_set$Status))

# Display Accuracy and Stats
print(cm$overall["Accuracy"])
print(cm$table)

# 4. Visualize Performance (Confusion Matrix Heatmap)
cm_df <- as.data.frame(cm$table)
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Model 2 Performance: Confusion Matrix",
    subtitle = "Darker squares = More predictions. Diagonal = Correct.",
    x = "Actual Status (Truth)",
    y = "Predicted Status"
  ) +
  theme_minimal()


## ----model_rf_balanced, message=FALSE, warning=FALSE--------------------------
library(randomForest)

# 1. Train the Model
# ntree=100 is sufficient for this data size and faster than default 500
set.seed(123)
rf_model <- randomForest(Status ~ ., data = train_balanced, ntree = 100, importance = TRUE)

# 2. Evaluate on Validation Set
val_preds_rf <- predict(rf_model, newdata = val_set)

# 3. Generate Confusion Matrix
cm_rf <- confusionMatrix(val_preds_rf, as.factor(val_set$Status))

# Display Accuracy and Stats
print(cm_rf$overall["Accuracy"])
print(cm_rf$table)
print("--- Class-Specific Metrics (Sensitivity & Precision) ---")
print(cm_rf$byClass[, c("Sensitivity", "Pos Pred Value")])

# 4. Visualize Feature Importance (Professional ggplot)
# Extract importance dataframe
imp_df <- as.data.frame(importance(rf_model))
imp_df$Feature <- rownames(imp_df)

# We use 'MeanDecreaseAccuracy' as the primary metric
# Select Top 20 features to avoid overcrowding
top_features <- imp_df %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  head(20)

# Create Professional Plot
ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() + # Horizontal bars for readability
  theme_minimal() +
  labs(
    title = "Random Forest: Top 20 Key Drivers",
    subtitle = "Features with highest impact on model accuracy",
    x = "",
    y = "Importance (Mean Decrease Accuracy)"
  ) +
  theme(axis.text.y = element_text(size = 10)) # Ensure labels are readable

# Interpretation
# The Feature Importance plot highlights that Total Revenue is the primary predictor of customer status, indicating that behavioral metrics outweigh static demographic data.

# 5. Visualize Predictions (Confusion Matrix Heatmap)
cm_rf_df <- as.data.frame(cm_rf$table)
ggplot(cm_rf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#e5f5e0", high = "#31a354") + # Green theme
  labs(
    title = "Random Forest Performance: Confusion Matrix",
    subtitle = "Prediction Accuracy Heatmap",
    x = "Actual Status",
    y = "Predicted Status"
  ) +
  theme_minimal()


## ----model_comparison, message=FALSE, warning=FALSE---------------------------
# Collect Accuracies
model_performance <- data.frame(
  Model = c("Decision Tree", "Multinomial Regression", "Random Forest"),
  Accuracy = c(cm_tree$overall["Accuracy"], cm$overall["Accuracy"], cm_rf$overall["Accuracy"])
)

# Visualize Comparison
ggplot(model_performance, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + # Add headroom for labels
  scale_fill_manual(values = c("#e6550d", "steelblue", "#31a354")) + # Match previous colors: Orange, Blue, Green
  labs(
    title = "Final Model Showdown",
    subtitle = "Accuracy on Validation Set (Higher is Better)",
    x = "",
    y = "Accuracy"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

knitr::kable(model_performance, caption = "Performance Summary")


## ----phase6_prep, message=FALSE, warning=FALSE--------------------------------
# Load Test Data
demographics_test <- read_data("customer_demographics_test.csv")
mrr_test <- read_data("customer_monthly_recurring_revenue_test.csv")
region_test <- read_data("customer_region_and_industry_test.csv")
revenue_test <- read_data("customer_revenue_history_test.csv")
satisfaction_test <- read_data("customer_satisfaction_scores_test.csv")
# status_test <- read_data("customer_status_level_test.csv") # TARGET typically missing in test, or we ignore it
newsletter_test <- read_data("newsletter_engagement_test.csv")
bugs_test <- read_data("product_bug_reports_test.csv")
support_test <- read_data("support_ticket_activity_test.csv")

# Standardize Key
demographics_test <- demographics_test %>% rename(`Customer ID` = `CUS ID`)

# Merge (Note: status_test excluded if strictly prediction, but we include if provided for checking)
# Based on files, status_test likely exists. We join it as placeholder or ground truth.
list_dfs_test <- list(
  demographics_test, mrr_test, region_test,
  revenue_test, satisfaction_test, newsletter_test,
  bugs_test, support_test
)
# If status_test exists, we join it too
if (file.exists("customer_status_level_test.csv")) {
  status_test <- read_data("customer_status_level_test.csv")
  list_dfs_test <- append(list(status_test), list_dfs_test)
}

df_test <- list_dfs_test %>% reduce(full_join, by = "Customer ID")

cat("Test Dataset Dimensions:", dim(df_test)[1], "rows.\n")


## ----phase6_feat, message=FALSE, warning=FALSE--------------------------------
# Clean Currency
df_test$`Survey Date` <- ymd(df_test$`Survey Date`)
df_test$`Response Date` <- ymd(df_test$`Response Date`)
df_test$MRR <- clean_currency(df_test$MRR)
df_test$`Total Revenue` <- clean_currency(df_test$`Total Revenue`)

# Create Derived Features
df_test$`Response Time (Days)` <- as.numeric(df_test$`Response Date` - df_test$`Survey Date`)
df_test$`Tickets Per Month` <- df_test$`Help Ticket Count` / df_test$`Customer Age (Months)`
df_test$`Bugs Per Month` <- df_test$`Product Bug Task Count` / df_test$`Customer Age (Months)`
df_test$`Revenue Per Month` <- df_test$`Total Revenue` / df_test$`Customer Age (Months)`

# Handle NAs & Infinite values
df_test <- df_test %>% mutate(across(c(`Tickets Per Month`, `Bugs Per Month`, `Revenue Per Month`), ~ ifelse(is.infinite(.), 0, .)))
df_test$`Response Time (Days)` <- replace_na(df_test$`Response Time (Days)`, 0)
df_test$`Understanding Score Length` <- str_length(df_test$`Please rate your understanding of our reporting capabilities in the panel`)
df_test$`Understanding Score Length` <- replace_na(df_test$`Understanding Score Length`, 0)

# Factor Alignment
test_model_ready <- df_test
test_model_ready$Region <- as.factor(test_model_ready$Region)
test_model_ready$Vertical <- as.factor(test_model_ready$Vertical)
test_model_ready$`Customer Level` <- as.factor(test_model_ready$`Customer Level`)
test_model_ready$`How frequently are you using our platform` <- as.factor(test_model_ready$`How frequently are you using our platform`)

# Fill NAs in numeric columns
test_model_ready <- test_model_ready %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Fill NAs in Factor columns (Crucial: model.matrix drops NAs by default)
# We fill with the most common value (Mode) to keep rows intact
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

test_model_ready <- test_model_ready %>%
  mutate(across(where(is.factor), ~ replace_na(., get_mode(.))))

# IMPORTANT: Reproduce One-Hot Encoding (Match Training Structure)
# The model was trained on SMOTE-prepared data where factors were converted to dummies.
# We must do the exact same transformation here.

# 1. Separate Numeric and Categorical (using same logic as smote_prep)
# Note: We must ensure columns match EXACTLY.
library(caret)

# Select Factor columns
test_cats <- test_model_ready %>% select(where(is.factor))

# Create Dummies
# We use the same formula. Warning: If a level is missing in Test, the column won't be created.
# We might need to enforce column presence.
test_dummies <- as.data.frame(model.matrix(~ . - 1, data = test_cats))

# Select Numeric columns
test_nums <- test_model_ready %>% select(where(is.numeric))

# Combine
test_encoded <- cbind(test_nums, test_dummies)

# Apply make.names to match the training column names (e.g. 'Customer Level Enterprise' -> 'Customer.Level.Enterprise')
names(test_encoded) <- make.names(names(test_encoded))

# Sanity Check: Add missing columns with 0s (in case Test lacks some levels present in Train)
# We can't easily poll the model for the exact list without inspecting 'rf_model$importance' rownames
train_cols <- rownames(rf_model$importance)
missing_cols <- setdiff(train_cols, names(test_encoded))

if (length(missing_cols) > 0) {
  # Add missing columns filled with 0
  test_encoded[missing_cols] <- 0
}

# Reorder columns to match model expectation (optional but safe)
test_encoded <- test_encoded[, train_cols]


## ----phase6_predict, message=FALSE, warning=FALSE-----------------------------
# Predict Status
# Now we use the encoded dataframe
final_predictions <- predict(rf_model, newdata = test_encoded)

# Create Result Table
results_df <- data.frame(
  CustomerID = test_model_ready$`Customer ID`, # Use original ID
  Predicted_Status = final_predictions
)

# Export
write_csv(results_df, "final_customer_predictions.csv")

# Visualize Predicted Distribution
ggplot(results_df, aes(x = Predicted_Status, fill = Predicted_Status)) +
  geom_bar() +
  scale_fill_manual(values = c("#e74c3c", "#f1c40f", "#2ecc71")) + # Red, Yellow, Green
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Forecast: Predicted Status for New Customers",
    subtitle = "Based on Random Forest Model",
    y = "Count"
  ) +
  theme_minimal()

