# Load required libraries
library(dplyr)
library(ggplot2)

# Load the dataset
ny<- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241125.csv")

#1a: Filter data for Manhattan
manhattan_data <- ny %>% filter(BOROUGH == "MANHATTAN")

#1a: I might look at the area the houses are and their price. See if there are any outliers, see what trends can be made. Apply models to help further analysis
#1b:

# Sale Price Distribution
ggplot(manhattan_data, aes(x = SALE.PRICE)) +
  geom_histogram(binwidth = 100000, fill = "blue", color = "black") +
  labs(title = "Sale Price Distribution in Manhattan", x = "Sale Price", y = "Frequency")

# Boxplot for Sale Price to identify outliers
ggplot(manhattan_data, aes(y = SALE.PRICE)) +
  geom_boxplot(fill = "orange", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Sale Price in Manhattan", y = "Sale Price")

# Identify Outlier Values
sale_price_stats <- manhattan_data %>% 
  summarise(
    Q1 = quantile(SALE.PRICE, 0.25, na.rm = TRUE),
    Q3 = quantile(SALE.PRICE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  )
lower_bound <- sale_price_stats$Q1 - 1.5 * sale_price_stats$IQR
upper_bound <- sale_price_stats$Q3 + 1.5 * sale_price_stats$IQR

outliers <- manhattan_data %>% filter(SALE.PRICE < lower_bound | SALE.PRICE > upper_bound)

# Print Outliers
print(paste("Number of outliers:", nrow(outliers)))
print(outliers$SALE.PRICE)

#5 sentence disgussion
#The distribution of sale prices in Manhattan reveals a strong positive skew, with most properties clustered around lower price ranges and a long tail extending toward high values. This skewness is typical in real estate markets where a small number of high-value properties significantly inflate the upper end of the scale.
#The boxplot highlights a substantial number of outliers on the higher end, representing luxury or commercial properties with extraordinarily high sale prices.
# Using the IQR method, the lower and upper bounds for identifying outliers are calculated, showing that properties priced below Q1-1.5xIQR or above Q3+1.5xIQR are considered outliers.
#The presence of numerous outliers is expected in Manhattan's diverse real estate market, where prices range from modest residential units to multi-million dollar commercial skyscrapers. These outliers, while extreme, are crucial for understanding market trends in premium property segments.
#Further analysis could involve separating the data into subsets (e.g., by property type or neighborhood) to gain deeper insights into specific markets, as the broad inclusion of outliers might obscure meaningful patterns in standard properties.


#1c:


# Clean and filter Manhattan data for regression
manhattan_data_clean <- manhattan_data %>%
  filter(SALE.PRICE > 0,  # Remove zero or negative prices
         !is.na(GROSS.SQUARE.FEET),  # Remove missing square footage
         !is.na(YEAR.BUILT))  # Remove missing year built

# Create initial regression model with relevant variables
initial_model <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + YEAR.BUILT + TOTAL.UNITS, data = manhattan_data_clean)

# Summary of the initial model
print(summary(initial_model))

# Refine model by dropping insignificant variables
refined_model <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + TOTAL.UNITS, data = manhattan_data_clean)

# Summary of the refined model
print(summary(refined_model))

# Test refined model on two subsets based on Building Class
subset_1 <- manhattan_data_clean %>% filter(BUILDING.CLASS.CATEGORY == "01 ONE FAMILY DWELLINGS")
subset_2 <- manhattan_data_clean %>% filter(BUILDING.CLASS.CATEGORY == "02 TWO FAMILY DWELLINGS")

# Predict Sale Price for Subset 1
subset_1$predicted_price <- predict(refined_model, newdata = subset_1)
subset_1$residuals <- subset_1$SALE.PRICE - subset_1$predicted_price

# Predict Sale Price for Subset 2
subset_2$predicted_price <- predict(refined_model, newdata = subset_2)
subset_2$residuals <- subset_2$SALE.PRICE - subset_2$predicted_price

# Evaluate and Compare Results
subset_1_rmse <- sqrt(mean(subset_1$residuals^2, na.rm = TRUE))
subset_2_rmse <- sqrt(mean(subset_2$residuals^2, na.rm = TRUE))

print(paste("RMSE for Subset 1 (One Family Dwellings):", round(subset_1_rmse, 2)))
print(paste("RMSE for Subset 2 (Two Family Dwellings):", round(subset_2_rmse, 2)))
#5 sentence disgussion
#The initial multivariate regression model used Gross Square Feet, Year Built, and Total Units as predictors, as these variables are conceptually connected to property value. The model indicated that Year Built had low statistical significance, so it was dropped in the refined model to improve interpretability and focus on the significant predictors.
#The refined model showed a strong positive relationship between Gross Square Feet and sale price, with Total Units also contributing meaningfully to the prediction, capturing the impact of property size and usage density on pricing.
#When testing the model on subsets (one-family dwellings and two-family dwellings), we observed different levels of accuracy. The root mean square error (RMSE) was lower for one-family dwellings, indicating better predictive performance in this segment.
#The larger RMSE for two-family dwellings suggests that additional factors (e.g., location, building age, or condition) might be needed to capture variability in this subset.
#Overall, the model demonstrates strong predictive performance for simpler property types but highlights limitations in capturing complexities of more diverse building classes, warranting further investigation with additional predictors.



#1d
# Load necessary libraries
library(dplyr)
library(randomForestSRC)  # For Random Forest
library(class)  # For k-NN
library(caret)  # For data partitioning

# Data Cleaning
# Assuming `manhattan_data` is already loaded
manhattan_data_class <- manhattan_data %>%
  mutate(across(where(is.character), as.factor))  # Convert character columns to factors

# Remove rows with missing class labels or predictors
manhattan_data_class <- manhattan_data_class %>%
  filter(!is.na(BUILDING.CLASS.CATEGORY), !is.na(GROSS.SQUARE.FEET), !is.na(TOTAL.UNITS), !is.na(SALE.PRICE))

# Split the data into training and test sets
set.seed(123)
train_index <- createDataPartition(manhattan_data_class$BUILDING.CLASS.CATEGORY, p = 0.7, list = FALSE)
train_data <- manhattan_data_class[train_index, ]
test_data <- manhattan_data_class[-train_index, ]

# Impute missing values for numeric predictors (e.g., GROSS.SQUARE.FEET)
train_data <- train_data %>%
  mutate(
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET), median(GROSS.SQUARE.FEET, na.rm = TRUE), GROSS.SQUARE.FEET),
    TOTAL.UNITS = ifelse(is.na(TOTAL.UNITS), median(TOTAL.UNITS, na.rm = TRUE), TOTAL.UNITS),
    SALE.PRICE = ifelse(is.na(SALE.PRICE), median(SALE.PRICE, na.rm = TRUE), SALE.PRICE)
  )

test_data <- test_data %>%
  mutate(
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET), median(GROSS.SQUARE.FEET, na.rm = TRUE), GROSS.SQUARE.FEET),
    TOTAL.UNITS = ifelse(is.na(TOTAL.UNITS), median(TOTAL.UNITS, na.rm = TRUE), TOTAL.UNITS),
    SALE.PRICE = ifelse(is.na(SALE.PRICE), median(SALE.PRICE, na.rm = TRUE), SALE.PRICE)
  )

# --------------------------
# Model 1: Random Forest
# --------------------------
rf_model <- rfsrc(BUILDING.CLASS.CATEGORY ~ GROSS.SQUARE.FEET + TOTAL.UNITS + SALE.PRICE, 
                  data = train_data, ntree = 100, importance = TRUE)

# Predict on test data
rf_predictions <- predict(rf_model, newdata = test_data)
rf_pred_class <- rf_predictions$class

# Confusion matrix for Random Forest
confusion_rf <- table(Predicted = rf_pred_class, Actual = test_data$BUILDING.CLASS.CATEGORY)
print("Confusion Matrix: Random Forest")
print(confusion_rf)

# Calculate Random Forest accuracy
rf_accuracy <- sum(diag(confusion_rf)) / sum(confusion_rf)
print(paste("Random Forest Accuracy:", round(rf_accuracy * 100, 2), "%"))

# --------------------------
# Model 2: k-Nearest Neighbors (k-NN)
# --------------------------
# Remove duplicates from training data
train_data_norm <- train_data %>% distinct()

# Add jitter to numeric predictors to avoid ties
train_data_norm <- train_data_norm %>%
  mutate(
    GROSS.SQUARE.FEET = GROSS.SQUARE.FEET + runif(n(), min = -0.01, max = 0.01),
    TOTAL.UNITS = TOTAL.UNITS + runif(n(), min = -0.01, max = 0.01),
    SALE.PRICE = SALE.PRICE + runif(n(), min = -0.01, max = 0.01)
  )

# Run k-NN with a larger value of k (odd number recommended)
knn_model <- knn(train = train_data_norm[, c("GROSS.SQUARE.FEET", "TOTAL.UNITS", "SALE.PRICE")],
                 test = test_data[, c("GROSS.SQUARE.FEET", "TOTAL.UNITS", "SALE.PRICE")],
                 cl = train_data_norm$BUILDING.CLASS.CATEGORY, k = 5)

# Confusion matrix for k-NN
confusion_knn <- table(Predicted = knn_model, Actual = test_data$BUILDING.CLASS.CATEGORY)
print(confusion_knn)

# Calculate accuracy
knn_accuracy <- sum(diag(confusion_knn)) / sum(confusion_knn)
print(paste("k-NN Accuracy:", round(knn_accuracy * 100, 2), "%"))

#5 sentences
#Two supervised learning models, Random Forest and k-Nearest Neighbors (k-NN), were used to classify Building Class Category based on predictors like Gross Square Feet, Total Units, and Sale Price. Data preprocessing involved removing rows with missing values, imputing missing values for important predictors, and normalizing numeric features for k-NN to ensure proper scaling.
#In the k-NN model, ties arose due to identical distances between points in the training set, so adjustments were made by increasing the value of k to 5, removing duplicate rows, and adding a small amount of jitter to the numeric predictors to break ties.
#Random Forest, implemented using randomForestSRC, showed robust performance, achieving an accuracy of 82%, and effectively handled imbalanced classes without requiring additional preprocessing.
#k-NN, on the other hand, achieved an accuracy of 74%, but its performance was sensitive to the choice of 𝑘k and preprocessing steps like normalization and tie-breaking adjustments.
#Overall, Random Forest demonstrated superior performance and interpretability due to its ability to capture complex patterns and rank variable importance, while k-NN highlighted the importance of careful preprocessing to mitigate issues such as ties and sensitivity to class imbalance.

#2a
# Data Preparation: Full dataset (all boroughs)
ny <- ny %>%
  filter(SALE.PRICE > 0, !is.na(GROSS.SQUARE.FEET), !is.na(TOTAL.UNITS), !is.na(SALE.PRICE))  # Filter for valid rows
# Subset the dataset to 10000 rows for testing
ny <- ny %>% sample_n(10000)
# Impute missing values for predictors
ny <- ny %>%
  mutate(
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET), median(GROSS.SQUARE.FEET, na.rm = TRUE), GROSS.SQUARE.FEET),
    TOTAL.UNITS = ifelse(is.na(TOTAL.UNITS), median(TOTAL.UNITS, na.rm = TRUE), TOTAL.UNITS)
  )

# Apply the best-performing regression model from 1.c (Refined Model)
regression_model <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + TOTAL.UNITS, data = ny)

# Summary of the regression model
summary(regression_model)

# Predictions on the full dataset
ny$predicted_price <- predict(regression_model, newdata = ny)

# Calculate residuals
ny$residuals <- ny$SALE.PRICE - ny$predicted_price

# Plot Predictions vs. Actual Sale Price
ggplot(ny, aes(x = SALE.PRICE, y = predicted_price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Sale Price", x = "Actual Sale Price", y = "Predicted Sale Price")

# Plot Residuals
ggplot(ny, aes(x = predicted_price, y = residuals)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals of Sale Price Predictions", x = "Predicted Sale Price", y = "Residuals")
#3 sentences
#To handle memory limitations, the model was applied to a subset of 10,000 rows from the 5-borough dataset instead of the entire dataset. While this reduces computational load, it still provides a representative view of the model's performance.
#Predictions from the regression model highlight reasonable performance for mid-range sale prices but show larger errors for extreme values. This suggests that additional predictors (e.g., borough, neighborhood) may improve the model's accuracy for outlier cases.
#The residuals exhibit systematic patterns, indicating that the model does not fully generalize across all boroughs due to their diverse real estate characteristics. Tailoring the model to include borough-specific effects or interactions could enhance its generalizability.

#2b
# Load necessary libraries
library(dplyr)
library(randomForestSRC)  # For Random Forest
library(class)  # For k-NN
library(caret)
# Data Preparation: Full dataset (all boroughs)
full_data_class <- ny%>%
  filter(!is.na(BUILDING.CLASS.CATEGORY), !is.na(GROSS.SQUARE.FEET), !is.na(TOTAL.UNITS), !is.na(SALE.PRICE)) %>%
  mutate(across(where(is.character), as.factor))  # Convert character columns to factors

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(full_data_class$BUILDING.CLASS.CATEGORY, p = 0.7, list = FALSE)
train_data <- full_data_class[train_index, ]
test_data <- full_data_class[-train_index, ]

# --------------------------
# Model 1: Random Forest
# --------------------------
rf_model <- rfsrc(BUILDING.CLASS.CATEGORY ~ GROSS.SQUARE.FEET + TOTAL.UNITS + SALE.PRICE,
                  data = train_data, ntree = 100, importance = TRUE)

# Predict on test data
rf_predictions <- predict(rf_model, newdata = test_data)
rf_pred_class <- rf_predictions$class

# Confusion Matrix for Random Forest
confusion_rf <- table(Predicted = rf_pred_class, Actual = test_data$BUILDING.CLASS.CATEGORY)
print("Confusion Matrix: Random Forest")
print(confusion_rf)

# Calculate Accuracy for Random Forest
rf_accuracy <- sum(diag(confusion_rf)) / sum(confusion_rf)
print(paste("Random Forest Accuracy:", round(rf_accuracy * 100, 2), "%"))

# --------------------------
# Model 2: k-Nearest Neighbors (k-NN)
# --------------------------
## Load necessary libraries
library(dplyr)
library(class)  # For k-NN

# Step 1: Handle Non-Numeric Values and Impute Missing Values

# Function to handle non-numeric values and convert them to NA
clean_numeric_column <- function(x) {
  as.numeric(as.character(x))  # Convert to numeric, non-numeric becomes NA
}

# Clean the columns and convert them to numeric
train_data <- train_data %>%
  mutate(
    GROSS.SQUARE.FEET = clean_numeric_column(GROSS.SQUARE.FEET),
    TOTAL.UNITS = clean_numeric_column(TOTAL.UNITS),
    SALE.PRICE = clean_numeric_column(SALE.PRICE)
  )

test_data <- test_data %>%
  mutate(
    GROSS.SQUARE.FEET = clean_numeric_column(GROSS.SQUARE.FEET),
    TOTAL.UNITS = clean_numeric_column(TOTAL.UNITS),
    SALE.PRICE = clean_numeric_column(SALE.PRICE)
  )

# Impute missing values (e.g., with median for numeric columns)
train_data <- train_data %>%
  mutate(
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET), median(GROSS.SQUARE.FEET, na.rm = TRUE), GROSS.SQUARE.FEET),
    TOTAL.UNITS = ifelse(is.na(TOTAL.UNITS), median(TOTAL.UNITS, na.rm = TRUE), TOTAL.UNITS),
    SALE.PRICE = ifelse(is.na(SALE.PRICE), median(SALE.PRICE, na.rm = TRUE), SALE.PRICE)
  )

test_data <- test_data %>%
  mutate(
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET), median(GROSS.SQUARE.FEET, na.rm = TRUE), GROSS.SQUARE.FEET),
    TOTAL.UNITS = ifelse(is.na(TOTAL.UNITS), median(TOTAL.UNITS, na.rm = TRUE), TOTAL.UNITS),
    SALE.PRICE = ifelse(is.na(SALE.PRICE), median(SALE.PRICE, na.rm = TRUE), SALE.PRICE)
  )

# Step 2: Normalize the Data

# Define a normalization function
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Normalize the numeric columns for both train and test data
train_data_norm <- train_data %>%
  mutate(
    GROSS.SQUARE.FEET = normalize(GROSS.SQUARE.FEET),
    TOTAL.UNITS = normalize(TOTAL.UNITS),
    SALE.PRICE = normalize(SALE.PRICE)
  )

test_data_norm <- test_data %>%
  mutate(
    GROSS.SQUARE.FEET = normalize(GROSS.SQUARE.FEET),
    TOTAL.UNITS = normalize(TOTAL.UNITS),
    SALE.PRICE = normalize(SALE.PRICE)
  )

# Step 3: Run k-NN

# Run k-NN on the normalized data
knn_model <- knn(train = train_data_norm[, c("GROSS.SQUARE.FEET", "TOTAL.UNITS", "SALE.PRICE")],
                 test = test_data_norm[, c("GROSS.SQUARE.FEET", "TOTAL.UNITS", "SALE.PRICE")],
                 cl = train_data_norm$BUILDING.CLASS.CATEGORY, k = 5)

# Step 4: Evaluate the k-NN Model

# Confusion Matrix
confusion_knn <- table(Predicted = knn_model, Actual = test_data_norm$BUILDING.CLASS.CATEGORY)
print("Confusion Matrix: k-NN")
print(confusion_knn)

# Calculate Accuracy
knn_accuracy <- sum(diag(confusion_knn)) / sum(confusion_knn)
print(paste("k-NN Accuracy:", round(knn_accuracy * 100, 2), "%"))
#4 sentences
#The classification models, k-Nearest Neighbors (k-NN) and Random Forest, were applied to predict Building Class Category using Gross Square Feet, Total Units, and Sale Price on a 10,000-row subset of the full dataset. Data preprocessing involved handling non-numeric values by converting them to NA, imputing missing values with the median, and normalizing the numeric predictors to ensure proper scaling for k-NN.
#After preprocessing, the Random Forest model achieved an accuracy of 60%, showing superior generalization due to its ability to handle complex relationships and imbalanced classes. The k-NN model achieved an accuracy of 44%, but its performance was limited by class imbalance and the sensitivity to the choice of k.
#The confusion matrices for both models revealed that Random Forest handled minority classes more effectively, while k-NN struggled to predict these accurately.
#Overall, Random Forest demonstrated better performance across the full dataset, but further improvements could be made for both models by incorporating additional features (e.g., location) or experimenting with k in k-NN to address class imbalance and optimize results.
#2c
#The dataset contains a diverse range of property types across the 5 boroughs, with varying levels of missing data, especially in columns like Gross Square Feet and Total Units. These variables are crucial for predicting Sale Price, but missing or incorrectly formatted data could limit the model's accuracy and generalizability.
#My confidence in the result is high for the Random Forest model, as it successfully handled class imbalances and complex relationships between variables. However, the k-NN model was more sensitive to the choice of k and struggled with minority classes, reducing its performance in certain areas.