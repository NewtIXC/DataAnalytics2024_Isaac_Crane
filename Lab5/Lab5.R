# Load the necessary libraries
library(caret)
library(e1071)
library(class)
library(tidyverse)
# Load Wine dataset
wine <- read.csv("wine.data", header = FALSE)
colnames(wine) <- c("Class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium",
                    "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                    "Color_intensity", "Hue", "OD280_OD315", "Proline")


wine$Class <- as.factor(wine$Class)  # Ensure Class is a factor

# Split the dataset into training and testing sets
set.seed(42)
train.indexes <- sample(nrow(wine), 0.7 * nrow(wine))
train <- wine[train.indexes, ]
test <- wine[-train.indexes, ]

# Separate features (x) and class labels (y)
x <- train[, -1]  # Remove the class label column (column 1) for features
y <- train[, 1]   # Column 1 is the class label

# Feature plots (optional for understanding)
featurePlot(x = x, y = y, plot = "box")
featurePlot(x = x, y = y, plot = "density")

# Train SVM with linear kernel
svm_linear <- svm(Class ~ ., data = train, kernel = "linear")

# Predict on train data
train_pred_linear <- predict(svm_linear, train)
cm_linear <- table(Actual = train$Class, Predicted = train_pred_linear)

# Compute confusion matrix metrics (precision, recall, f1)
n <- sum(cm_linear)
nc <- nrow(cm_linear)  # Number of classes
diag <- diag(cm_linear)
rowsums <- apply(cm_linear, 1, sum)
colsums <- apply(cm_linear, 2, sum)

recall <- diag / rowsums
precision <- diag / colsums
f1 <- 2 * precision * recall / (precision + recall)

svm_linear_metrics <- data.frame(precision, recall, f1)

# Print metrics for linear kernel
print("SVM Linear Kernel Metrics:")
print(svm_linear_metrics)

# Train SVM with radial kernel (or polynomial, based on instructions)
svm_radial <- svm(Class ~ ., data = train, kernel = "radial")

# Predict on train data
train_pred_radial <- predict(svm_radial, train)
cm_radial <- table(Actual = train$Class, Predicted = train_pred_radial)

# Compute confusion matrix metrics (precision, recall, f1)
n_radial <- sum(cm_radial)
nc_radial <- nrow(cm_radial)
diag_radial <- diag(cm_radial)
rowsums_radial <- apply(cm_radial, 1, sum)
colsums_radial <- apply(cm_radial, 2, sum)

recall_radial <- diag_radial / rowsums_radial
precision_radial <- diag_radial / colsums_radial
f1_radial <- 2 * precision_radial * recall_radial / (precision_radial + recall_radial)

svm_radial_metrics <- data.frame(precision_radial, recall_radial, f1_radial)

# Print metrics for radial kernel
print("SVM Radial Kernel Metrics:")
print(svm_radial_metrics)

# Train kNN model
knn_mod <- train(Class ~ ., data = train, method = "knn", tuneLength = 10)

# Predict on train data
train_pred_knn <- predict(knn_mod, train)
cm_knn <- table(Actual = train$Class, Predicted = train_pred_knn)

# Compute confusion matrix metrics (precision, recall, f1)
n_knn <- sum(cm_knn)
nc_knn <- nrow(cm_knn)
diag_knn <- diag(cm_knn)
rowsums_knn <- apply(cm_knn, 1, sum)
colsums_knn <- apply(cm_knn, 2, sum)

recall_knn <- diag_knn / rowsums_knn
precision_knn <- diag_knn / colsums_knn
f1_knn <- 2 * precision_knn * recall_knn / (precision_knn + recall_knn)

knn_metrics <- data.frame(precision_knn, recall_knn, f1_knn)

# Print metrics for kNN
print("kNN Metrics:")
print(knn_metrics)

# Test the models on the test set (not just train set)

# Predict for linear kernel on test set
test_pred_linear <- predict(svm_linear, test)
cm_test_linear <- table(Actual = test$Class, Predicted = test_pred_linear)

# Predict for radial kernel on test set
test_pred_radial <- predict(svm_radial, test)
cm_test_radial <- table(Actual = test$Class, Predicted = test_pred_radial)

# Predict for kNN on test set
test_pred_knn <- predict(knn_mod, test)
cm_test_knn <- table(Actual = test$Class, Predicted = test_pred_knn)

# Compute metrics for test set (precision, recall, f1)
n_test_linear <- sum(cm_test_linear)
nc_test_linear <- nrow(cm_test_linear)
diag_test_linear <- diag(cm_test_linear)
rowsums_test_linear <- apply(cm_test_linear, 1, sum)
colsums_test_linear <- apply(cm_test_linear, 2, sum)

recall_test_linear <- diag_test_linear / rowsums_test_linear
precision_test_linear <- diag_test_linear / colsums_test_linear
f1_test_linear <- 2 * precision_test_linear * recall_test_linear / (precision_test_linear + recall_test_linear)

svm_linear_test_metrics <- data.frame(precision_test_linear, recall_test_linear, f1_test_linear)

# Print test set metrics for linear kernel
print("SVM Linear Kernel Test Metrics:")
print(svm_linear_test_metrics)

# Compute metrics for radial kernel test set
n_test_radial <- sum(cm_test_radial)
nc_test_radial <- nrow(cm_test_radial)
diag_test_radial <- diag(cm_test_radial)
rowsums_test_radial <- apply(cm_test_radial, 1, sum)
colsums_test_radial <- apply(cm_test_radial, 2, sum)

recall_test_radial <- diag_test_radial / rowsums_test_radial
precision_test_radial <- diag_test_radial / colsums_test_radial
f1_test_radial <- 2 * precision_test_radial * recall_test_radial / (precision_test_radial + recall_test_radial)

svm_radial_test_metrics <- data.frame(precision_test_radial, recall_test_radial, f1_test_radial)

# Print test set metrics for radial kernel
print("SVM Radial Kernel Test Metrics:")
print(svm_radial_test_metrics)

# Compute metrics for kNN test set
n_test_knn <- sum(cm_test_knn)
nc_test_knn <- nrow(cm_test_knn)
diag_test_knn <- diag(cm_test_knn)
rowsums_test_knn <- apply(cm_test_knn, 1, sum)
colsums_test_knn <- apply(cm_test_knn, 2, sum)

recall_test_knn <- diag_test_knn / rowsums_test_knn
precision_test_knn <- diag_test_knn / colsums_test_knn
f1_test_knn <- 2 * precision_test_knn * recall_test_knn / (precision_test_knn + recall_test_knn)

knn_test_metrics <- data.frame(precision_test_knn, recall_test_knn, f1_test_knn)

# Print test set metrics for kNN
print("kNN Test Metrics:")
print(knn_test_metrics)

#####################################
#ny dataset
# Load NY Housing dataset
housing <- read.csv("NY-House-Dataset.csv")  # Replace with your file path
colnames(housing)
# Split into training and testing sets
set.seed(123)
train.index <- createDataPartition(housing$PRICE, p = 0.7, list = FALSE)
train <- housing[train.index, ]
test <- housing[-train.index, ]

# SVM Regression Model
svm_regressor <- svm(PRICE ~ PROPERTYSQFT, data = train, kernel = "radial", cost = 1, gamma = 0.1)

# Linear Regression Model
linear_regressor <- lm(PRICE ~ PROPERTYSQFT, data = train)

# Predictions
svm_preds <- predict(svm_regressor, test)
linear_preds <- predict(linear_regressor, test)

# Plot predicted vs actual prices
par(mfrow = c(1, 2))
plot(test$PRICE, svm_preds, main = "SVM: Predicted vs Actual", xlab = "Actual Price", ylab = "Predicted Price", col = "blue", pch = 19)
abline(0, 1, col = "red")

plot(test$PRICE, linear_preds, main = "Linear: Predicted vs Actual", xlab = "Actual Price", ylab = "Predicted Price", col = "blue", pch = 19)
abline(0, 1, col = "red")

# Evaluate models with RMSE
svm_rmse <- sqrt(mean((svm_preds - test$PRICE)^2))
linear_rmse <- sqrt(mean((linear_preds - test$PRICE)^2))

cat("SVM RMSE:", svm_rmse, "\n")
cat("Linear Regression RMSE:", linear_rmse, "\n")

