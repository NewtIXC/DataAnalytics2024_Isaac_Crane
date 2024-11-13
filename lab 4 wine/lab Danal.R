wine<- read.csv("wine.data")
library(ggfortify)
library(e1071)
library(class)
library(psych)
library(tidyverse)

##########################################
### PCA and k-NN on Wine Dataset ###
##########################################

colnames(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", 
                    "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", 
                    "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline")
# Derive target variable based on Type
wine$Type_group <- wine$Type
# Remove unused columns for analysis
wine <- wine[, -1] # Drop Type column (original identifier)
# Plot using pairs.panels with color-coded groups
pairs.panels(wine[,-13], gap = 0, bg = c("red", "yellow", "blue")[wine$Type_group], pch = 21)
# Scatter plot for selected attributes
colnames(wine)
ggplot(wine, aes(x = Alcohol, y = Proline, colour = Type_group)) +
  geom_point()
# Scatter plot for scaled data (centered but not scaled)
ggplot(scale(wine[,-13], scale = FALSE, center = TRUE), aes(x = Alcohol, y = wine$Proline, colour = wine$Type_group)) +
  geom_point()
# Scatter plot for standardized data
ggplot(scale(wine[,-13], scale = TRUE, center = FALSE), aes(x = Alcohol, y = wine$Proline, colour = wine$Type_group)) +
  geom_point()
# Perform PCA with scaling
wine_pca <- prcomp(wine[,-13], center = TRUE, scale. = TRUE)
# Print PCA attributes and summary
attributes(wine_pca)
summary(wine_pca)
wine_pca$rotation
# Plo the principal components
plot(wine_pca)
# Line plot for PCA
plot(wine_pca, type = "l")
# Biplot for PCA components
autoplot(wine_pca, data = wine, colour = 'Type_group', 
         loadings = TRUE, loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3)
# KNN Classifier using all attributes
k <- 65
knn_pred_all <- knn(train = wine[,-13], test = wine[,-13], cl = wine$Type_group, k = k)
# Confusion matrix for all attributes
cm_all <- table(Predicted = knn_pred_all, Actual = wine$Type_group, dnn = list('predicted','actual'))
# Evaluation metrics for all attributes
n <- sum(cm_all)
nc <- nrow(cm_all)
diag_vals <- diag(cm_all)
rowsums <- apply(cm_all, 1, sum)
colsums <- apply(cm_all, 2, sum)
p <- rowsums / n
q <- colsums / n
precision_all <- diag_vals / colsums
recall_all <- diag_vals / rowsums
f1_all <- 2 * precision_all * recall_all / (precision_all + recall_all)
data.frame(recall_all, precision_all, f1_all)
# KNN using first 2 principal components
k <- 65
knn_pred_pca <- knn(train = wine_pca$x[,c(1,2)], test = wine_pca$x[,c(1,2)], cl = wine$Type_group, k = k)
# Confusion matrix for PCA (first 2 PCs)
cm_pca <- table(Predicted = knn_pred_pca, Actual = wine$Type_group, dnn = list('predicted','actual'))
# Evaluation metrics for PCA
precision_pca <- diag(cm_pca) / colsums
recall_pca <- diag(cm_pca) / rowsums
f1_pca <- 2 * precision_pca * recall_pca / (precision_pca + recall_pca)
data.frame(recall_pca, precision_pca, f1_pca)
# Identify least contributing variable to 1st PC and drop it
contributions <- abs(wine_pca$rotation[,1])
least_contributing_var <- names(sort(contributions, decreasing = FALSE))[1]
wine_reduced <- wine[, !(names(wine) == least_contributing_var)]
wine_reduced_scaled <- scale(wine_reduced[,-13])
# Rerun PCA on reduced dataset
wine_pca_reduced <- prcomp(wine_reduced_scaled, center = TRUE, scale. = TRUE)
# KNN using first 3 PCs after dropping least contributing variable
k <- 3
knn_pred_reduced <- knn(train = wine_pca_reduced$x[,1:3], test = wine_pca_reduced$x[,1:3], cl = wine$Type_group, k = k)
# Confusion matrix for reduced PCA
cm_reduced <- table(Predicted = knn_pred_reduced, Actual = wine$Type_group, dnn = list('predicted','actual'))
# Evaluation metrics for reduced PCA
precision_reduced <- diag(cm_reduced) / colsums
recall_reduced <- diag(cm_reduced) / rowsums
f1_reduced <- 2 * precision_reduced * recall_reduced / (precision_reduced + recall_reduced)
data.frame(recall_reduced, precision_reduced, f1_reduced)

