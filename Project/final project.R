#READ IN THE LIBRARIES AND STARY AT ANALYSIS PREP
# the libraries -----------------------------------------------------------
library(randomForest)
library(pbapply)
library(pbmcapply)
library(dendextend)
library(data.table)
library(dplyr)
library(tidyverse)
library(GPUmatrix)
library(tensorflow)
library(vegan)
library(devtools)
library(MASS)
library(parallel)
library(Rtsne)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(corrplot)
library(foreach)
library(randomForestSRC)
library(doParallel)
library(vroom)
library(doSNOW)
library(plyr)
library(foreach)
library(caret)
library(future)
# Reading in the cancer DOnT RUN -----------------------------------------------------
Brain<-vroom("Brain_GSE50161.csv")
Brain2<-vroom("Brain_GSE15824.csv") #DO NOT RUN
# changing the sample name DONT RUN------------------------------------------------
Brain$samples<-"Brain"
Brain2$samples<-"Brain"
 
# merging DONT RUN -----------------------------------------------------------------
Brain_all<- rbind(Brain, Brain2)
any(is.na(Brain_all))
summary(Brain_all)

 #DO NOT RUN
# ANALYSIS, prep START HERE ---------------------------------------------------
num_cores <- detectCores() - 10  # Number of cores to use  (CHANGE IF YOU HAVE A WEAKER SYSTEM)
cl <- makeCluster(num_cores)  # Create a cluster with the available cores
registerDoParallel(cl) 
# no run write.csv(Brain_all, "Brain")
Brain_all<-vroom("Brain") #I WILL SUPPLY THIS FILE



#subset the data for the numeric colluns
numeric_brain <- Brain_all[sapply(Brain_all, is.numeric)]
#making a stem and leaf plot, this makes one for every collumn in the dataset
lapply(numeric_brain, stem) #stop this after a few seconds


#removing the sample numbers
Brain_all3<-Brain_all[, -(1)]
#subsetting for the different status
normal_data <- subset(Brain_all3, type == "normal")
ependymoma_data <- subset(Brain_all3, type == "ependymoma") 
Astrocytoma_data<- subset(Brain_all3, type == "Astrocytoma")
Brain_all3$type <- gsub("glioblastoma-cell-line", "glioblastoma", Brain_all3$type)
Brain_all3$type <- gsub("Glioblastoma", "glioblastoma", Brain_all3$type)
glioblastoma_data<-subset(Brain_all3, type == "glioblastoma")
medulloblastoma_data<-subset(Brain_all3, type == "medulloblastoma")
pilocytic_astrocytoma<-subset(Brain_all3, type == "pilocytic_astrocytoma")
Oligodendrioglioma<-subset(Brain_all3, type == "Oligodendrioglioma")
 #START HERE  
# histograms --------------------------------------------------------------
#making a dataframe for each of the subsets
# Histogram for Astrocytoma_data
numeric_data_astrocytoma <- unlist(Astrocytoma_data[sapply(Astrocytoma_data, is.numeric)])
mean_astrocytoma <- mean(numeric_data_astrocytoma, na.rm = TRUE)
sd_astrocytoma <- sd(numeric_data_astrocytoma, na.rm = TRUE)

hist(numeric_data_astrocytoma, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Astrocytoma_data",
     xlab = "Expression Values",
     ylab = "Frequency")
# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_astrocytoma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)

# Normal distribution curve
mean_astrocytoma <- mean(numeric_data_astrocytoma, na.rm = TRUE)
sd_astrocytoma <- sd(numeric_data_astrocytoma, na.rm = TRUE)
x <- seq(min(numeric_data_astrocytoma, na.rm = TRUE), 
         max(numeric_data_astrocytoma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_astrocytoma, sd = sd_astrocytoma) * length(numeric_data_astrocytoma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2)  # Add normal curve (blue dashed line)
# Add the rug plot for individual data points
rug(numeric_data_astrocytoma)




# Histogram for Ependymoma_data
numeric_data_ependymoma <- unlist(ependymoma_data[sapply(ependymoma_data, is.numeric)])
mean_ependymoma <- mean(numeric_data_ependymoma, na.rm = TRUE)
sd_ependymoma <- sd(numeric_data_ependymoma, na.rm = TRUE)
hist(numeric_data_ependymoma, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Ependymoma_data",
     xlab = "Expression Values",
     ylab = "Frequency")
# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_ependymoma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)

# Normal distribution curve
mean_ependymoma <- mean(numeric_data_ependymoma, na.rm = TRUE)
sd_ependymoma <- sd(numeric_data_ependymoma, na.rm = TRUE)
x <- seq(min(numeric_data_ependymoma, na.rm = TRUE), 
         max(numeric_data_ependymoma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_ependymoma, sd = sd_ependymoma) * length(numeric_data_ependymoma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_ependymoma)



# Histogram for Normal_data
numeric_data_normal <- unlist(normal_data[sapply(normal_data, is.numeric)])
mean_normal <- mean(numeric_data_normal, na.rm = TRUE)
sd_normal <- sd(numeric_data_normal, na.rm = TRUE)

hist(numeric_data_normal, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Normal_data",
     xlab = "Expression Values",
     ylab = "Frequency")
# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_normal, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)

# Normal distribution curve
mean_normal <- mean(numeric_data_normal, na.rm = TRUE)
sd_normal <- sd(numeric_data_normal, na.rm = TRUE)
x <- seq(min(numeric_data_normal, na.rm = TRUE), 
         max(numeric_data_normal, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_normal, sd = sd_normal) * length(numeric_data_normal) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_normal)



# Histogram for Glioblastoma_data
numeric_data_glioblastoma <- unlist(glioblastoma_data[sapply(glioblastoma_data, is.numeric)])
mean_glioblastoma <- mean(numeric_data_glioblastoma, na.rm = TRUE)
sd_glioblastoma <- sd(numeric_data_glioblastoma, na.rm = TRUE)


hist(numeric_data_glioblastoma, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Glioblastoma_data",
     xlab = "Expression Values",
     ylab = "Frequency")


# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_glioblastoma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)

# Add the glioblastoma distribution curve with calculated mean and standard deviation
# Normal distribution curve
mean_glioblastoma <- mean(numeric_data_glioblastoma, na.rm = TRUE)
sd_glioblastoma <- sd(numeric_data_glioblastoma, na.rm = TRUE)
x <- seq(min(numeric_data_glioblastoma, na.rm = TRUE), 
         max(numeric_data_glioblastoma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_glioblastoma, sd = sd_glioblastoma) * length(numeric_data_glioblastoma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_glioblastoma)



# Histogram for Medulloblastoma_data
numeric_data_medulloblastoma <- unlist(medulloblastoma_data[sapply(medulloblastoma_data, is.numeric)])
mean_medulloblastoma <- mean(numeric_data_medulloblastoma, na.rm = TRUE)
sd_medulloblastoma <- sd(numeric_data_medulloblastoma, na.rm = TRUE)

hist(numeric_data_medulloblastoma, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Medulloblastoma_data",
     xlab = "Expression Values",
     ylab = "Frequency")


# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_medulloblastoma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)


# Normal distribution curve
mean_medulloblastoma <- mean(numeric_data_medulloblastoma, na.rm = TRUE)
sd_medulloblastoma <- sd(numeric_data_medulloblastoma, na.rm = TRUE)
x <- seq(min(numeric_data_medulloblastoma, na.rm = TRUE), 
         max(numeric_data_medulloblastoma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_medulloblastoma, sd = sd_medulloblastoma) * length(numeric_data_medulloblastoma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_medulloblastoma)




# Histogram for Oligodendrioglioma
numeric_data_oligodendrioglioma <- unlist(Oligodendrioglioma[sapply(Oligodendrioglioma, is.numeric)])
mean_oligodendrioglioma <- mean(numeric_data_oligodendrioglioma, na.rm = TRUE)
sd_oligodendrioglioma <- sd(numeric_data_oligodendrioglioma, na.rm = TRUE)

hist(numeric_data_oligodendrioglioma,
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Oligodendrioglioma",
     xlab = "Expression Values",
     ylab = "Frequency")


# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_oligodendrioglioma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)



# Normal distribution curve
mean_oligodendrioglioma <- mean(numeric_data_oligodendrioglioma, na.rm = TRUE)
sd_oligodendrioglioma <- sd(numeric_data_oligodendrioglioma, na.rm = TRUE)
x <- seq(min(numeric_data_oligodendrioglioma, na.rm = TRUE), 
         max(numeric_data_oligodendrioglioma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_oligodendrioglioma, sd = sd_oligodendrioglioma) * length(numeric_data_oligodendrioglioma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_oligodendrioglioma)



# Histogram for Pilocytic_Astrocytoma
numeric_data_pilocytic_astrocytoma <- unlist(pilocytic_astrocytoma[sapply(pilocytic_astrocytoma, is.numeric)])
mean_pilocytic_astrocytoma <- mean(numeric_data_pilocytic_astrocytoma, na.rm = TRUE)
sd_pilocytic_astrocytoma <- sd(numeric_data_pilocytic_astrocytoma, na.rm = TRUE)


hist(numeric_data_pilocytic_astrocytoma, 
     breaks = seq(0, 15, 1),  # Adjust number of bins as needed
     col = "lightblue", 
     border = "black",
     main = "Histogram of Numeric Values for Pilocytic_Astrocytoma",
     xlab = "Expression Values",
     ylab = "Frequency")


# Add the density curve using Sheather-Jones bandwidth selector
lines(density(numeric_data_pilocytic_astrocytoma, na.rm = TRUE, bw = "SJ"), 
      col = "darkred",        # Color of the density line
      lwd = 2,                # Line width
      lty = 1)                # Line type (solid)



# Normal distribution curve
mean_pilocytic_astrocytoma <- mean(numeric_data_pilocytic_astrocytoma, na.rm = TRUE)
sd_pilocytic_astrocytoma <- sd(numeric_data_pilocytic_astrocytoma, na.rm = TRUE)
x <- seq(min(numeric_data_pilocytic_astrocytoma, na.rm = TRUE), 
         max(numeric_data_pilocytic_astrocytoma, na.rm = TRUE), 
         length.out = 100)
bin_width <- diff(seq(0, 15, 1))[1]
q <- dnorm(x, mean = mean_pilocytic_astrocytoma, sd = sd_pilocytic_astrocytoma) * length(numeric_data_pilocytic_astrocytoma) * bin_width
lines(x, q, col = "blue", lwd = 2, lty = 2) 
rug(numeric_data_pilocytic_astrocytoma)

# qq- plots ---------------------------------------------------------------
#qq plots of each of the subsets
#qqplot astro
qqnorm(numeric_data_astrocytoma, main = "Q-Q Plot for astrocytoma Data")
qqline(numeric_data_astrocytoma, col = "red")

#qqplot epend
qqnorm(numeric_data_ependymoma, main = "Q-Q Plot for ependymoma Data")
qqline(numeric_data_ependymoma, col = "red")

#qq plot  glio
qqnorm(numeric_data_glioblastoma, main = "Q-Q Plot for glioblastoma Data")
qqline(numeric_data_glioblastoma, col = "red")

#medullo qq plot
qqnorm(numeric_data_medulloblastoma, main = "Q-Q Plot for medulloblastoma Data")
qqline(numeric_data_medulloblastoma, col = "red")

#qq normal
qqnorm(numeric_data_normal, main = "Q-Q Plot for normal Data")
qqline(numeric_data_normal, col = "red")

#qq oligo
qqnorm(numeric_data_oligodendrioglioma, main = "Q-Q Plot for oligodendrioglioma Data")
qqline(numeric_data_oligodendrioglioma, col = "red")

#qq pilo
qqnorm(numeric_data_pilocytic_astrocytoma, main = "Q-Q Plot for pilocytic_astrocytoma Data")
qqline(numeric_data_pilocytic_astrocytoma, col = "red")

# Boxplots ----------------------------------------------------------------
# Boxplot for comparing different subsets
#converting the subsets to dataframes
numeric_data_astrocytomadf<- as.data.frame(numeric_data_astrocytoma)
numeric_data_ependymomadf<- as.data.frame(numeric_data_ependymoma)
numeric_data_glioblastomadf<- as.data.frame(numeric_data_glioblastoma)
numeric_data_medulloblastomadf<- as.data.frame(numeric_data_medulloblastoma)
numeric_data_normaldf<- as.data.frame(numeric_data_normal)
numeric_data_Oligodendriogliomadf<- as.data.frame(numeric_data_oligodendrioglioma)
numeric_data_pilocytic_astrocytomadf<- as.data.frame(numeric_data_pilocytic_astrocytoma)

#comparison of boxplots, the light pink one is oligodendrioglioma
boxplot(
  numeric_data_astrocytomadf$numeric_data_astrocytoma, 
  numeric_data_ependymomadf$numeric_data_ependymoma, 
  numeric_data_glioblastomadf$numeric_data_glioblastoma, 
  numeric_data_medulloblastomadf$numeric_data_medulloblastoma, 
  numeric_data_Oligodendriogliomadf$numeric_data_oligodendrioglioma, 
  numeric_data_pilocytic_astrocytomadf$numeric_data_pilocytic_astrocytoma, 
  numeric_data_normaldf$numeric_data_normal,
  names = c("Astrocytoma", "Ependymoma", "Glioblastoma", "Medulloblastoma", 
            "oligodendrioglioma", "Pilocytic Astrocytoma", "Normal"),
  main = "Comparison of Expression Across Subsets",
  col = c("lightblue", "lightgreen", "lightcoral", "lightyellow", 
          "lightpink", "lightgray", "lightcyan")
)




# Linear models -----------------------------------------------------------
Brain2<- vroom("Brain")
brain_all2<-Brain2[, -(1)]

#pivoting long
# Remove the 'clusters' column before pivoting
long_brain <- brain_all2 %>%
  pivot_longer(cols = -c(samples, type),  # Keep Sample and Type columns
               names_to = "Gene",       # Gene names will be in the 'Gene' column
               values_to = "Expression")  # Gene expression values will be in 'Expression'
#subsetting again
normal_data2 <- subset(long_brain, type == "normal")
ependymoma_data2 <- subset(long_brain, type == "ependymoma") 
Astrocytoma_data2<- subset(long_brain, type == "Astrocytoma")
long_brain$type <- gsub("glioblastoma-cell-line", "glioblastoma", long_brain$type)
long_brain$type <- gsub("Glioblastoma", "glioblastoma", long_brain$type)
glioblastoma_data2<-subset(long_brain, type == "glioblastoma")
medulloblastoma_data2<-subset(long_brain, type == "medulloblastoma")
pilocytic_astrocytoma2<-subset(long_brain, type == "pilocytic_astrocytoma")
Oligodendrioglioma2<-subset(long_brain, type == "Oligodendrioglioma")

# Simple linear model
long_brain$type<- as.factor(long_brain$type)
simple_model <- lm(Expression ~ type, data = long_brain)
plot(long_brain$Expression ~ long_brain$type, main = "Expression by Type")
abline(simple_model, col = "red")

# Number of genes to plot (change this if you want to see more plots)
num_genes_to_plot <- 1

# Loop through the first few genes were doing 10 because 50k would be time consuming
unique_genes <- unique(long_brain$Gene)  # Get all unique genes

for (i in 1:num_genes_to_plot) {
  # Subset data for the current gene
  gene_name <- unique_genes[i]
  gene_data <- subset(long_brain, Gene == gene_name)
  
  # Fit linear model
  lin_model <- lm(Expression ~ type, data = gene_data)
  
  # Scatterplot of expression vs type
  plot(as.numeric(gene_data$type), gene_data$Expression,
       main = paste("Scatterplot for Gene:", gene_name),
       xlab = "Sample Type (as numeric)", ylab = "Expression",
       pch = 19, col = "blue")
  
  # Add regression line
  abline(lin_model, col = "red", lwd = 2)
  
  # Residuals vs Fitted values
  plot(lin_model$fitted.values, lin_model$residuals,
       main = paste("Residuals vs Fitted for Gene:", gene_name),
       xlab = "Fitted Values", ylab = "Residuals",
       pch = 19, col = "darkgreen")
  abline(h = 0, col = "red", lwd = 2)
  
  # Q-Q Plot of residuals
  qqnorm(lin_model$residuals,
         main = paste("Q-Q Plot for Gene:", gene_name))
  qqline(lin_model$residuals, col = "red", lwd = 2)
  
  # Histogram of residuals
  hist(lin_model$residuals,
       main = paste("Histogram of Residuals for Gene:", gene_name),
       xlab = "Residuals", col = "lightblue", breaks = 20)
}




# DIMENTION REDUCTION -----------------------------------------------------
#NMDS
# Extract numeric gene expression data for NMDS
nmds_brain <- Brain_all[, -(1:3)]

nmds_brain_result <- metaMDS(
  nmds_brain,
  k = 3, 
  trymax = 100, 
  autotransform = FALSE, 
  distance = "euclidean"
)
# Extract NMDS results
nmds_points <- data.frame(nmds_brain_result$points) %>%
  mutate(sample = Brain_all$samples, type = Brain_all$type)

# Plot NMDS with ggplot2
ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = type)) +
  geom_point(size = 2) +
  labs(title = "NMDS Plot", x = "NMDS1", y = "NMDS2") +
  theme_minimal()
#insufficient data, too perfect?

#correspondance analysis
ca_result <- CA(nmds_brain, graph = FALSE)

# Summary of CA
summary(ca_result)

# Visualize CA
fviz_ca_biplot(ca_result,
               map = "symbiplot",
               arrow = c(TRUE, FALSE),
               repel = TRUE) +
  ggtitle("Correspondence Analysis Biplot")


#PCA
pca_result <- prcomp(nmds_brain, center = TRUE, scale. = TRUE)

# PCA summary
summary(pca_result)

# Scree plot
fviz_eig(pca_result)

# PCA biplot
fviz_pca_biplot(pca_result, 
                repel = TRUE, 
                col.var = "blue", 
                col.ind = Brain_all$type) +
  ggtitle("PCA Biplot")

# MACHINE LEARNING --------------------------------------------------------
# KMEANS ------------------------------------------------------------------
#kmeans
Brain_num <- Brain_all[, -(1:3)]

# Scale the data (standardizing)
brain_num_scaled <- scale(Brain_num)

# Set a random seed for reproducibility
set.seed(123)

# Perform K-means clustering with 3 clusters 
kmeans_result <- kmeans(brain_num_scaled, centers = 4)

# Print the size of each cluster and the cluster centers
kmeans_result$size
kmeans_result$centers

# Perform PCA for dimensionality reduction
pca_result <- prcomp(brain_num_scaled)
pca_data <- data.frame(pca_result$x)

# Add the cluster results to the PCA data
pca_data$cluster <- as.factor(kmeans_result$cluster)

# Plot the first two principal components with cluster information
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 4) +
  theme_bw(base_size = 18) +
  labs(title = "K-means Clustering on Brain Data (PCA)", x = "PC1", y = "PC2")

# Total within-cluster sum of squares
kmeans_result$tot.withinss

# Calculate the gap statistic (for optimal number of clusters)
library(cluster)

# Define the bootstrap k-means function
bootstrap_kmeans_parallel <- function(data, k, B = 50, ncores = 20) {
  
  # Register parallel backend
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # Run the parallel loop with progress monitoring using pbmcapply
  inertia_values <- foreach(b = 1:B, .combine = 'c', .packages = 'stats') %dopar% {
    # Perform k-means clustering on the bootstrap sample
    set.seed(b)  # Set a random seed for reproducibility
    sample_indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    bootstrap_sample <- data[sample_indices, ]
    
    # Run k-means on the bootstrap sample
    kmeans_result <- kmeans(bootstrap_sample, centers = k, nstart = 25)
    
    # Return the inertia (within-cluster sum of squares) for this sample
    kmeans_result$tot.withinss
  }
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  return(inertia_values)
}

# Example usage
# Set the number of bootstrap samples (B) and number of clusters (k)
B <- 50  # Number of bootstrap samples
k <- 7   # Number of clusters for k-means
ncores <- 20  # Number of cores for parallel processing (adjust based on your system)

# Run the parallelized bootstrap k-means
inertia_values <- bootstrap_kmeans_parallel(brain_num_scaled, k, B, ncores)

# Visualize the gap statistic
print(inertia_values)
# Convert the inertia values into a data frame for easier plotting
inertia_df <- data.frame(inertia = inertia_values)

# Plot the distribution of inertia values
ggplot(inertia_df, aes(x = inertia)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of Inertia (Within-Cluster Sum of Squares)",
       x = "Inertia (Within-Cluster SS)",
       y = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Hierarchical clustering -------------------------------------------------
# Filter/select the relevant columns for clustering (expression data)
brain_all_use <-Brain_all[, -(1:3)]
# Scale the data
brain_all_scaled <- scale(brain_all_use)

# Compute the distance matrix
brain_all_dist <- dist(brain_all_scaled)

# Perform hierarchical clustering (using 'ward.D2' method)
brain_all_clust <- hclust(brain_all_dist, method = 'ward.D2')

# Plot the dendrogram
plot(brain_all_clust, main = "Hierarchical Clustering Dendrogram", xlab = "", ylab = "Height")

# Color branches for better visualization
brain_all_dend <- as.dendrogram(brain_all_clust)
brain_all_colored <- color_branches(brain_all_dend, k = 7)  # Choose k for clusters
plot(brain_all_colored)

# Cut the tree into 5 clusters
brain_all_clusters <- cutree(brain_all_clust, k = 7)
Brain_all$clusters <- factor(brain_all_clusters)

# Check the table of clusters
table(Brain_all$type, Brain_all$clusters)

# Visualization of clusters across 'type'
ggplot(Brain_all, aes(x = type, fill = clusters)) +
  geom_bar() +
  facet_wrap(~ clusters) +
  theme_minimal() +
  labs(title = "Cluster Distribution by Type")


ggplot(long_brain, aes(x = factor(Brain_all$clusters[match(long_brain$samples, Brain_all$samples)]), y = Expression)) +
  geom_boxplot() +
  facet_wrap(~ type, scales = "free") +
  theme_minimal() +
  labs(title = "Cluster Comparisons by Type")
# Comparing methods
# apply other distance methods like Manhattan, Euclidean, etc.
brain_all_dist_manhattan <- dist(brain_all_scaled, method = 'manhattan')
brain_all_clust_manhattan <- hclust(brain_all_dist_manhattan, method = 'ward.D2')
plot(brain_all_clust_manhattan)
brain_all_dist_euc <- dist(brain_all_scaled, method = 'euclidean')
brain_all_clust_euc <- hclust(brain_all_dist_manhattan, method = 'ward.D2')
plot(brain_all_clust_euc)
# Tanglegram for comparing different clustering methods

brain_all_clust_single <- hclust(brain_all_dist, method = 'single')
brain_all_clust_complete <- hclust(brain_all_dist, method = 'complete')

# Plot tanglegram to compare the dendrograms
tanglegram(as.dendrogram(brain_all_clust), as.dendrogram(brain_all_clust_complete))
# Logistic Regression -----------------------------------------------------
#starting with the scaled data from earl;ier
# Add the 'type' or any other binary outcome column for regression (e.g., strat)
brain_all_scaled <- cbind(brain_all_scaled, type = Brain_all$type)
brain_all_scaled<-as.data.frame(brain_all_scaled)
# Check the data
head(brain_all_scaled)

# Set seed for reproducibility
set.seed(100)

# Split into training (70%) and testing (30%)
train_index <- sample(1:nrow(brain_all_scaled), size = floor(0.7 * nrow(brain_all_scaled)), replace = FALSE)

# Create training and testing datasets
training <- brain_all_scaled[train_index, ]
testing <- brain_all_scaled[-train_index, ]
# Perform PCA to reduce dimensionality
Brain2 <- brain_all2[, -(1:3)]

# Scale the data (standardizing)
brain2_scaled <- scale(Brain2)
pca <- prcomp(brain2_scaled, center = TRUE, scale. = TRUE)
summary(pca)
# Keep only a few principal components (e.g., first 100)
brain_all_pca <- data.frame(pca$x[, 1:109], type = brain_all_scaled$type)

# Load necessary package
install.packages("nnet")
library(nnet)

# Fit a multinomial logistic regression model
multinom_model <- multinom(type ~ ., data = brain_all_pca)

# View model summary
summary(multinom_model)

# Predict on new data (e.g., training set or test set)
predictions <- predict(multinom_model, brain_all_pca)

# Create a confusion matrix to evaluate the model
cf<- table(Predicted = predictions, Actual = brain_all_pca$type)
cf
accuracycf <- sum(diag(cf)) / sum(cf)
print(paste("Accuracy:", accuracy))

# KNN ---------------------------------------------------------------------
# Load necessary libraries
library(class)


# Encode 'type' as a factor (if not already)
brain_all_scaled$type <- as.factor(brain_all_scaled$type)

# Split the data into training and testing sets
set.seed(3333)
train_index <- createDataPartition(brain_all_scaled$type, p = 0.7, list = FALSE)
brain_train <- brain_all_scaled[train_index, ]
brain_test <- brain_all_scaled[-train_index, ]

# Prepare training and testing data
train_features <- brain_train[, -(54676)]
train_labels <- brain_train$type
test_features <- brain_test[, -(54676)]
test_labels <- brain_test$type

# Perform KNN
k <- 8  # Choose a value for k
knn_predictions <- knn(train = train_features, 
                       test = test_features, 
                       cl = train_labels, 
                       k = k, 
                       prob = TRUE)

# Confusion Matrix
confusion_matrix <- table(test_labels, knn_predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Optimize K using a loop
k_values <- seq(1, 25, 2)  # Test odd values of k
accuracy_values <- sapply(k_values, function(k) {
  knn_pred <- knn(train = train_features, 
                  test = test_features, 
                  cl = train_labels, 
                  k = k)
  sum(knn_pred == test_labels) / length(test_labels)
})

# Create a data frame for visualization
accuracy_df <- data.frame(k = k_values, accuracy = accuracy_values)

# Plot accuracy vs k
ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_point(color = "orchid") +
  geom_line(color = "orchid4") +
  labs(title = "KNN Accuracy vs K",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()



# Neural Networks ---------------------------------------------------------
# Select numeric columns as predictors (you might need to select your numeric columns manually)
install.packages("neuralnet")
library(neuralnet)
Brain_all3<-vroom("Brain")

brain_all_pca_nn <- data.frame(pca$x, type = brain_all_scaled$type)
# Split the data into training and testing sets
#use one of the pca's from before 
set.seed(123)
train_index <- createDataPartition(brain_all_pca_nn$type, p = 0.7, list = FALSE)
train_set <- brain_all_pca_nn[train_index, ]
test_set <- brain_all_pca_nn[-train_index, ]

# Neural network model: The 'family' column will be the response, and predictors are other numeric columns
nn_model <- neuralnet(type ~ ., 
                      data = train_set, 
                      hidden = c(10, 10),  # You can adjust the number of hidden layers and neurons as needed
                      linear.output = FALSE,  # Since we're doing classification, not regression
                      threshold = 0.01, 
                      stepmax = 1e+08)

# Visualize the neural network (this will lag)
plot(nn_model)

# Make predictions on the test set

test_features <-test_set[, -(168)]  # Exclude 'type' column from features

nn_predictions <- predict(nn_model, test_features)

# Convert predictions to factor
predicted_class <- apply(nn_predictions, 1, which.max)  # Find the index of the max value
predicted_class <- factor(predicted_class, levels = 1:length(levels(test_set$type)))

# Confusion matrix
cm <- table(Predicted = predicted_class, Actual = test_set$type)
print(cm)

# Calculate accuracy
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy: ", accuracy))

# Random Forest -----------------------------------------------------------
#grabbing a pca
pcarf<- data.frame(pca$x, type = brain_all_scaled$type)
pcarf$type<-as.factor(pcarf$type)
rf_brainsrc <- rfsrc(type ~ ., data = pcarf, importance = TRUE)
rf_brain <- randomForest(type ~ ., data = pcarf, importance = TRUE)

# Step 3: Print the model summary
print(rf_brainsrc)
print(rf_brain)
# Step 4: Plotting the Random Forest model
# Plot the first tree
plot(get.tree(rf_brainsrc, 1))
varImpPlot(rf_brain)
# Step 5: Variable Importance Plot
plot(rf_brainsrc)

# Step 6: Get Importance Scores
imp_score <- importance(rf_brain)

# Convert the importance scores to a data frame for plotting
imp_score_df <- data.frame(
  Feature = rownames(imp_score),
  MeanDecreaseGini = as.numeric(imp_score)
)

# Step 7: Plot the variable importance using ggplot
ggplot(imp_score_df, aes(x = reorder(Feature, MeanDecreaseGini), 
                         y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Features", y = "Mean Decrease in Gini") +
  theme_minimal()



#This is all of the code, start at the analysis prep since i will give the csv for the combined 2 datasets, also is very computaiopnal
#so u might need to run in segments
