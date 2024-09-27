setwd("~/Documents/Data Analytics")
install.packages("class")
library(tidyverse)
library("e1071")
library(class)
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual')) 
classifier$apriori
classifier$tables$Petal.Length  
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")
######################################################################################
#excersize 1
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),
                    header = FALSE, sep = ",")
classifier2<- naiveBayes(abalone[,1:4], abalone[,5])
table(predict(classifier2, abalone[,-5]), abalone[,5],dnn=list("predicted",'actual'))
classifier2$apriori
classifier2$tables$V1
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="abalone stuff") 
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")


#######################################################################################
#KNN
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 
abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_wieght)
s_abalone <- sample(4177,2924)
abalone.train <-abalone.norm[s_abalone,]
abalone.test <-abalone.norm[-s_abalone,]
abalone.train <-abalone[s_abalone,-1]
abalone.test <-abalone[-s_abalone,-1]
sqrt(2924)
k = 55
 #k = 80
KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
contingency.table <- table(KNNpred,abalone.test$age.group)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(abalone.test$age.group)
accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group))
}
plot(ks,accuracy,type = "b", ylim = c(0.67,0.69))
###############################################################################
#excersize 2
colnames(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
iris.norm <- iris[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
iris.norm[1:3] <- as.data.frame(lapply(iris.norm[1:3], normalize))
summary(iris.norm$Petal.Length)
s_iris <- sample(150,1)
iris.train <-iris.norm[s_iris,]
iris.test <-iris.norm[-s_iris,]
iris.train <-iris[s_iris,-1]
abalone.test <-iris[-s_iris,-1]
sqrt(1)
k = 1
#k = 12
KNNpred <- knn(train = iris.train[1:3], test = iris.test[1:3], cl = iris.train$Petal.Length, k = k)
contingency.table <- table(KNNpred,iris.test$Petal.Length)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Petal.Length)
accuracy <- c()
ks <- c(1,2,3,4,5,6)
for (k in ks) {
  KNNpred <- knn(train = iris.train[1:3], test = iris.test[1:3], cl = iris.train$Petal.Length, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Petal.Length, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Petal.Length))
}
plot(ks,accuracy,type = "b", ylim = c(0,1))



#######################################################################################3
#K-means
library(ggplot2)
# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()
wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])
#######################################################################3
#excersize 3
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = assigned.clusters)) +
  geom_point()
wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])


#for the abalone set
ggplot(abalone, aes(x = V1, y = V2,colour=V9)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
abalone.km <- kmeans(abalone[,-1], centers = 6)
assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone, aes(x = V1, y = V2, colour = assigned.clusters)) +
  geom_point()
wss <- c()
ks <- c(1,2,3,4,5)
for (k in ks) {
  abalone.km <- kmeans(abalone[,-1], centers = k)
  wss <- c(wss,abalone.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, abalone[,5])
