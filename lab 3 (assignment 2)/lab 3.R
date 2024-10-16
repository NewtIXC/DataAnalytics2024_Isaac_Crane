library(tidyverse)
library(dplyr)
library(ggplot2)
#setting the working directory
setwd("~/Data anal")
#reading in the epi dataset
epidemic<- read.csv("epi2024results_DA_F24_lab03.csv")
#attaching the dataframe
attach(epidemic)
library(class)
#getting 2 regions
region_europe <- epidemic %>% filter(region == "Eastern Europe")
region_africa <- epidemic %>% filter(region == "Sub-Saharan Africa")

#setting  as a valiu and glimpsing it 
ECO_eur<- (region_europe$ECO) %>% 
  glimpse()
ECO_afr<- (region_africa$ECO) %>% 
  glimpse()

#seeing if there are any NA's TRUE= there is an NA
tf <- is.na(ECO_eur) %>% 
  glimpse()
tf <- is.na(ECO_afr) %>% 
  glimpse()

#filtering out the NA values if they are presebt
E <- ECO_eur[!tf] %>% 
  glimpse()
A <- ECO_afr[!tf] %>% 
  glimpse()


#taking the summary of the data
summary(ECO_afr)
summary(ECO_eur)
#geting 5 numbers
fivenum(ECO_eur, na.rm = TRUE)
fivenum(ECO_afr, na.rm = TRUE)

#making a histogram
hist(ECO_eur, seq(20.,90.,1),prob=TRUE)
hist(ECO_eur)
lines(density(ECO_eur,na.rm=TRUE,bw='SJ'))
rug(ECO_eur)
hist(ECO_eur, seq(30., 81., 1.0), prob=TRUE)
lines(density(ECO_eur,na.rm=TRUE,bw= "SJ"))
rug(ECO_eur)
#more histograms
hist(ECO_afr, seq(20.,76.,1),prob=TRUE)
hist(ECO_afr)
lines(density(ECO_afr,na.rm=TRUE,bw='SJ'))
rug(ECO_afr)
hist(ECO_afr, seq(20., 90., 1.0), prob=TRUE)
lines(density(ECO_afr,na.rm=TRUE,bw= "SJ"))
rug(ECO_afr)
#Q-Q plot
qqnorm(ECO_eur); qqline(ECO_eur)
qqplot(rt(ppoints(250), df = 2), ECO_eur, xlab = "Q-Q plot for ECO_eur")
qqline(ECO_eur)

qqnorm(ECO_afr); qqline(ECO_afr)
qqplot(rt(ppoints(250), df = 2), ECO_afr, xlab = "Q-Q plot for ECO_afr")
qqline(ECO_afr)
#END OF FIRST PART OF ASSIGNMENT

#########################################################################

#linear models
#chooseing the variables
model<-epidemic %>% 
  select(EPI, H2O, AIR, BDH, ECO,PAR )
#making the lin model
lm<-lm(EPI~H2O + AIR + BDH + ECO + PAR, data = model)
summary(lm)
#lm plot
ggplot(lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
#ECO seems to be to influence epi the most so weel plot tht one
#plotting ECO vs epi
ggplot(model, aes(x = ECO, y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("EPI vs ECO with Fitted Line") +
  theme_minimal()
#doing it with one of the region ones
model2<- region_africa %>% 
  select(EPI, H2O, AIR, BDH, ECO,PAR )
lm2<-lm(EPI~H2O + AIR + BDH + ECO + PAR, data = model2)
summary(lm2)
#eco still is the smallest p value so it influences the data more
ggplot(model2, aes(x = ECO, y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("EPI vs ECO with Fitted Line") +
  theme_minimal()
#the model that had all of the regions and all of their data was better than the region specific one that i did since the R squared value for the all model was much higher than the region specific one. 
#this may be because the less datapoints there are the higher variability there is so the model can change drastically when there is a couple outliers whereas the whole dataset has way more datapoints so its less likely to get thrown out of wack because of a couple outliers.
#END OF PART 2
##################################################################################################################
#knn training
#filtering the data to only have 3 of the regions
EPI3reg<-epidemic %>% 
  filter(region %in% c("Eastern Europe", "Sub-Saharan Africa", "Latin America & Caribbean"))

#filtering the new dataset for it to only have 5 variables
subset<-EPI3reg %>% 
  select(region,EPI, H2O, AIR, ECO,SPI )
subset.norm<-subset
glimpse(subset)
#doing the training
subset.norm$region<-as.factor(subset.norm$region)
glimpse(subset.norm)
#mormalizing
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
subset.norm[1:6]<- as.data.frame(lapply(subset.norm[1:6],normalize))
#this gives a weir error tht doesnt affect anything 
summary(subset.norm)
s_EPI<- sample(97,68)
subset.train <-subset.norm[s_EPI,]
subset.test <-subset.norm[-s_EPI,]
subset.train <-subset[s_EPI, ]
subset.test <-subset[-s_EPI, ]
#setting k=8
sqrt(68)
k= 8
#doing the knn
KNNpred <- knn(train = subset.train[2:6], test = subset.test[2:6], cl = subset.train$region, k = k)
contingency.table <- table(KNNpred,subset.test$region)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(subset.test$region)
accuracy <- c()
ks <- c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,68)
#looping
for (k in ks) {
  KNNpred <- knn(train = subset.train[2:6], test = subset.test[2:6], cl = subset.train$region, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = subset.test$region, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(subset.test$region))
}


plot(ks,accuracy,type = "b", ylim = c(0,1))
#ks of 5 is the most accurate


###########################################################3
#doing it again with 3 other regions
#knn training
#filtering the data to only have 3 of the regions
EPI3reg2<-epidemic %>% 
  filter(region %in% c("Southern Asia", "Greater Middle East", "Global West
"))

#filtering the new dataset for it to only have 5 variables
subset2<-EPI3reg2 %>% 
  select(region,EPI, H2O, AIR, ECO,SPI )
subset2.norm<-subset2
glimpse(subset2)
#doing the training
subset2.norm$region<-as.factor(subset2.norm$region)
glimpse(subset2.norm)
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
subset2.norm[1:6]<- as.data.frame(lapply(subset2.norm[1:6],normalize))
summary(subset2.norm)
s_EPI2<- sample(97,68)
subset2.train <-subset2.norm[s_EPI2,]
subset2.test <-subset2.norm[-s_EPI2,]
subset2.train <-subset2[s_EPI2, ]
subset2.test <-subset2[-s_EPI2, ]
subset2.train<-na.omit(subset2.train)

#setting k=8
sqrt(68)
k= 8
#doing the knn
KNNpred2 <- knn(train = subset2.train[2:6], test = subset2.test[2:6], cl = subset2.train$region, k = k)
contingency2.table <- table(KNNpred2,subset2.test$region)
contingency2.table
contingency2.matrix = as.matrix(contingency2.table)
sum(diag(contingency2.matrix))/length(subset2.test$region)
accuracy <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
for (k in ks) {
  KNNpred2 <- knn(train = subset2.train[2:6], test = subset2.test[2:6], cl = subset2.train$region, k = k)
  cm = as.matrix(table(Actual=KNNpred2, Predicted = subset2.test$region, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(subset2.test$region))
  warnings()
}


plot(ks,accuracy,type = "b", ylim = c(.5,1))
#ks of 1-11  is the most accurate rest go to .87
# The model with the southern asia, global west and middle east are more accurate and thus better than the other model with the other 3 regions.
#This may be because the first 3 regions are in totally different locations whereas the 3 new regions are in a closer area in comparison. 
#END OF PART 3
#############################################################################################################3
#kmeans+wss
#K-means
epi<-epidemic
# Plot epi with eco and ber
epi<-epi %>% 
  filter(region %in% c("Southern Asia", "Greater Middle East", "Asia-Pacific"))
epi<- epi %>% 
  select(region,EPI, H2O, AIR, ECO, BER )


ggplot(epi, aes(x = ECO, y = BER, colour = region)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
glimpse(epi)
epi<- na.omit(epi)
epi.km <- kmeans(epi[, -1], centers = 3)

assigned.clusters <- as.factor(epi.km$cluster)
ggplot(epi, aes(x = ECO, y = BER, colour = assigned.clusters)) +
  geom_point()
wss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
for (k in ks) {
  epi.km <- kmeans(epi[,-1], centers = k)
  wss <- c(wss,epi.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "Southern Asia"
labeled.clusters[labeled.clusters==2] <- "Greater Middle East"
labeled.clusters[labeled.clusters==3] <- "Asia-Pacific"
table(labeled.clusters, epi[,1])
##########################################################
#lets do it again with a nother 3 regions
#kmeans+wss
#K-means
epi<-epidemic
# Plot epi with eco and ber
epi<-epi %>% 
  filter(region %in% c("Former Soviet States", "Global West", "Latin America & Caribbean"))
epi<- epi %>% 
  select(region,EPI, H2O, AIR, ECO, BER )


ggplot(epi, aes(x = ECO, y = BER, colour = region)) +
  geom_point()
# set seed for random number generator
set.seed(123)
# run k-means
glimpse(epi)
epi<- na.omit(epi)
epi.km <- kmeans(epi[, -1], centers = 3)

assigned.clusters <- as.factor(epi.km$cluster)
ggplot(epi, aes(x = ECO, y = BER, colour = assigned.clusters)) +
  geom_point()
wss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
for (k in ks) {
  epi.km <- kmeans(epi[,-1], centers = k)
  wss <- c(wss,epi.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "Former Soviet States"
labeled.clusters[labeled.clusters==2] <- "Global West"
labeled.clusters[labeled.clusters==3] <- "Latin America & Caribbean"
table(labeled.clusters, epi[,1])
#I think the table of the first set of regions is better than the one  for the second set of regions.
#This is because the second set ofr regions graaph isnt as uniform as the firsts theres more random peaks in the second one.

