library(tidyverse)
library(ggplot2)
#reading in the necessary files
populations_2023<-read.csv("countries_populations_2023.csv")
weights<-read.csv("epi2024weights.csv")
results<- read.csv("epi2024results06022024.csv")
#quantile quantile plot
qqnorm(results$EPI.new)
qqline(results$EPI.new)
#qqplot against generating dist
x<- seq(20.,80.,10)
qqplot(qnorm(ppoints(200)),x)
qqline(x)

qqplot(qnorm(ppoints(200)),results$EPI.new)
qqline(results$EPI.new)
#cumulative density function
plot(ecdf(results$EPI.new),do.points=F)
plot(ecdf(rnorm(1000, 45, 10)),do.points=F)
lines(ecdf(results$EPI.new))
###########################################################
#Excersize 1
#making a boxplot comparing BDH.old and BDH.new and EPI.old
boxplot(results$BDH.old, results$BDH.new, results$EPI.old, names=c("BDH.old", "BDH.new", "EPI.new"))
#plotting BDH.old
plot(ecdf(rnorm(10000,50,10)),do.points=F)
lines(ecdf(results$BDH.old))
plot(ecdf(rnorm(results$BDH.old)),do.points=F)
#plotting the BDH.new
plot(ecdf(rnorm(10000,50,10)),do.points=F)
lines(ecdf(results$BDH.new))
plot(ecdf(rnorm(results$BDH.new)),do.points=F)
#plotting EPI.old
plot(ecdf(results$EPI.old),do.points=F)
plot(ecdf(rnorm(1000, 45, 10)),do.points=F)
lines(ecdf(results$EPI.old))
#comparing the BDH.old and the BDH.new
plot(ecdf(results$BDH.new),do.points=F, main='BDH.old vs. BDH.new ECDF')
lines(ecdf(results$BDH.old))
#comparing BDH.old and EPI.old
plot(ecdf(results$BDH.new),do.points=F, main='BDH.old vs. EPI.old ECDF')
lines(ecdf(results$EPI.old))
#comparing BDH.old and EPI.new
plot(ecdf(results$BDH.old),do.points=F, main='BDH.old vs. EPI.new ECDF')
lines(ecdf(results$EPI.new))
##########################################################
#integrating datasets
#making a data frame with only the countries and the populations
# drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% results$country),]
# sort populations by country
populations <- populations[order(populations$Country),]
# drop countries not in populations
epi.results.sub <- results[-which(!results$country %in% populations$Country),]
# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
# only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]
# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)
# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)

#linear models
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)
plot(epi.results.sub$EPI.new~epi.results.sub$population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)

#ggploting
ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
###################################################################################
#Excersize 2
#instead of comparing with epi old and epi new, do it with BDH.new and BDH.old
epi.results.sub2 <- results[-which(!results$country %in% populations$Country),]
# sort epi results by country
epi.results.sub2 <- epi.results.sub2[order(epi.results.sub2$country),]
#keeping the necessary collumns
epi.results.sub2 <- epi.results.sub2[,c("country","BDH.old","BDH.new")]
# convert population to numeric
epi.results.sub2$population <- as.numeric(populations$Population)
# compute population log base 10
epi.results.sub2$population_log <- log10(epi.results.sub2$population)
#lin moding the new stuff
lin.mod.bdhnew<-lm(epi.results.sub2$BDH.new~epi.results.sub2$population_log,epi.results.sub2)
plot(epi.results.sub2$BDH.new~epi.results.sub2$population_log)
abline(lin.mod.bdhnew)
#ggplottind the new stuff
ggplot(epi.results.sub2, aes(x = population_log, y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.bdhnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
#doing the lin mod for the BDH old
#lin moding the old stuff
lin.mod.bdhold<-lm(epi.results.sub2$BDH.old~epi.results.sub2$population_log,epi.results.sub2)
plot(epi.results.sub2$BDH.old~epi.results.sub2$population_log)
abline(lin.mod.bdhold)
#ggplottind the old stuff
ggplot(epi.results.sub2, aes(x = population_log, y = BDH.old)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.bdhold, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')