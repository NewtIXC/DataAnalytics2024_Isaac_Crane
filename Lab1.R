#loading the tidyverse package
library(tidyverse)
#set the working directory to my DA file in the computer
setwd("~/Documents/Data Analytics")
#read in the .csv files 
epidemic <- read.csv('epi2024results06022024.csv')
epiother <- read.csv('epi_2024_results_DAF24 - Copy(epi_2024_results_DAF24).csv')

#attaching the dataframe
attach(epidemic)
attach(epiother)
#taking a look at the data editor
fix(epidemic)
fix(epiother)
#epi new is what we are using 
EPI.new

#setting EPI.new as a valiu and glimpsing it 
EPI.new<- (epidemic$EPI.new) %>% 
  glimpse()

#seeing if there are any NA's TRUE= there is an NA
tf <- is.na(EPI.new) %>% 
  glimpse()

#filtering out the NA values if they are presebt
E <- EPI.new[!tf] %>% 
  glimpse()


#taking the summary of the data
summary(EPI.new)
#geting 5 numbers
fivenum(EPI.new, na.rm = TRUE)
#stem and leaf plot
stem(EPI.new)

#making a histogram
hist(EPI.new, seq(20.,80.,1.0),prob=TRUE)
hist(EPI.new)
lines(density(EPI.new,na.rm=TRUE,bw='SJ'))
rug(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw= "SJ"))
rug(EPI.new)
x <- seq(20,80,1)
q<- dnorm(x,mean=42,sd=5,log=F)
lines(x,q)
#the new line goes way to high, making it smaller
lines(x,.4*q)
ln<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*ln)
#making a boxplot
boxplot(EPI.new, APO.new)


#density function
plot(ecdf(EPI.new),do.points=F, verticals=T)
#Q-Q plot
qqnorm(EPI.new); qqline(EPI.new)
qqplot(rt(ppoints(250), df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)



#########################################################################
#again but with a different variable "PFL.new"
EPI.old
#dataframing
PFL.new <- (epidemic$PFL.new)
#seeing if theres NA's 
tf <- is.na(PFL.new) %>% 
  glimpse()
#filtering out the NA's
E <- EPI.new[!tf] %>% 
  glimpse()

#summarize
summary(PFL.new)
fivenum(PFL.new, na.rm = T)
#stemplot
stem(PFL.new)
#histogram
hist(PFL.new)
hist(PFL.new, seq(0.,100.,1),prob=T)
lines(density(PFL.new,na.rm=T,bw=1.))
rug(PFL.new)
hist(PFL.new, seq(0., 100., 1.0), prob=TRUE)
lines (density(PFL.new,na.rm=TRUE,bw=1.)) 
rug(PFL.new)
lines (density(PFL.new,na.rm=TRUE,bw='SJ')) 
x <- seq(0,100,1)
q <- dnorm(x, mean=44, sd=5, log=F)
lines(x,.25*q)
#boxplot 
boxplot(PFL.new, EPI.new)
#plot
plot(ecdf(PFL.new),do.points=F, verticals=T)

#qq plot
qqnorm(PFL.new); qqline(PFL.new)
qqplot(rnorm(ppoints(250)),PFL.new, xlab= "Q-Q plot for norm dist")
