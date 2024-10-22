library(tidyverse)
library(ggplot2)
#QUESTION 1
#PART a
#reading in the country datasets
us2022<- read.csv("us-counties-2022.csv")
us2023<- read.csv("us-counties-2023.csv")
house<- read.csv("NY-House-Dataset.csv")
#taking out any NA's that may be present
us2022<-na.omit(us2022)
us2023<-na.omit(us2023)
#since this is for the deaths and cases lets make dataframs with just those collumns
us2022n<-us2022[5:6]
us2023n<-us2023[5:6]

#lets see if we take out one state and re do the analysis
us2022NY<-subset(us2022, state=="New York")
us2023NY<-subset(us2023, state=="New York")
#since this is for the deaths and cases lets make dataframs with just those collumns
us2022n<-us2022[5:6]
us2023n<-us2023[5:6]
# Boxplot for 'cases' (2022)
ggplot(us2022n, aes(y = cases)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Boxplot of Cases (2022)", x = "", y = "Cases") +
  theme_minimal()

# Boxplot for 'cases' (2023)
ggplot(us2023n, aes(y = cases)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot of Cases (2023)", x = "", y = "Cases") +
  theme_minimal()

# Boxplot for 'deaths' (2022)
ggplot(us2022n, aes(y = deaths)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Boxplot of Deaths (2022)", x = "", y = "Deaths") +
  theme_minimal()

# Boxplot for 'deaths' (2023)
ggplot(us2023n, aes(y = deaths)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot of Deaths (2023)", x = "", y = "Deaths") +
  theme_minimal()

# Summary statistics for 2022
summary_2022_cases <- summary(us2022n$cases)
summary_2022_deaths <- summary(us2022n$deaths)
print("Summary Statistics for Cases (2022)")
print(summary_2022_cases)
print("Summary Statistics for Deaths (2022)")
print(summary_2022_deaths)

# Summary statistics for 2023
summary_2023_cases <- summary(us2023n$cases)
summary_2023_deaths <- summary(us2023n$deaths)
print("Summary Statistics for Cases (2023)")
print(summary_2023_cases)
print("Summary Statistics for Deaths (2023)")
print(summary_2023_deaths)
#lets see if we take out one state and re do the analysis
us2022NY<-subset(us2022, state=="New York")
us2023NY<-subset(us2023, state=="New York")
#repeating for the NY subsets
# Boxplot for 'cases' (2022)
ggplot(us2022NY, aes(y = cases)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Boxplot of Cases (2022)", x = "", y = "Cases") +
  theme_minimal()

# Boxplot for 'cases' (2023)
ggplot(us2023NY, aes(y = cases)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot of Cases (2023)", x = "", y = "Cases") +
  theme_minimal()

# Boxplot for 'deaths' (2022)
ggplot(us2022NY, aes(y = deaths)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Boxplot of Deaths (2022)", x = "", y = "Deaths") +
  theme_minimal()

# Boxplot for 'deaths' (2023)
ggplot(us2023NY, aes(y = deaths)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot of Deaths (2023)", x = "", y = "Deaths") +
  theme_minimal()

# Summary statistics for 2022
summary_2022NY_cases <- summary(us2022NY$cases)
summary_2022NY_deaths <- summary(us2022NY$deaths)
print("Summary Statistics for Cases (2022)")
print(summary_2022_cases)
print("Summary Statistics for Deaths (2022)")
print(summary_2022NY_deaths)

# Summary statistics for 2023
summary_2023NY_cases <- summary(us2023NY$cases)
summary_2023NY_deaths <- summary(us2023n$deaths)
print("Summary Statistics for Cases (2023)")
print(summary_2023NY_cases)
print("Summary Statistics for Deaths (2023)")
print(summary_2023NY_deaths)
#The 2 datasets I picked were the 2022 and 2023 covid data and made box plots based off of the cases and the deaths in both datasets.
#There are a lot of outliers in the data with the minimums being 1 and 0 and max for cases being 3632440 and the max for the deaths being 34671.
#I then did it again just with the NY state portion of the data and it looks better but there is still alot of outliers the max for cases here is570676 and the deaths are 5627.
#END OF PART a
#PART b
#making histograms for the nonfiltered datasets cases
# Histogram for 'cases' (2022) with normal fit
ggplot(us2022n, aes(x = cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2022n$cases), sd = sd(us2022n$cases)), color = "red") +
  labs(title = "Histogram of Cases (2022) with Normal Fit", x = "Cases", y = "Density") +
  theme_minimal()

# Histogram for 'cases' (2023) with normal fit
ggplot(us2023n, aes(x = cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "orange", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2023n$cases), sd = sd(us2023n$cases)), color = "red") +
  labs(title = "Histogram of Cases (2023) with Normal Fit", x = "Cases", y = "Density") +
  theme_minimal()

# Histogram for 'deaths' (2022) with normal fit
ggplot(us2022n, aes(x = deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2022n$deaths), sd = sd(us2022n$deaths)), color = "red") +
  labs(title = "Histogram of Deaths (2022) with Normal Fit", x = "Deaths", y = "Density") +
  theme_minimal()

# Histogram for 'deaths' (2023) with normal fit
ggplot(us2023n, aes(x = deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "orange", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2023n$deaths), sd = sd(us2023n$deaths)), color = "red") +
  labs(title = "Histogram of Deaths (2023) with Normal Fit", x = "Deaths", y = "Density") +
  theme_minimal()

#now for the NY filter
# Histogram for 'cases' (2022) with normal fit
ggplot(us2022NY, aes(x = cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2022NY$cases), sd = sd(us2022NY$cases)), color = "red") +
  labs(title = "Histogram of Cases (2022) with Normal Fit", x = "Cases", y = "Density") +
  theme_minimal()

# Histogram for 'cases' (2023) with normal fit
ggplot(us2023NY, aes(x = cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "orange", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2023NY$cases), sd = sd(us2023NY$cases)), color = "red") +
  labs(title = "Histogram of Cases (2023) with Normal Fit", x = "Cases", y = "Density") +
  theme_minimal()

# Histogram for 'deaths' (2022) with normal fit
ggplot(us2022NY, aes(x = deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2022NY$deaths), sd = sd(us2022NY$deaths)), color = "red") +
  labs(title = "Histogram of Deaths (2022) with Normal Fit", x = "Deaths", y = "Density") +
  theme_minimal()

# Histogram for 'deaths' (2023) with normal fit
ggplot(us2023NY, aes(x = deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "orange", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(us2023NY$deaths), sd = sd(us2023NY$deaths)), color = "red") +
  labs(title = "Histogram of Deaths (2023) with Normal Fit", x = "Deaths", y = "Density") +
  theme_minimal()
#Normal fit seems the best for the histograms. The histograms are very skewed to the left because of the outliers in the datasets. 



#PART c
#ECDFs for the 2 datasets for the 2 variables and the filtered versions
#ecdf for 2022 and 2023 cases
ggplot(us2022n, aes(x = cases)) +
  stat_ecdf(geom = "step", color = "blue") +
  stat_ecdf(data = us2023n, aes(x = cases), geom = "step", color = "orange") +
  labs(title = "ECDF for Cases", x = "Cases", y = "ECDF") +
  theme_minimal()

# ECDF for 'deaths' for 2023 and 2022
ggplot(us2022n, aes(x = deaths)) +
  stat_ecdf(geom = "step", color = "blue") +
  stat_ecdf(data = us2023n, aes(x = deaths), geom = "step", color = "orange") +
  labs(title = "ECDF for Deaths", x = "Deaths", y = "ECDF") +
  theme_minimal()

# Q-Q Plot for 'cases' comparing to a normal distribution
qqnorm(us2022n$cases, main = "Q-Q Plot for Cases (2022)", col = "blue")
qqline(us2022n$cases, col = "red")
#2023
qqnorm(us2023n$cases, main = "Q-Q Plot for Cases (2023)", col = "orange")
qqline(us2023n$cases, col = "red")

# Q-Q Plot for 'deaths' comparing to a normal distribution
qqnorm(us2022n$deaths, main = "Q-Q Plot for Deaths (2022)", col = "blue")
qqline(us2022n$deaths, col = "red")
#2023
qqnorm(us2023n$deaths, main = "Q-Q Plot for Deaths (2023)", col = "orange")
qqline(us2023n$deaths, col = "red")

ggplot(us2022n, aes(x = cases)) +
  stat_ecdf(geom = "step", color = "blue") +
  stat_ecdf(data = us2023NY, aes(x = cases), geom = "step", color = "orange") +
  labs(title = "ECDF for Cases", x = "Cases", y = "ECDF") +
  theme_minimal()

# ECDF for 'deaths' for 2023 and 2022
ggplot(us2022NY, aes(x = deaths)) +
  stat_ecdf(geom = "step", color = "blue") +
  stat_ecdf(data = us2023NY, aes(x = deaths), geom = "step", color = "orange") +
  labs(title = "ECDF for Deaths", x = "Deaths", y = "ECDF") +
  theme_minimal()

# Q-Q Plot for 'cases' comparing to a normal distribution
qqnorm(us2022NY$cases, main = "Q-Q Plot for Cases (2022)", col = "blue")
qqline(us2022NY$cases, col = "red")
#2023
qqnorm(us2023NY$cases, main = "Q-Q Plot for Cases (2023)", col = "orange")
qqline(us2023NY$cases, col = "red")

# Q-Q Plot for 'deaths' comparing to a normal distribution
qqnorm(us2022NY$deaths, main = "Q-Q Plot for Deaths (2022)", col = "blue")
qqline(us2022NY$deaths, col = "red")
#2023
qqnorm(us2023NY$deaths, main = "Q-Q Plot for Deaths (2023)", col = "orange")
qqline(us2023NY$deaths, col = "red")
#the ecdfs rapidly increase because of the extreme outliers but are less extreme when only doing it with one state. 
#the qq  plots show a trend for a while but then dont because of the increadse in cases and deaths. 

#END QUESTION 1

#QUESTION 2
#a.
# Fit the linear model in R
model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = house)

# View the summary of the model
summary(model)

# Scatterplot of PROPERTYSQFT vs PRICE with the best fit line
ggplot(house, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Property Square Footage vs Price", x = "Property Square Footage", y = "Price") +
  theme_minimal()

# Plot residuals of the linear model
ggplot(house, aes(x = fitted(model), y = resid(model))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of the Linear Model", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#property sqft is the most influential. the plots are all super close together except for one valuewhich is an extrememoutlier.


#b.
# Subset the data where BEDS < 4
subset_data <- subset(house, BEDS < 4)

# Fit the linear model on the subset
model_subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = subset_data)

# View the summary of the model for the subset
summary(model_subset)

# Scatterplot of PROPERTYSQFT vs PRICE (Subset) with best fit line
ggplot(subset_data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Property Square Footage vs Price (Subset)", x = "Property Square Footage", y = "Price") +
  theme_minimal()

# Plot residuals of the subset model
ggplot(subset_data, aes(x = fitted(model_subset), y = resid(model_subset))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of the Linear Model (Subset)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#having less than 4 beds shows a trend where a lower square footage house has less beds, except for a few woutliers. as well as this a lower price also correlates to less beds and less squarefootage. 

