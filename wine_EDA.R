#---
#title: "Individual Assignment 2 - Esslam Mumen"
#author: "Esslam Mumen"
#date: "16/6/2022"
#---
#Purpose:
#In this notebook, I am aiming to perform some exploratory data analysis on the quality of red
#wine, using its main features such as acidity, chloride, pH, density, etc. I will be using
#some of the main tools provided in R with its most commonly utilized packages for data analysis.
  
library(tidyverse)
library(janitor)
library(psych)

getwd()
install.packages("devtools")
devtools::install_github("sfirke/janitor")

#First, let's import the data and clean its variable names.

wine <- read_csv("winequality-red.csv")

wine %>%
  clean_names()

make.names(names(wine))

#Now, let's obtain a basic summary of each column in the data using the summary function.
#For a more detailed report, we can use the 'describe' function in the "psych" package.

summary(wine)
psych::describe(wine)
glimpse(wine) #There are 12 columns and 1599 rows in this dataset.

#A histogram showing the frequency for each quality rating in the dataset


hist(wine$quality, 
     main = "Histogram of Quality", 
     xlab = "Red Wine Quality Ratings",
     ylab = "Frequency") #Most of the dataset contains quality ratings between 4
#and 8


#We can also tidy up the data using the pivot_longer function in tidyr.
wine %>% 
  pivot_longer(cols = everything(), names_to = "key", values_to = "value") -> wine_t

wine_t


#If we want to isolate the dataset into good and bad quality, we can use filter()
wine %>%
  filter(quality >= 6 | quality < 6)
good_quality <- wine$quality >= 6
bad_quality <- wine$quality < 6
good <- as_tibble(good_quality)
good

#Now, let's make some transformations using mutate()

diamonds %>%
  mutate(fixed_acidity_log10 = log10(wine$`fixed acidity`),
         volatile_acidity_log10 = log10(wine$volatileacidity),
         citric_acid_log10 = log10(wine$citric_acid),
         residual_sugar_log10 = log10(wine$residual_sugar),
         chlorides_log10 = log10(wine$chlorides),
         density_log10 = log10(wine$density),
         total_sulfur_dioxide_log10 = log10(wine$total_sulfur_dioxide))

##Since the red wine in the dataset are categorized based on quality, let's find
#out what differentiates wine quality.In other words, let's find out "what" 
#makes a good or bad quality wine.

#We can summarize each column depending on quality rating and then plot them.
#Turning it into data frame helps in providing more control of the data and
#plotting them.

quality_pH_summary <- data.frame(wine %>%
                                group_by(quality) %>%
                                summarise(avg = mean(pH),
                                          stdev = sd(pH),
                                          min = min(pH),
                                          max = max(pH),
                                          range = diff(range(pH))))

quality_pH_summary

#Quality based on density
quality_density_summary <- data.frame(wine %>%
                                   group_by(quality) %>%
                                   summarise(avg = mean(density),
                                             stdev = sd(density),
                                             min = min(density),
                                             max = max(density),
                                             range = diff(range(density))))

quality_density_summary

#Quality based on fixed acidity

quality_fixed_acidity_summary <- data.frame(wine %>%
                                        group_by(quality) %>%
                                        summarise(avg = mean(`fixed acidity`),
                                                  stdev = sd(`fixed acidity`),
                                                  min = min(`fixed acidity`),
                                                  max = max(`fixed acidity`),
                                                  range = diff(range(`fixed acidity`))))
quality_fixed_acidity_summary
#Quality based on residual sugars

quality_residual_sugars_summary <- data.frame(wine %>%
                                              group_by(quality) %>%
                                              summarise(avg = mean(`residual sugar`),
                                                        stdev = sd(`residual sugar`),
                                                        min = min(`residual sugar`),
                                                        max = max(`residual sugar`),
                                                        range = diff(range(`residual sugar`))))
quality_residual_sugars_summary

#Quality based on chlorides

quality_chlorides_summary <- data.frame(wine %>%
                                                group_by(quality) %>%
                                                summarise(avg = mean(chlorides),
                                                          stdev = sd(chlorides),
                                                          min = min(chlorides),
                                                          max = max(chlorides),
                                                          range = diff(range(chlorides))))
quality_chlorides_summary

#Quality based on total sulfur dioxide

quality_total_sulfur_dioxide_summary <- data.frame(wine %>%
                                          group_by(quality) %>%
                                          summarise(avg = mean(`total sulfur dioxide`),
                                                    stdev = sd(`total sulfur dioxide`),
                                                    min = min(`total sulfur dioxide`),
                                                    max = max(`total sulfur dioxide`),
                                                    range = diff(range(`total sulfur dioxide`))))
quality_total_sulfur_dioxide_summary

#Quality based on alcohol

quality_alcohol_summary <- data.frame(wine %>%
                                                     group_by(quality) %>%
                                                     summarise(avg = mean(`total sulfur dioxide`),
                                                               stdev = sd(`total sulfur dioxide`),
                                                               min = min(`total sulfur dioxide`),
                                                               max = max(`total sulfur dioxide`),
                                                               range = diff(range(`total sulfur dioxide`))))
quality_alcohol_summary

#The average pH is shown for every quality rating. Moreover, note that the quality
#in this particular dataset ranges between 5 and 8 out of 10.
#To better determine whether lower pH contributes to a higher quality wine, let's
#visualize it by plotting the mean pH for every quality point.

plot(quality_pH_summary$quality, quality_pH_summary$avg,
     main="Average pH for each Rating",
     ylab="Average pH",
     xlab="Quality Rating") #According to this plot, lower pH produces higher 
#quality wine and vice versa.


#Let's apply this method with some other variables

plot(quality_density_summary$quality, quality_density_summary$avg,
     main="Average Density for each Rating",
     ylab="Average Density",
     xlab="Quality Rating") # In general, the lower the density, the higher the
#quality rating

plot(quality_residual_sugars_summary$quality, quality_residual_sugars_summary$avg,
     main="Average Amount of Residual Sugars for each Rating",
     ylab="Average Residual Sugars",
     xlab="Quality Rating")#There is no clear pattern here. The amount of residual
#sugars generally does not affect the quality of red wine.

plot(quality_fixed_acidity_summary$quality, quality_fixed_acidity_summary$avg,
     main="Average Fixed Acidity for each Rating",
     ylab="Average Fixed Acidity",
     xlab="Quality Rating")#Red wine with low average fixed acidity yields 
#better quality.

plot(quality_chlorides_summary$quality, quality_chlorides_summary$avg,
     main="Average Chloride Amount for each Rating",
     ylab="Average Chloride",
     xlab="Quality Rating")

plot(quality_total_sulfur_dioxide_summary$quality, quality_total_sulfur_dioxide_summary$avg,
     main="Average Total Sulfur Dioxide for each Rating",
     ylab="Average pH",
     xlab="Quality Rating")#Lower amounts of chloride yield better quality.

plot(quality_alcohol_summary$quality, quality_alcohol_summary$avg,
     main="Average Amount of Alcohol for each Rating",
     ylab="Average Alcohol",
     xlab="Quality Rating")#According to the plot, lower alcohol in wine makes
#it better.


#The subset() function can be useful but is currently not needed for this particular
#dataset, as it is preferably used for categorical data.

new_data1 <- subset(wine$pH, wine$quality == 3)
new_data1

new_data2 <- subset(wine$pH, wine$quality == 4)
new_data2

new_data3 <- subset(wine$pH, wine$quality == 5)
new_data3

new_data4 <- subset(wine$pH, wine$quality == 6)
new_data4

new_data5 <- subset(wine$pH, wine$quality == 7)
new_data5

new_data6 <- subset(wine$pH, wine$quality == 8)
new_data6

