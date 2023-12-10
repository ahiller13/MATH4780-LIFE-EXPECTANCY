#in case you need to install these packages
#install.packages("GGally")
#install.packages("tidyverse")
#install.packages("RCurl")
#install.packages("ggplot2")
# DECEMBER 9TH, 2023
#libraries needed
library(RCurl)
library(tidyverse)
library(GGally)
# Getting the url With my token
url_w_token <- "https://raw.githubusercontent.com/ahiller13/MATH4780-LIFE-EXPECTANCY/main/LEDU.csv?token=GHSAT0AAAAAACLGJC4OLAMZY2OQP73GPKEAZLU5MVQ"

# nicky token
headers <- c('Authorization' = paste('Bearer','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'))

dataset <- getURL(url_w_token,httpheader = headers)
#print(dataset)

Life_expectancy <- read.csv(text = dataset)

# Checking format
head(Life_expectancy)

ggplot(Life_expectancy,aes(x= as.factor(Economy_status_Developed),y= Life_expectancy)) +
    stat_summary(fun = mean,geom = 'bar',fill = 'skyblue',position = 'dodge') +
    labs(title = 'Bar Graph of mean Life Expectancy by Economy status (developed)',
        x='Economy Status (developed)',
        y='Mean Life Expectancy') +
        theme(plot.title = element_text(size=48,face='bold'),
        axis.title.x = element_text(size=32),
        axis.title.y = element_text(size=32)
        )


# Scatter Plot Matrix
response_var_life_ex <- Life_expectancy$Life_expectancy
regressors <- Life_expectancy[, -which(names(Life_expectancy) == "Life_expectancy")]
pairs(regressors)
## Decapritated
#################### ANDREW BIT ####################
#just how I have the data set mapped in my files on my laptop
#data <- read.csv("School/FALL23/MATH 4780/R sets/Life-Expectancy-Data-Updated.csv")

# Check the structure of the dataset
#str(data)

#(12/06)
#Built a basic bar graph comparing mean life expectancy for Developed and Developing countries

# Create a bar graph with mean Life_expectancy
#ggplot(data, aes(x = as.factor(Economy_status_Developed), y = Life_expectancy)) +
#  stat_summary(fun = mean, geom = "bar", fill = "skyblue", position = "dodge") +
#  labs(title = "Bar Graph of Mean Life Expectancy by Economy Status (Developed)",
#       x = "Economy Status (Developed)",
#       y = "Mean Life Expectancy")
