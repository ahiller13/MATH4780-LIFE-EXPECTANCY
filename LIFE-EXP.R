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
library(dplyr)
library(car)
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
    labs(title = 'Life Expectancy by Economy status',
        x='Economy Status (developed)',
        y='Mean Life Expectancy') +
        theme(
            plot.title = element_text(size=32,face='bold',margin= margin(b=20,t=20)),
            axis.title.x = element_text(size=32, margin=margin(t=10)),
            axis.title.y = element_text(size=32, margin=margin(r=10)),
            plot.margin = unit(c(2,3,1,1),'cm')
        )


# Scatter Plot Matrix code
response_var_life_ex <- Life_expectancy$Life_expectancy
regressors <- Life_expectancy[, -which(names(Life_expectancy) == "Life_expectancy")]
regressors <- regressors %>% mutate(Country = as.factor(Country))
regressors <- regressors %>% mutate(Region = as.factor(Region))
# checking mutation
head(regressors)
str(regressors)
## ScatterPlot_matrix
pairs(regressors)

## For creating your model you might need the dataset below this
combined_data <- data.frame(Response= response_var_life_ex,regressors)

##################################################################
##### Nicky Code: for Symmetry and Normality #####################
##################################################################

full_model <- lm(Response ~ .,data=combined_data)
summ_full_mode <- summary(full_model)
## QQ Plot of R-Student Residuals Comparing Tn-p-1
car::qqPlot(full_model, id = TRUE, col.lines = "red",reps=1000,ylab="Ordered R-Student Residuals",pch=16,cex=2)

## Density Plot of R-Student Residuals
rstud <- rstudent(full_model)
hist(rstud,prob=TRUE,breaks=15, xlab= "R-Student Residuals",main = "R-Student residual Density Plot",ylim = c(0,max(density(rstud)$y) * 1.2),xlim = c(min(rstud), max(rstud)))
lines(density(rstud,adjust=2), col="red",lwd=2,label = "R-Student Residuals")

mu <- mean(rstud)
sigma <- sd(rstud)
x <- seq(min(rstud), max(rstud), length = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2,label = "Normal Distribution")


legend("topright", legend = c("R-Student Residuals", "Normal Distribution"), col = c("red", "blue"), lty = 1, lwd = 2)

par(cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2, mar = c(5, 5, 4, 2) + 0.1)

##Box-Cox
summary(car::powerTransform(full_model,family = 'bcPower'))


#lambda estimate 1.2442


##################################################################
##### Nicky Code: Lack of Fit ####################################
##################################################################






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







####################################################
################# Ethan Code #######################
################ Model Adequacy#####################

dataLE <- read.csv("/Users/ethanbaierl/Downloads/Life-Expectancy-Data-Updated.csv")
View(dataLE)

install.packages("car")
library(car)

## Linear regression models
linM1 <- lm(Life_expectancy ~ Infant_deaths + Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Measles + BMI + Polio + Diphtheria + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)
summary(linM1) ## R2 = 0.9792
               ## Adj R2 = 0.9790 (Lower due to penalty imposed from adding more variables)


# Remove non-significant variables from model 1
linM2 <- lm(Life_expectancy ~ Infant_deaths + Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)
summary(linM2) ## R2 = 0.9791
               ## Adj R2 = 0.9791


# Remove less significant variables from model 2
linM3 <- lm(Life_expectancy ~ Infant_deaths + Under_five_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)
summary(linM3) ## R2 = 0.9791
               ## Adj R2 = 0.979
vif(linM3) ## Collinearity

linM4 <- lm(Life_expectancy ~ Infant_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)  ## Remove 'Under_five_deaths'
summary(linM4) ## R2 = 0.9774
               ## Adj R2 = 0.9773
vif(linM4)


linM5 <- lm(Life_expectancy ~ + Under_five_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE) ## Remove 'Infant_deaths'
summary(linM5) ## R2 = 0.9787 
               ## Adj R2 = 0.9786
vif(linM5)
plot(linM5)
