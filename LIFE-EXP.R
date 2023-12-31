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
headers <- c('Authorization' = paste('Bearer',''))

dataset <- getURL(url_w_token,httpheader = headers)
#print(dataset)

Life_expectancy <- read.csv(text = dataset)

# Checking format
head(Life_expectancy)


### PLOT OF DEVELOPED VS NON DEVELOPED AND THE LIFE EXPECTANCY
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

# Install necessary packages
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
linM3 <- lm(Life_expectancy ~ Infant_deaths + 
              Under_five_deaths + 
              Adult_mortality + 
              Alcohol_consumption + 
              BMI + Incidents_HIV + 
              GDP_per_capita + 
              Thinness_five_nine_years + 
              Schooling + 
              Economy_status_Developed,
            data = dataLE)

summary(linM3) ## R2 = 0.9791
               ## Adj R2 = 0.979
vif(linM3)

linM4 <- lm(Life_expectancy ~ Infant_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)
summary(linM4)
vif(linM4)

install.packages("car")
library(car)
linM5 <- lm(Life_expectancy ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed,
            data = dataLE)
summary(linM5) ## R2 = 0.9787
               ## Adj R2 = 0.9786

vif(linM5)
plot(linM5)

#########################
## Leverage Points
hatLinM5 <- hatvalues(linM5)
sortHatLinM5 <- sort(hatLinM5, decreasing = TRUE)

# Calculate the threshold based on the number of predictors (p) and the sample size (n)
p <- 9  # Number of predictors
n <- dim(dataTest)[1]  # Sample size

# Calculate the threshold (2 * p / n)
threshold <- 2 * p / n

# Find the observations with hat values greater than the threshold
outliers <- sortHatLinM5[sortHatLinM5 > threshold]

# Print or inspect the outliers
print(outliers)
plot(outliers)
########################################
### Display outliers and leverage points & Influential points
library(olsrr)
ols_plot_resid_lev(linM5)

### Cooks D
ols_plot_cooksd_chart(linM5)
cooks.distance(linM5)

# Influential mesaures
influence.measures(linM5)


###############################
# Calculate studentized residuals
r_student <- rstudent(linM5)
# Round and sort the studentized residuals in decreasing order
sorted_r_student <- round(sort(r_student, decreasing = TRUE), 2)
# Print or inspect the sorted studentized residuals
print(sorted_r_student)
plot(sorted_r_student)

# Calculate DFFITS
round(dffit <- dffits(linM5), 2)
dffit[abs(dffit) > 2 * sqrt(p/n)] 

# Calculate COV Ratio
(covra <- covratio(linM5))
covra[covra > (1 + 3*p/n)] 
covra[covra < (1 - 3*p/n)] 

# Bubble Plot
car::influencePlot(linM5)


#####################
#### Collinearity ###
plot(dataLE$Infant_deaths, dataLE$Under_five_deaths, 
  main = "Infant Deaths vs Under 5 y/o Deaths",
  xlab = "Infant Deaths",
  ylab = "Under 5 y/o Deaths",
)

plot(dataLE$Polio, dataLE$Diphtheria, 
     main = "Polio vs Diphtheria",
     xlab = "Polio",
     ylab = "Diptheria",
)

#################
## train & test model set
install.packages("caret")
library(caret)
install.packages("magrittr")
library(magrittr)

# Set seed for reproducibility/randomness
set.seed(1)

## Create new dataset using the variables from linM5 (best model)
dataTest <- dataLE[c("Under_five_deaths",
                     "Adult_mortality",
                     "Alcohol_consumption",
                     "BMI",
                     "Incidents_HIV",
                     "GDP_per_capita",
                     "Thinness_five_nine_years",
                     "Schooling",
                     "Economy_status_Developed",
                     "Life_expectancy"
                     )]
                     
                     

# Split the data into training (70%) and validation (30%) sets
splitIndex <- createDataPartition(dataTest$Life_expectancy,
                                  p = 0.7, list = FALSE)

trainData <- dataTest[splitIndex, ]
validData <- dataTest[-splitIndex, ]

modellm <- lm(Life_expectancy ~., trainData)
summary(modellm)
prediction <- modellm %>% predict(validData)
modelError <- RMSE(predict, validData$Life_expectancy) ## 1.33
modelAccuracy <- R2(predict, validData$Life_expectancy) ## 0.9795

# Formula to find error rate using RMSE
errorRate <- modelError/mean(validData$Life_expectancy) ## 1.93%
errorRate




##################################
############Andrew################
####Residuals vs. Fitted Values###

# Checking for Non-Constant Variance
# Plotting residuals vs fitted values
plot(linM5$fitted.values, linM5$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values Plot",
     pch = 16, col = "blue")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Add a lowess smooth line to help identify patterns
lines(lowess(linM5$fitted.values, linM5$residuals), col = "green")

# Add a legend
legend("topright", legend = c("Residuals", "y = 0", "Lowess Smooth"),
       col = c("blue", "red", "green"), pch = c(16, 16, 16))
