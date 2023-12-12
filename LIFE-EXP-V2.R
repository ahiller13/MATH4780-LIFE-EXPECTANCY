library(RCurl)
library(tidyverse)
library(GGally)
library(dplyr)
library(car)

##url for Raw Data
url_w_token <- "https://raw.githubusercontent.com/ahiller13/MATH4780-LIFE-EXPECTANCY/main/LEDU.csv?token=GHSAT0AAAAAACLGJC4OLAMZY2OQP73GPKEAZLU5MVQ"

# nicky token
headers <- c('Authorization' = paste('Bearer','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'))

dataset <- getURL(url_w_token,httpheader = headers)
#print(dataset)

Life_expectancy <- read.csv(text = dataset)

response_var_life_ex <- Life_expectancy$Life_expectancy
regressors <- Life_expectancy[, -which(names(Life_expectancy) == "Life_expectancy")]
regressors <- regressors %>% mutate(Country = as.factor(Country))
regressors <- regressors %>% mutate(Region = as.factor(Region))

exclude_vars <- c("Country","Region","Economy_status_Developed","Economy_status_Developing")



## For creating your model you might need the dataset below this
combined_data <- data.frame(Response= response_var_life_ex,regressors)

## this is to make the covariance matrix to get rid of collinearity
regressors_for_cov <- regressors %>% select(-one_of(exclude_vars)) 

X <- regressors_for_cov[,-1]
Sig <- cor(X)
C <- solve(Sig)
VIF_scores=diag(C)
print(VIF_scores)






### MODEL 5 Model Adequacy
model_5 <- lm(Response ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed, data = combined_data)
vif(model_5)
summary_full_model <- summary(model_5)
## QQ Plot checking for normality
car::qqPlot(model_5, id = TRUE, col.lines = "red",reps=1000,ylab="Ordered R-Student Residuals",main="QQ plot for our model",pch=16,cex=1.5,cex.main=3,cex.lab = 3)

## Density plot of R-student residuals
rstud <- rstudent(model_5)
hist(rstud,prob=TRUE,breaks=15, xlab= "R-Student Residuals",main = "R-Student residual Density Plot (Original Model)",ylim = c(0,max(density(rstud)$y) * 1.2),xlim = c(min(rstud), max(rstud)))
lines(density(rstud,adjust=2), col="red",lwd=2,label = "R-Student Residuals")

mu <- mean(rstud)
sigma <- sd(rstud)
x <- seq(min(rstud), max(rstud), length = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2,label = "Normal Distribution")

legend("topright", legend = c("R-Student Residuals", "Normal Distribution"), col = c("red", "blue"), lty = 1, lwd = 2)

par(cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2, mar = c(5, 5, 4, 2) + 0.1)


##boxcox transformation
summary(car::powerTransform(model_5, family = "bcPower"))

## It says 0.083 if its close to zero log transform 

model_5_w_log <- lm(log(Response) ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + BMI + Incidents_HIV + GDP_per_capita + Thinness_five_nine_years + Schooling + Economy_status_Developed, data = combined_data)

car::qqPlot(model_5_w_log, id = TRUE, col.lines = "red",reps=1000,ylab="Ordered R-Student Residuals",main="QQ plot for model with Log transformation",pch=16,cex=2,cex.main=3,cex.lab = 3)
coef(model_5_w_log)

## Density plot of R-student residuals
rstud <- rstudent(model_5_w_log)
hist(rstud,prob=TRUE,breaks=15, xlab= "R-Student Residuals",main = "R-Student residual Density Plot (model 5 w/ Log)",ylim = c(0,max(density(rstud)$y) * 1.2),xlim = c(min(rstud), max(rstud)))
lines(density(rstud,adjust=2), col="red",lwd=2,label = "R-Student Residuals")

mu <- mean(rstud)
sigma <- sd(rstud)
x <- seq(min(rstud), max(rstud), length = 100)
y <- dnorm(x, mean = mu, sd = sigma)
lines(x, y, col = "blue", lwd = 2,label = "Normal Distribution")

legend("topright", legend = c("R-Student Residuals", "Normal Distribution"), col = c("red", "blue"), lty = 1, lwd = 2)

par(cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2, mar = c(5, 5, 4, 2) + 0.1)

#########################################
#### NON-CONSTANT VARIANCE ##############
#########################################
car::spreadLevelPlot(model_5,smooth = FALSE,pch=16, col = "red", lwd = 3)
car::spreadLevelPlot(model_5_w_log,smooth = FALSE,pch=16, col = "red", lwd = 3)

########################################
#### LINEARITY LACK OF FIT #############
#######################################