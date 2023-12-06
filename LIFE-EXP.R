library(tidyverse)

#just how I have the data set mapped in my files on my laptop
data <- read.csv("School/FALL23/MATH 4780/R sets/Life-Expectancy-Data-Updated.csv")

# Check the structure of the dataset
str(data)

#(12/06)
#Built a basic bar graph comparing mean life expectancy for Developed and Developing countries

# Create a bar graph with mean Life_expectancy
ggplot(data, aes(x = as.factor(Economy_status_Developed), y = Life_expectancy)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", position = "dodge") +
  labs(title = "Bar Graph of Mean Life Expectancy by Economy Status (Developed)",
       x = "Economy Status (Developed)",
       y = "Mean Life Expectancy")

