
R.home("bin")



library(RCurl)
raw_URL <- "https://github.com/ahiller13/MATH4780-LIFE-EXPECTANCY/blob/main/Life-Expectancy-Data-Updated.csv"
text_URL <- getURL(raw_URL)
life_expectancy <- read.csv(text = text_URL)





