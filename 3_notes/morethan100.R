# set the working directory
setwd("~/anaconda3/envs/notebook/databreaches")

#?setwd

# load libraries
library(tidyverse)
options("scipen" = 10) # this makes sure big numbers don't devolve into scientific notation

summary(dfa)

dfmore <- filter(dfa,individuals >= 100000)

# let's create a year
dfmore$year <- format(as.Date(dfmore$breaches), "%Y") # need this to sort


# group by year-month, sort descending and filter out any NAs in individuals then summarise
dfgroup <- dfmore %>% 
  group_by(year) %>% 
  arrange(desc(breachd)) %>% 
  #filter(!is.na(account)) %>% 
  summarise(countbr = n(),sumind = sum(individuals))


write_csv(dfgroup,"2_output/morethan100.csv")
