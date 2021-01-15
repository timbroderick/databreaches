# This file automates the data process for our databreach tracker
# https://www.modernhealthcare.com/assets/graphics/evergreen/breachtracker-20181022/child.html

# first step is to download the data from https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf
# we'll need both the cases under investigation (what you see there)
# and the archive (click the archive button)

# To download a csv of both, look a the gray bar just above the table
# called "Breach Report Results." On that bar and to the right there are four little icons
# hover over them and a little tooltip will popup with a description
# you'll want to click the third icon, "Export as CSV"

# this script assumes that you've downloaded and placed the two files
# in a folder called csv, that's in the same overall folder with this script

# set the working directory
setwd("~/anaconda3/envs/notebook/databreaches")

#?setwd

# load libraries
library(tidyverse)
options("scipen" = 10) # this makes sure big numbers don't devolve into scientific notation

df <- read_csv('2_output/combined.csv')


summary(df)

dflist <- select(df,'Name of Covered Entity','State','Covered Entity Type','Individuals Affected','Breach Submission Date','Type of Breach','Location of Breached Information') 
colnames(dflist) <- c('entity','state','org','affect','date','type','location')


dflist <- filter(dflist, (date >= '2020-01-01') & (date <= '2020-10-31') )

regions <- read_csv("1_data/regions.csv")

dfmerge <- merge(dflist,regions,by.x="state",by.y="st",all.x=TRUE)


dfreg <- dfmerge %>% 
  group_by(region) %>% 
  summarize(count=n(),
            affected=sum(affect))

dfreg$perccount <- round( (dfreg$count/sum(dfreg$count) )*100,2)
dfreg$percaffect <-  round( (dfreg$affected/sum(dfreg$affected) )*100,2)

write_csv(dfreg,"2_output/byregion.csv",na="")
