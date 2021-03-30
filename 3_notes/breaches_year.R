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

# check the file names. This is for a mac. Windows may be different
# learn more at: https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
files <- list.files("1_data/") 
for (file in files) {
  print(paste(file),quote=FALSE) # this should print the filenames that are in the csv dir
}

# Reading these dates in requires a special format: %m/%d/%Y
# Note the capitol Y for years as 0000
dfyear <- read_csv("2_output/combined.csv", na = "")


head(df)
summary(df)

#------ 
# Create our top ten list for the period we're examining
# that's just a matter of slicing and sorting
# first get the columns we want and rename them
dflist <- select(dfyear,'Name of Covered Entity','State','Covered Entity Type','Individuals Affected','Breach Submission Date','Type of Breach','Location of Breached Information') 
colnames(dflist) <- c('entity','state','org','affect','date','type','location')
# filter for the period we want, sort by top number of indv affected and slice the top ten
dften <- filter(dflist, (date >= '2020-01-01') & (date <= '2020-11-30') ) %>% arrange(desc(affect)) %>% slice(1:10)
# save to csv 
write_csv(dften,'2_output/topten_year.csv',na="")


# now let's see what we can learn from this year
# create columns of year, month and just year to prep for grouping
dflist$yearmonth <- format(as.Date(dflist$date), "%Y-%m") 
dflist$year <- format(as.Date(dflist$date), "%Y") 
dflist$month <- format(as.Date(dflist$date), "%m") 

dflist_dec <- filter(dflist,month<12)

# let's exclude any reported in december
# breaches by location
dflocate <- dflist_dec %>% 
  group_by(year,location) %>% 
  summarize(count=n())
# now pivot wide
locate_wide <- pivot_wider(dflocate,id_cols = c("location"), 
                                        names_from = "year", 
                                        values_from = c("count") )
write_csv(locate_wide,"2_output/location_year.csv",na="")

# breaches by type
dftype <- dflist_dec %>% 
  group_by(year,type) %>% 
  summarize(count=n())
# now pivot wide
type_wide <- pivot_wider(dftype,id_cols = c("type"), 
                           names_from = "year", 
                           values_from = c("count") )
write_csv(type_wide,"2_output/type_year.csv",na="")



# select just the columns we want and rename them
dfa <- select(df,"Individuals Affected","Breach Submission Date")
colnames(dfa) <- c("individuals","breaches")
  

# create columns of year, month to prep for grouping
dfa$breachd <- format(as.Date(dfa$breaches), "%Y-%m") # need this to sort
dfa$breachdate <- format(as.Date(dfa$breaches), "%b-%Y") # need this for presentation

# now take a look at what we have so far
head(dfa)

# we have everything we need to do essentially a pivot table in R
# what we want to end up with is three columns
# A column with a month an year like Jan-2019
# A column that counts the number of breaches in each month
# A column that totals the number of individuals affected by breaches for each month
# https://www.rforexcelusers.com/make-pivottable-in-r/
# here's how we do that:


# group by year-month, sort descending and filter out any NAs in individuals then summarise
dfgroup <- group_by(dfa, breachd,breachdate) %>% 
  arrange(desc(breachd)) %>% 
  #filter(!is.na(account)) %>% 
  summarise(countbr = n(),sumind = sum(individuals,na.rm = TRUE))

# take a look at what we have
head(dfgroup)
tail(dfgroup)


# save to csv 
write_csv(dfgroup,'2_output/update.csv',na="")



