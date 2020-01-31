# set directory
setwd("~/anaconda3/envs/notebook/databreaches")

# load libraries
library(tidyverse)
options("scipen" = 10)

# check the file names. This is for a mac. Windows may be different
# learn more at: https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
files <- list.files("csv/") 
for (file in files) {
  print(paste(file),quote=FALSE) # this should print the filenames that are in the csv dir
}

dfopen <- read_csv("csv/breach_report.csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
dfclosed <- read_csv("csv/breach_report(1).csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
# add in the status just in case
dfopen$status <- "open"
dfclosed$status <- "closed"

# now append the two together
df <- rbind(dfopen, dfclosed)
colnames(df)

colnames(df) <- c("name","state","place","individuals","breaches","type","location","bizassoc","web","status")

# create columns of year, month to prep for grouping
df$breachd <- format(as.Date(df$breaches), "%Y-%m") # need this to sort
df$breachdate <- format(as.Date(df$breaches), "%b-%Y") # need this for presentation
df$month <- format(as.Date(df$breaches), "%m") # need this to group 

# grab anything in current month
df19 <- df %>% filter((breaches >= '2019-12-01') & (breaches <= '2019-12-31') )

# for comparison purposes, we want the average number of individuals affected per month,
# and the number of breaches by type, location and place

# first individuals
dfindv19 <- df19 %>% 
  group_by(breachd,breachdate) %>% 
  filter(!is.na(individuals)) %>%
  summarize(count=n(),
            avg=round(mean(individuals),1),
            sum=sum(individuals))

# save the file
write_csv(dfindv19,'csv/dfindv19.csv', na = '')

# now by type
dftype19 <- df19 %>% 
  group_by(type) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dftype19,'csv/dftype19.csv', na = '')

# location
dflocate19 <- df19 %>% 
  group_by(location) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dflocate19,'csv/dflocate19.csv', na = '')

# finally by org
dfplace19 <- df19 %>% 
  group_by(place) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfplace19,'csv/dfplace19.csv', na = '')



# now for comparison
# grab anything before 2019
df18 <- df %>% filter(breachd < "2019-01")

dfindv18 <- df18 %>% 
  group_by(month) %>% 
  filter(!is.na(individuals)) %>%
  summarize(avgmonth=round(mean(individuals),1))

# save the file
write_csv(dfindv18,'csv/dfindv18.csv', na = '')

# by type
dftype18 <- df18 %>% 
  group_by(type) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dftype18,'csv/dftype18.csv', na = '')

# by location
dflocate18 <- df18 %>% 
  group_by(location) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dflocate18,'csv/dflocate18.csv', na = '')

# and by org
dfplace18 <- df18 %>% 
  group_by(place) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfplace18,'csv/dfplace18.csv', na = '')



