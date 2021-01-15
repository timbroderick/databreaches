# set directory
setwd("~/anaconda3/envs/notebook/databreaches")

# load libraries
library(tidyverse)
options("scipen" = 10)

# check the file names. This is for a mac. Windows may be different
# learn more at: https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
files <- list.files("1_data/") 
for (file in files) {
  print(paste(file),quote=FALSE) # this should print the filenames that are in the csv dir
}

dfopen <- read_csv("1_data/breach_report.csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
dfclosed <- read_csv("1_data/breach_report(1).csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
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
df20 <- df %>% filter((breaches >= '2020-01-01') & (breaches <= '2020-01-31') )

# for comparison purposes, we want the average number of individuals affected per month,
# and the number of breaches by type, location and place

# first individuals
dfindv20 <- df20 %>% 
  group_by(breachd,breachdate) %>% 
  filter(!is.na(individuals)) %>%
  summarize(count=n(),
            avg=round(mean(individuals),1),
            sum=sum(individuals))

# save the file
write_csv(dfindv20,'2_output/dfindv_0120.csv', na = '')

# now by type
dftype20 <- df20 %>% 
  group_by(type) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dftype20,'2_output/dftype_0120.csv', na = '')

# location
dflocate20 <- df20 %>% 
  group_by(location) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dflocate20,'2_output/dflocate_0120.csv', na = '')

# finally by org
dfplace20 <- df20 %>% 
  group_by(place) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfplace20,'2_output/dfplace_0120.csv', na = '')



# now for comparison
# grab anything before 2020
# should only have to do this a few times into the new year
dfpre <- df %>% filter(breachd < "2020-01")

dfpre_ind <- dfpre %>% 
  group_by(month) %>% 
  filter(!is.na(individuals)) %>%
  summarize(avgmonth=round(mean(individuals),1))

# save the file
write_csv(dfpre_ind,'2_output/dfpre_ind.csv', na = '')

# by type
dfpre_type <- dfpre %>% 
  group_by(type) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfpre_type,'2_output/dfpre_type.csv', na = '')

# by location
dfpre_locate <- dfpre %>% 
  group_by(location) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfpre_locate,'2_output/dfpre_locate.csv', na = '')

# and by org
dfpre_place <- dfpre %>% 
  group_by(place) %>% 
  summarize(count=n(),
            proportion = round( (count / nrow(.) )*100,1)
  )
# save the file
write_csv(dfpre_place,'2_output/dfpre_place.csv', na = '')



