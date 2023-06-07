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
dfopen <- read_csv("1_data/breach_report.csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
# they changed something with the portal - there's now 
# a space before the (1). That could change so watch for it
dfclosed <- read_csv("1_data/breach_report(1).csv", col_types = cols("Breach Submission Date" = col_date(format = "%m/%d/%Y")), na = "")
# add in the status just in case
dfopen$status <- "open"
dfclosed$status <- "closed"

# now append the two together
df <- rbind(dfopen, dfclosed)

df$breachd <- format(as.Date(df$`Breach Submission Date`), "%Y-%m") # need this to sort
df$breachdate <- format(as.Date(df$`Breach Submission Date`), "%b-%Y") # need this for presentation
df$breachyear <- format(as.Date(df$`Breach Submission Date`), "%Y") # need this for presentation
df$breachmonth <- format(as.Date(df$`Breach Submission Date`), "%m") # need this for presentation

# saving combined
write_csv(df,'2_output/combined.csv',na="")


# select just the columns we want and rename them
dfa <- select(df,"Individuals Affected","Breach Submission Date")
colnames(dfa) <- c("individuals","breaches")
  

# create columns of year, month to prep for grouping
dfa$breachd <- format(as.Date(dfa$breaches), "%Y-%m") # need this to sort
dfa$breachdate <- format(as.Date(dfa$breaches), "%b-%Y") # need this for presentation
dfa$breachyear <- format(as.Date(dfa$breaches), "%Y") # need this for presentation
dfa$breachmonth <- format(as.Date(dfa$breaches), "%m") # need this for presentation

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
dfgroup <- group_by(dfa, breachyear,breachd,breachdate) %>% 
  arrange(desc(breachd)) %>% 
  #filter(!is.na(account)) %>% 
  summarise(countbr = n(),sumind = sum(individuals,na.rm = TRUE))

# save to csv 
write_csv(dfgroup,'2_output/update.csv',na="")


# let's see how many breaches and individuals per year
dfsum <- group_by(dfa,breachyear) %>% summarize(count=n(),
                                                sumind = sum(individuals,na.rm = TRUE))

# save that
write_csv(dfsum,"2_output/byyear.csv",na="")


# now let's select just for the year to date
dfmonth <- dfa %>% filter(breachmonth <= "04")
unique(dfmonth$breachmonth)

dfy2d <- dfmonth %>% group_by(breachyear) %>% summarize(count=n(),
                                                sumind = sum(individuals,na.rm = TRUE))

write_csv(dfy2d,"2_output/todate.csv",na="")

#------ 
# Next, we create our top ten list for the year so far
# that's just a matter of slicing and sorting
# first get the columns we want and rename them
dflist <- select(df,'Name of Covered Entity','State','Covered Entity Type','Individuals Affected','Breach Submission Date','Type of Breach','Location of Breached Information') 
colnames(dflist) <- c('entity','state','org','affect','date','type','location')


# filter for the year we want, sort by top number of indv affected and slice the top ten
dflist <- filter(dflist, (date >= '2023-01-01') & (date <= '2023-04-30') ) %>% arrange(desc(affect)) %>% slice(1:10)

# save to csv 
write_csv(dflist,'2_output/topten.csv',na="")



#-----
# now let's create a meme chart


# first we want to grab just a few years
dfch <- filter(dfgroup, (breachd >= '2020-04') & (breachd <= '2023-04') )
# let's load our theme 
source("3_notes/theme_mh.R")

# now plot
plot <- ggplot(dfch) +
  aes(x = breachd, 
      y = countbr,
      fill = factor(breachyear)) +
  geom_bar(stat="identity") +
  # customizing our labels
  labs(title = "Healthcare data breaches",
       subtitle = "Number of breaches by month, through April 2023",
       caption = "Note: Numbers are preliminary. Date refers to when breaches were reported, not when\nthey occurred. Only breaches affecting 500 or more individuals reported.\nSource: HHS, Office for Civil Rights",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_mh() + # this is our theme, or styles
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x.bottom = element_line(size = .1),
        axis.ticks.length.x = unit(-.07, "cm"),
        legend.position = "none") + # this turns off the legend (we don't need one)
  scale_colour_manual( alternating_colors, name="" ) + 
  scale_fill_manual(values = alternating_colors, name="") +
  scale_x_discrete(labels=c("","","","July","","","","","",
                            "Jan\n2021","","","","","","July","","","","","",
                            "Jan\n2022","","","","","","July","","","","","","Jan\n2023","","",""))


plot
# let's add some annotation
# and an arrow 

plot_annotate <- plot +
  annotate("text", x = 25, y = 98, # placement of the text
           label = "There were fewer breaches in April, 2023\ncompared to the same month in 2021 and 2022.",
           size = 1.5, 
           fontface = 'bold', 
           lineheight=0.9) + 
  geom_curve( # start, end of line, and curve angle
    size=0.15,
    curvature = -0.4,
    ncp = 3000,
    arrow = arrow(length = unit(0.02, "npc")),
    color="#83425e",
    aes(x = 34.4,
        y = 95,
        xend = 37,
        yend = 48) 
  )

plot_annotate

# now let's save our plot
#width <- 2.43  
#height <- 1.75 
#dev.new(width = width, height = height, unit = "in", noRStudioGD =T)
# plot_annotate

width <- 1200
height <- 800
units <-"px"
dpi <- 400
ggsave("2_output/breaches_newsletter.jpg", plot_annotate, 
       width = width, height = height,
       units=units,dp=dpi)



# let's do a little geographcal analysis

# first add in a year column
df$breachyear <- format(as.Date(df$`Breach Submission Date`), "%Y") # need this for presentation

#grab the regions
regions <- read_csv("1_data/regions.csv")
df <- merge(df,regions,by.x="State",by.y="st",all.x=TRUE)

df2022 <- df %>% filter(breachyear=="2022")

dfreg22 <- df2022 %>% group_by(region) %>% 
  summarise(count=n(),
            sumind = sum(`Individuals Affected`,na.rm = TRUE))

dfsubreg22 <- df2022 %>% group_by(region, subregion) %>% 
  summarise(count=n(),
            sumind = sum(`Individuals Affected`,na.rm = TRUE))

df2023 <- df %>% filter(breachyear=="2023")

dfreg23 <- df2023 %>% group_by(region) %>% 
  summarise(count=n(),
            sumind = sum(`Individuals Affected`,na.rm = TRUE))

dfsubreg23 <- df2023 %>% group_by(region, subregion) %>% 
  summarise(count=n(),
            sumind = sum(`Individuals Affected`,na.rm = TRUE))


# save it out
write_csv(df,"2_output/geoanalize.csv",na="")


# type of breaches
dftype <- df %>% group_by(breachyear,`Type of Breach`) %>% 
  summarize(count=n())


#------------
#let's grab breaches under 10000
df10 <- filter(dfa,individuals<10000)

# group by year-month, sort descending and filter out any NAs in individuals then summarise
dfgroup10 <- group_by(df10, breachd,breachdate) %>% 
  arrange(desc(breachd)) %>% 
  summarise(countbr = n(),
            sumind = sum(individuals,na.rm = TRUE))

dfjoin <- merge(dfgroup,dfgroup10,by="breachdate",all.x=TRUE)
dfjoin$perc_breaches <- round((dfjoin$countbr.y/dfjoin$countbr.x)*100,2)
dfjoin$perc_indv <- round((dfjoin$sumind.y/dfjoin$sumind.x)*100,2)

# let's do it by year
# group by year-month, sort descending and filter out any NAs in individuals then summarise
dfyear10 <- group_by(df10, breachyear) %>% 
  arrange(desc(breachyear)) %>% 
  #filter(!is.na(account)) %>% 
  summarise(countbr = n(),sumind = sum(individuals,na.rm = TRUE))

yrjoin <- merge(dfyear,dfyear10,by="breachyear",all.x=TRUE)
yrjoin$perc_breaches <- round((yrjoin$countbr.y/yrjoin$countbr.x)*100,2)
yrjoin$perc_indv <- round((yrjoin$sumind.y/yrjoin$sumind.x)*100,2)
yrjoin$over10 <- yrjoin$countbr.x - yrjoin$countbr.y
yrjoin$pover10_perc <- round((yrjoin$over10/yrjoin$countbr.x)*100,2)

write_csv(yrjoin,"2_output/smalltargets.csv")



#------
# breaches by year
dfa$breachyear <- format(as.Date(dfa$breaches), "%Y") 

# group by year-month, sort descending and filter out any NAs in individuals then summarise
dfyear <- group_by(dfa, breachyear) %>% 
  arrange(desc(breachyear)) %>% 
  #filter(!is.na(account)) %>% 
  summarise(countbr = n(),sumind = sum(individuals,na.rm = TRUE))

# save to csv 
write_csv(dfyear,'2_output/updatebyyear.csv',na="")
