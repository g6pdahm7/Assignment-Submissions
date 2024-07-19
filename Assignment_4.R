#Assignment 4 - Ahmed Mokhtar

#' Plan: 
#' 1. Go through columns and fix formatting
#' if applicable. 
#' 2. Break up data into multiple columns if applicable.

#' Reading the dataframe and assigning it to variable
#' "ufodata".
ufodata <- read.csv("C:/Users/ahmed/OneDrive - University of Toronto/MBiotech/Summer 2024/R/Assignment-Submissions/ufo_subset.csv")

#' I am going to include this here in case we need to use it 
#' later on.
library(lubridate)

#' First I check the unique values of the country column.
#' I noticed that blank cells are not converted to NAs.
#' Instead, I decided to mark them as unreported and to 
#' use it in any upcoming analysis. 
unique(ufodata$country)
sum(is.na(ufodata$country))
ufodata$country[ufodata$country == ""] <- "UNREPORTED"

#' First I checked the unique values. 
#' I replaced blank cells with "UNREPORTED. This was followed by
#' changing entries marked as "light" to "other", since 
#' light is not a shape. 
unique(ufodata$shape)
sum(is.na(ufodata$shape))
ufodata$shape[ufodata$shape == ""] <- "UNREPORTED"
ufodata$shape[ufodata$shape == "light"] <- "other"

#' First I checked to see if there are any cells that are
#' missing entries. I decided not to eliminate any outliers
#' when it comes to length of duration. This is because
#' someone may have observed a ufo over the span of multiple 
#' days, and then reported it as one long observation.
length(ufodata$duration.seconds[ufodata$duration.seconds == ""])

#' First I checked if there are any empty cells. None of the 
#' two date columns have any empty cells. I also checked for
#' the presence of NAs, none are present.
length(ufodata$datetime[ufodata$datetime == ""])
length(ufodata$date_posted[ufodata$date_posted == ""])
sum(is.na(ufodata$datetime))
sum(is.na(ufodata$date_posted))

#' Next, we have to check if there are any hoaxes, and eliminate
#' their rows from the dataframe. The grepl function was used 
#' to identify rows where the word hoax was mentioned in the
#' comments column. I also made sure to ignose letter casing. 
#' This was followed by deleting these rows that were assigned
#' to "hoaxes". I then wanted to double check that they have all
#' been removed, so I used the sum function to get a count. 
hoaxes <- grepl("hoax", ufodata$comments, ignore.case = TRUE)
ufodata <- ufodata[!hoaxes, ]
sum(grepl("hoax", ufodata$comments, ignore.case = TRUE))

#' Next, we have to convert the two date columns to date formats from 
#' a set of characters. I decided to use the as.Date function, and chose to
#' eliminate the times, since we are only interested in the difference in days. 
ufodata$datetime <- as.Date(ufodata$datetime, format = "%Y-%m-%d")
ufodata$date_posted <- as.Date(ufodata$date_posted, format = "%d-%m-%Y")

#' To calculate the difference in time, I just the columns from each other.
ufodata$report_delay <- ufodata$date_posted - ufodata$datetime

#' Next, I wanted to identify how many of the entries in the report_delay
#' column were negative, indicating that the sighting date was after the 
#' reporting date. There were 6. I simply created a placeholder, and 
#' reassigned the placeholder without the rows with the negative values of 
#' report delay.
sum(ufodata$report_delay < 0)
placeholder <- ufodata$report_delay < 0
ufodata <- ufodata[!placeholder, ]

#' In order to create the table, I decided to follow a series of steps. 
#' The first step is to use piping effectively to group them by countries,
#' This is followed by calculating the mean delay for each country. I 
#' decided to print after that process so we can see the table. 
library(dplyr)
countrytable <- ufodata %>%
  group_by(country) %>%
  summarize("Average Delay" = mean(report_delay))
print(countrytable)


#' For the histogram, I decided to graph the log of duration.seconds. I
#' also picked the colour red because it is my favorite colour.
hist(log(as.numeric(ufodata$duration.seconds)), main = "Histogram of the Log Duration of Sightings", xlab = "Log of the observation duration time (s)", ylab = "Number of Occurences", col = "red")
