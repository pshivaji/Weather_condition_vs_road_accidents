library(readr)
library(dplyr)

crashdata <- read_csv("C:/Users/lucin/OneDrive/Documents/GitHub/DATA7001Project/Data/locations.csv")
print(head(crashdata))

crashdata$regional_group <- " "
currentyear <- 9999
group <- 0

for (i in 1:nrow(crashdata)) {
  if (crashdata[i, "Crash_Year"] < currentyear)
    group <- group + 1
  crashdata[i, "regional_group"] <- group
  currentyear <- crashdata[i, "Crash_Year"]
}

crashdata$month_group <- " "
currentyear <- 9999
currentmonth <- " "
group <- 0

for (i in 1:nrow(crashdata)) {
  if (crashdata[i, "Crash_Year"] != currentyear | crashdata[i, "Crash_Month"] != currentmonth)
    group <- group + 1
  crashdata[i, "month_group"] <- group
  currentyear <- crashdata[i, "Crash_Year"]
  currentmonth <- crashdata[i, "Crash_Month"] 
}


write.csv(crashdata, "C:/Users/lucin/OneDrive/Documents/GitHub/DATA7001Project/Data/locations_grouptest.csv")

