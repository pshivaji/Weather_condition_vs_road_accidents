library(readr)
library(dplyr)
library(lubridate)

crashdata <- read_csv("C:/Users/lucin/OneDrive/Documents/GitHub/DATA7001Project/Data/locations_grouptest.csv")
crashdata$ForwardImputedDate <- as.POSIXct(0, origin = "1960-01-01") 
crashdata$ReverseImputedDate <- as.POSIXct(0, origin = "1960-01-01") 
crashdata$DateImputionError <- ""
crashdata$DateImputionAgreement <- NULL

# summarise group stats
groupcount<- crashdata %>%
  group_by(regional_group) %>%
  summarize('grouptotal' = n())

monthcount <- crashdata %>%
  group_by(month_group) %>%
  summarize('monthtotal' = n())

monthaverage <- monthcount %>%
   summarize(mean(monthtotal), median(monthtotal), max(monthtotal), min(monthtotal))

View(monthaverage)

# Uncomment when testing to reduce set size
#crashdata <- crashdata[1:1000,]
#dim(crashdata)

crashdata_split <- split(crashdata, crashdata$month_group)
dim(crashdata_split)

#Imput dates forward
for (i in 1:length(crashdata_split)) {
  currentMonth <- crashdata_split[[i]][1,'Crash_Month']
  currentYear <- crashdata_split[[i]][1,'Crash_Year']
  currentDay <- as.POSIXct(paste(currentYear, currentMonth, "1", sep='/'), tz="", format = "%Y/%B/%d")
  for (j in 1:nrow(crashdata_split[[i]])) {
    dateallocated <- FALSE
    
    while (!dateallocated) {
      if (weekdays(currentDay) == crashdata_split[[i]][j,"Crash_Day_Of_Week"]) {
        crashdata_split[[i]][j,"ForwardImputedDate"] <- currentDay
        dateallocated <- TRUE
      } 
      else {
        currentDay <- currentDay + 60 * 60 * 24
      }
        
        
      if (format(currentDay,"%B") != currentMonth) {
        crashdata_split[[i]][j,"DateImputionError"] <- "Allocation exceeded month"
      }
    }
  }
}

#IMpute dates in reverse
for (i in 1:length(crashdata_split)) {
  currentMonth <- crashdata_split[[i]][1,'Crash_Month']
  currentYear <- crashdata_split[[i]][1,'Crash_Year']
  # Set current day to last day of the month
  currentDay <- ceiling_date(as.POSIXct(paste(currentYear, currentMonth, "2", sep='/'), tz="", format = "%Y/%B/%d"), "month") - 60 * 60 * 24 
  for (j in nrow(crashdata_split[[i]]):1) {
    dateallocated <- FALSE
    
    while (!dateallocated) {
      if (weekdays(currentDay) == crashdata_split[[i]][j,"Crash_Day_Of_Week"]) {
        crashdata_split[[i]][j,"ReverseImputedDate"] <- currentDay
        dateallocated <- TRUE
      } else
      {
        # Cycle backwards through the month
        currentDay <- currentDay - 60 * 60 * 24
      }
      if (format(currentDay,"%B") != currentMonth) {
        crashdata_split[[i]][j,"DateImputionError"] <- "Allocation exceeded month"
      }
    }
  }
}

#Compare imputed dates
for (i in 1:length(crashdata_split)) {
   for (j in 1:nrow(crashdata_split[[i]])) {
    if (crashdata_split[[i]][j,"ForwardImputedDate"] == crashdata_split[[i]][j,"ReverseImputedDate"])
      crashdata_split[[i]][j,"DateImputionAgreement"] <- TRUE
    else
      crashdata_split[[i]][j,"DateImputionAgreement"] <- FALSE

  }
}

crashdata_unsplit <- bind_rows(crashdata_split)
dim(crashdata_unsplit)
write.csv(crashdata_unsplit, "C:/Users/lucin/OneDrive/Documents/GitHub/DATA7001Project/Data/imputed_dates.csv")

