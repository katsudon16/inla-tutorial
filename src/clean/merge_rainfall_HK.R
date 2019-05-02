# Merge HK rainfall
# HV, Cape, Quarry Bay
# Creates an excel file containing the merge result

rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")

# daily data
excelFile <- "../../dat/climate/HKCD_test.xlsx"
daily <- read.xlsx("../../dat/climate/HK_rainfall.xlsx",
                   sheet="Daily",
                   startRow=1, colNames=TRUE, detectDates=TRUE)
daily$Cape <- as.numeric(gsub("[^.0-9]", "", daily$Cape))
daily$Quarry.Bay <- as.numeric(gsub("[^.0-9]", "", daily$Quarry.Bay))
dailydf <- data.frame()

for (i in 1:dim(daily)[1]) {
  totalValid <- ifelse(is.na(daily$HV[i]), 0, 1) +
                ifelse(is.na(daily$Cape[i]), 0, 1) +
                ifelse(is.na(daily$Quarry.Bay[i]), 0, 1)
  sumValid <- ifelse(is.na(daily$HV[i]), 0, daily$HV[i]) +
              ifelse(is.na(daily$Cape[i]), 0, daily$Cape[i]) +
              ifelse(is.na(daily$Quarry.Bay[i]), 0, daily$Quarry.Bay[i])
  rainfall <- sumValid / totalValid
  dailydf <- rbind(dailydf, c(daily$Year[i], daily$Month[i], daily$Day[i], rainfall))
}

names(dailydf) <- c("Year", "Month", "Day", "rainfall")
write.xlsx(dailydf, excelFile, append=TRUE)


# monthly data
excelFile <- "../../dat/climate/HKCD_test.xlsx"
monthly <- read.xlsx("../../dat/climate/HK_rainfall.xlsx",
                   sheet="Monthly",
                   startRow=1, colNames=TRUE, detectDates=TRUE)
monthly$HV <- as.numeric(gsub("[^.0-9]", "", monthly$HV))
monthly$Cape <- as.numeric(gsub("[^.0-9]", "", monthly$Cape))
monthly$Quarry.Bay <- as.numeric(gsub("[^.0-9]", "", monthly$Quarry.Bay))
monthlydf <- data.frame()

for (i in 1:dim(monthly)[1]) {
  totalValid <- ifelse(is.na(monthly$HV[i]), 0, 1) +
    ifelse(is.na(monthly$Cape[i]), 0, 1) +
    ifelse(is.na(monthly$Quarry.Bay[i]), 0, 1)
  sumValid <- ifelse(is.na(monthly$HV[i]), 0, monthly$HV[i]) +
    ifelse(is.na(monthly$Cape[i]), 0, monthly$Cape[i]) +
    ifelse(is.na(monthly$Quarry.Bay[i]), 0, monthly$Quarry.Bay[i])
  rainfall <- sumValid / totalValid
  monthlydf <- rbind(monthlydf, c(monthly$year[i], monthly$month[i], rainfall))
}

names(monthlydf) <- c("Year", "Month", "rainfall")
write.xlsx(monthlydf, excelFile, append=TRUE)
