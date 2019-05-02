# Merge districts into 5 areas: NTW, NTE, NTS, KL, HK
# Creates an excel file containing the merge result (monthly climate data)

rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
library("crayon")

#---------USER INPUTS-------------
# districts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")
districts <- c("KP", "HK") # to be merged
# output file
excelFile <- "d:/workspace/dengue-forecast-hk/dat/climate/HKCD_test.xlsx"
sheetName <- "HK"
#---------------------------------

data <- list()
for (d in districts) {
  df <- read.xlsx("../../dat/climate/HKCM.xlsx",
                  sheet=d,
                  startRow=1, colNames=TRUE, detectDates=TRUE)
  data[[d]] <- df
}

isDateValid <- function(i) {
  isDateSame <- TRUE
  date <- list()
  for (d_i in 1:length(districts)) {
    district <- districts[d_i]
    if (d_i == 1) {
      date <- list(year=data[[district]]$year[i],
                   month=data[[district]]$month[i])
      next
    }
    if (data[[district]]$year[i] != date$year |
        data[[district]]$month[i] != date$month) {
      isDateSame <- FALSE
      break
    }
  }
  return(isDateSame)
}

getTotalDays <- function(year, month) {
  oddMonths <- c(1, 3, 5, 7, 8, 10, 12)
  if (month %in% oddMonths) return(31)
  if (month == 2) {
    if (year %% 400 == 0) return(28)
    if (year %% 4 == 0) {
      if (year %% 100 == 0) return(29)
      return(28)
    }
    return(29)
  }
  return(30)
}

getAverageRow <- function(i) {
  row <- list()
  avgCols <- c("pressure", "avgmax", "avg", "avgmin", "dewpoint",
               "relhumidity", "totalrain", "winddic", "windspeed")
  for (col_i in 1:length(colnames(data[[districts[1]]]))) {
    col <- colnames(data[[districts[1]]])[col_i]
    if (col == "year" | col == "month") {
      row[col_i] <- data[[districts[1]]][[col]][i]
      next
    }
    avg <- 0
    totalDays <- getTotalDays(data[[districts[1]]][["year"]][i],
                              data[[districts[1]]][["month"]][i])
    totalValidDays <- 0
    maxVal <- -1000
    minVal <- 1000
    isNA <- TRUE
    for (d_i in 1:length(districts)) {
      if (is.na(data[[districts[d_i]]][[col]][i])) next
      isNA <- FALSE
      if (col %in% avgCols) {
        if (districts[d_i] == "HK") {
          if (col == "totalrain") {
            totalValidDays <- totalValidDays + 4 * totalDays
            avg <- avg + totalDays * 4 * data[[districts[d_i]]][[col]][i]
          } else {
            totalValidDays <- totalValidDays + 2 * totalDays
            avg <- avg + totalDays * 2 * data[[districts[d_i]]][[col]][i]
          }
        } else {
          totalValidDays <- totalValidDays + totalDays
          avg <- avg + totalDays * data[[districts[d_i]]][[col]][i]
        }
      } else if (col == "absmax") {
        maxVal <- max(maxVal, data[[districts[d_i]]][[col]][i])
      } else {
        minVal <- min(minVal, data[[districts[d_i]]][[col]][i])
      }
    }
    if (!isNA) {
      avg <- avg / totalValidDays
    } else {
      avg <- NA
      maxVal <- NA
      minVal <- NA
    }
    if (col %in% avgCols) {
      row[col_i] <- avg
    } else if (col == "absmax") {
      row[col_i] <- maxVal
    } else {
      row[col_i] <- minVal
    }
  }
  return(row)
}

avg_df <- data.frame()
# clean
for (d in districts) {
  for (col in colnames(data[[districts[1]]])) {
    data[[d]][col] <- as.numeric(gsub("[^.0-9]", "", data[[d]][col][,]))
  }
}
for (i in 1:dim(data[[districts[1]]])[1]) {
  if (!isDateValid(i)) {
    cat(red("Date is not the same..."))
    break
  }
  row <- getAverageRow(i)
  avg_df <- rbind(avg_df, row)
}
names(avg_df) <- names(data[[districts[1]]])
write.xlsx(avg_df, excelFile, sheetName=sheetName, append=TRUE)
