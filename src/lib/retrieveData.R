# by default: use changzhou data

getAnnualRelativeRisk <- function(filePath="../../dat/cases/hk_monthly_cases.csv") {
  allCases <- read.csv(filePath, header=T)
  allCases <- allCases[, c("Month", "Year", "Local.Cases")]
  # clean column titles
  names(allCases)[1] <- "month"
  names(allCases)[2] <- "year"
  names(allCases)[3] <- "localCases"
  allCases$relativeRisk <- 0

  avgCases <- 0
  minYear <- min(allCases$year, na.rm=TRUE)
  maxYear <- max(allCases$year, na.rm=TRUE)
  for (year in minYear:maxYear) {
    totalCases <- sum(allCases[allCases$year == year, "localCases"], na.rm=TRUE)
    allCases[allCases$year == year, "relativeRisk"] <- totalCases
    avgCases <- avgCases + totalCases
  }
  avgCases = avgCases / (maxYear - minYear + 1)
  allCases$relativeRisk = allCases$relativeRisk / avgCases
  return(allCases)
}

getAnnualRiskByArea <- function(filePath="../../dat/cases/hk_annual_cases_areas.xlsx") {
  library(openxlsx)
  areaRisk <- read.xlsx(filePath, startRow=1, colNames=TRUE)
  return(areaRisk)
}

getCasesByDistrict <- function(district, filePath="../../dat/cases/hk_annual_cases_district.csv") {
  cases <- read.csv(filePath, header=T)
  cases <- cases[, c("year", district)]
  return(cases)
}

readDFFromFile <- function(location="CC", filePath="../../dat/climate/HKCD.xlsx") {
  library(openxlsx)
  data <- read.xlsx(filePath,
                    sheet=paste("HKCD", location, sep=""),
                    startRow=1, colNames=TRUE, detectDates=TRUE)
  return(data)
}

getMonthlyTemperatureOnType <- function(type="mean", location="CC", colName="Daily.Mean.Temperature", filePath="../../dat/climate/HKCD.xlsx", df=NULL) {
  allClimates <- df
  if (is.null(allClimates)) {
    library(openxlsx)
    allClimates <- read.xlsx(filePath,
                             sheet=paste("HKCD", location, sep=""),
                             startRow=1, colNames=TRUE, detectDates=TRUE)
  }
  allClimates[[colName]] <- as.numeric(gsub("[^.0-9]", "", allClimates[[colName]]))
  if (type == "min") {
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), min, na.rm=TRUE)
  } else if (type == "mean") {
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), mean, na.rm=TRUE)
  } else {
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), max, na.rm=TRUE)
  }
  names(temp)[1] <- "month"
  names(temp)[2] <- "year"
  names(temp)[3] <- "temperature"
  temp$month_txt <- month.abb[temp$month]
  rm(list = c("allClimates"))
  return(temp)
}

getMonthlyRainfallOnType <- function(type="mean", filePath="../../dat/climate/HKCD.xlsx", location="CC", df=NULL) {
  allClimates <- df
  if (is.null(allClimates)) {
    library(openxlsx)
    allClimates <- read.xlsx(filePath,
                             sheet=paste("HKCD", location, sep=""),
                             startRow=1, colNames=TRUE, detectDates=TRUE)
  }
  allClimates$`Total.Rainfall.(mm)` <- as.numeric(gsub("[^.0-9]", "", allClimates$`Total.Rainfall.(mm)`))
  if (type == "min") {
    temp <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), min, na.rm=TRUE)
  } else if (type == "mean") {
    temp <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), mean, na.rm=TRUE)
  } else if (type == "max") {
    temp <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), max, na.rm=TRUE)
  } else {
    temp <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), sum, na.rm=TRUE)
  }
  names(temp)[1] <- "month"
  names(temp)[2] <- "year"
  names(temp)[3] <- "rainfall"
  temp$month_txt <- month.abb[temp$month]
  rm(list = c("allClimates"))
  return(temp)
}

getMonthlyRainFall <- function(filePath="../../dat/climate/changzhou_climate(clean).csv") {
  monthlyClimates <- read.csv(filePath, header=T)
  rainfall <- monthlyClimates[,c("month", "year", "totalrain")]
  rainfall$totalrain <- as.numeric(gsub("[^.0-9]", "", rainfall$totalrain))
  rm(list = c("monthlyClimates"))
  return(rainfall)
}

getOvitrapIndex <- function(filePath="data/ovitrap_CC.csv") {
  ovitrap <- read.csv(filePath, header=T)
  names(ovitrap) <- c("year", "month", "percentage")
  ovitrap$month_txt <- month.abb[ovitrap$month]
  return(ovitrap)
}

extractSeasonalData <- function(dataset, field="totalrain", isPreseason=TRUE) {
  # pre season: jan - jun, in season = jul - aug
  temp <- data.frame()
  if (isPreseason) {
    temp <- dataset[dataset$month <= 6,]
  } else {
    temp <- dataset[dataset$month >= 7 & dataset$month <= 8,]
  }
  seasonData <- list()
  seasonData$mean <- mean(temp[[field]], na.rm=TRUE)
  lgFormula <- formula(paste(field, "~ month", sep=""))
  lg <- lm(formula=lgFormula, data=temp)
  seasonData$grad <- lg$coefficients[2]
  return(seasonData)
}

# extract climate data frame in form of monthly temperature, rainfall, and annual cases table
# @params:
# - temperatureField (the selected temperature column): "mean", "absMin", or "absMax"
# - temperatureType (the temperature aggregate type)  : "mean', "max", or "min"
# - rainfallType (the rainfall aggregate type)        : "total" or "max"
# - areas (areas selected for the model), default=c() : e.g., c("HKL", "NTS", "NTN) 
extractAnnualClimateData <- function (temperatureField="mean", temperatureType="mean",
                                      rainfallType="total",
                                      areas=c(), minYear=2002, maxYear=2018) {
  temperatureColLabels <- c(
    mean="Daily.Mean.Temperature",
    absMax="Absolute.Daily.Max.Temperature",
    absMin="Absolute.Daily.Min.Temperature"
  )
  temperatureColLabel <- temperatureColLabels[temperatureField]
  rainfallColLabel <- "Total.Rainfall.(mm)"
  climateFile <- "../../dat/climate/HKCD_areas.xlsx"

  areaRisk <- getAnnualRiskByArea()

  areasT <- list()
  areasR <- list()
  for (area in areas) {
    data <- readDFFromFile(area, filePath=climateFile)
    areasT[[area]] <- getMonthlyTemperatureOnType(type=temperatureType,
                                                  colName=temperatureColLabel,
                                                  df=data)
    areasR[[area]] <- getMonthlyRainfallOnType(type=rainfallType,
                                               location=area,
                                               df=data)
  }

  df <- data.frame()
  for (year in minYear:maxYear) {
    for (area_i in 1:length(areas)) {
      area <- areas[area_i]
      Tdata <- areasT[[area]]
      Rdata <- areasR[[area]]
      T <- 0
      R <- 0
      risk <- areaRisk[areaRisk$year == year, area]
      for (month in 1:8) {
        T[month] <- Tdata[Tdata$month==month & Tdata$year==year, "temperature"]
        R[month] <- Rdata[Rdata$month==month & Rdata$year==year, "rainfall"]
      }
      # missing data result in -Inf
      if (length(R[!is.finite(R)]) > 0) next
      df <- rbind(df, c(area_i, year, risk, T, R))
    }
  }
  names(df) <- c("AREA", "YEAR", "RISK",
                 "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                 "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8")
  df$AREA <- as.factor(df$AREA)
  return(df)
}
