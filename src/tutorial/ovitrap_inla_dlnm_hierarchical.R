# clean the environment
rm(list=ls(all=TRUE))

# select an area between: NTS, NTN, KL, HK
areas <- c("HK", "KL", "NTN", "NTS")

data <- data.frame()

for (area_i in 1:length(areas)) {
  # load from csv file into 'data' variable
  filePath <- paste("../../dat/monthly_data_", areas[area_i], ".csv", sep="")
  dataArea_i <- read.csv(filePath, header=T)
  # clean data (esp. those with '#')
  for (fieldName in names(dataArea_i)) {
    dataArea_i[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", dataArea_i[[fieldName]]))
  }
  dataArea_i$area <- areas[area_i]
  for (row_i in 1:dim(dataArea_i)[1]) {
    row <- dataArea_i[row_i,]
    data <- rbind(data, row)
  }
}

library("INLA")
library("dlnm")
library("splines")

# create crossbasis variables
cb.meanTemperature <- crossbasis(data$temperature_avg, lag=2,
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(4, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=3,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(5, 2)))

getUpdatedColnames <- function(varName, oldColnames) {
  newColnames <- c()
  for (colname_i in 1:length(oldColnames)) {
    newColnames[colname_i] <- paste(varName,
                                    oldColnames[colname_i],
                                    sep="")
  }
  return (newColnames)
}

# update colnames
colnames(cb.meanTemperature) <- getUpdatedColnames("cb.meanTemperature", colnames(cb.meanTemperature))
colnames(cb.totalRainfall) <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))

inla.fit <- inla(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall + f(area, model="iid"),
                 data=data,
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T))


print(inla.fit$dic$dic)
print(inla.fit$waic$waic)

# get the posterior distribution of the random effects 1
# inla.fit$marginals.random$area$index.1
