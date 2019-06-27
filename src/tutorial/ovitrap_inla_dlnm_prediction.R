# clean the environment
rm(list=ls(all=TRUE))

# select an area between: NTS, NTN, KL, HK
area <- "NTS"

# load from csv file into 'data' variable
filePath <- paste("../../dat/monthly_data_", area, ".csv", sep="")
data <- read.csv(filePath, header=T)

# clean data (esp. those with '#')
for (fieldName in names(data)) {
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]]))
}

library("INLA")
library("dlnm")
library("splines")

# create crossbasis variables
cb.meanTemperature <- crossbasis(data$temperature_avg, lag=8,
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(4, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=5,
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

getRelevantCoefAndVcov <- function(varName, summary.fixed, coef, vcov) {
  cond <- paste(varName,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
  indvcov <- grep(cond, rownames(vcov))
  indcoef <- grep(cond, rownames(summary.fixed))
  coef <- coef[indcoef]
  vcov <- vcov[indvcov,indvcov,drop=FALSE]
  return (list("coef"=coef, "vcov"=vcov))
}

# update colnames
colnames(cb.meanTemperature) <- getUpdatedColnames("cb.meanTemperature", colnames(cb.meanTemperature))
colnames(cb.totalRainfall) <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))

inla.fit <- inla(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall,
                 data=data,
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T, config=T))

inla.coef <- inla.fit$summary.fixed$mean
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix

# obtain coef and vcov for each crossbasis variables
info.meanTemperature <- getRelevantCoefAndVcov("cb.meanTemperature", inla.fit$summary.fixed,
                                               inla.coef, inla.vcov)
info.totalRainfall <- getRelevantCoefAndVcov("cb.totalRainfall", inla.fit$summary.fixed,
                                             inla.coef, inla.vcov)


# manual prediction
# initiate with NA (120 in total)
predicted <- rep(NA, 120)
inla.intercept <- inla.fit$summary.fixed$mean[1]

# avoid NA between 1 - 8
for (row_i in 9:120) {
  temp.row <- cb.meanTemperature[row_i,]
  rain.row <- cb.totalRainfall[row_i,]
  
  # following the formula ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall ( + intercept)
  predicted[row_i] <- sum(temp.row * info.meanTemperature$coef) +
                      sum(rain.row * info.totalRainfall$coef) + inla.intercept
}
predicted[predicted < 0] <- 0
plot(data$ovitrap_idx, type="l", col="red")
lines(predicted, col="blue")