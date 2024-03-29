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
                               arglag=list(fun="ns", knots=logknots(5, 1)))

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

inla.fit <- inla(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall,
                 data=data,
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T))

inla.coef <- inla.fit$summary.fixed$mean
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix

# obtaining values relevant to cb.meanTemperature
cond <- paste("cb.meanTemperature","[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
indvcov <- grep(cond, rownames(inla.vcov))
indcoef <- grep(cond, rownames(inla.fit$summary.fixed))
inla.coef <- inla.coef[indcoef]
inla.vcov <- inla.vcov[indvcov,indvcov,drop=FALSE]

pred.meanTemperature <- crosspred(cb.meanTemperature, at=10:30, bylag=0.2,
                                  cumul=T, coef=inla.coef, vcov=inla.vcov)

plot(pred.meanTemperature, xlab="Temperature", zlab="Ovitrap Index", ylab="Lag")
plot(pred.meanTemperature, "contour",
     plot.title = title(xlab="Temperature", ylab="Lag", main="Contour Graph"),
     key.title=title("OI"))

print(inla.fit$dic$dic)
print(inla.fit$waic$waic)
