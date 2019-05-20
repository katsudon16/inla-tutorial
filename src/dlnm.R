library("INLA")
library("dlnm")
library(splines)

chic <- chicagoNMMAPS
cb.temp <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="bs", degree=3, df=6), arglag=list(df=5))
cb.o3 <- crossbasis(chicagoNMMAPS$o3, lag=10, argvar=list(fun="thr", thr.value=40.3), arglag=list(fun="strata", breaks=c(2,6)))

glm.fit <- glm(death ~ cb.temp + cb.o3 + dow + ns(time, 7*14),
               family="quasipoisson",
               data=chicagoNMMAPS)
pred.temp <??? crosspred(cb.temp, glm.fit, by = 2)
plot(pred.temp)
pred.o3 <- crosspred(cb.o3, glm.fit, at = c(0:65, 40.3, 50.3))
plot(pred.o3)


plot(pred.temp, "contour", plot.title = title(xlab="Temperature", ylab="Lag", main="Contour Graph"), key.title=title("RR"))
