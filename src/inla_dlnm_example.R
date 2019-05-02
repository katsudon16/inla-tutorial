library("INLA")
library("dlnm")
library(splines)

chic <- chicagoNMMAPS
cb1.pm <- crossbasis(chic$pm10, lag=15, argvar=list(fun="lin"), arglag=list(fun="poly", degree=5))
cb1.o3 <- crossbasis(chic$o3, lag=10, argvar=list(fun="lin"), arglag=list(fun="poly", degree=4))

# update colnames
colnames(cb1.pm) <- c("cb1.pmv1.l1", "cb1.pmv1.l2", "cb1.pmv1.l3", "cb1.pmv1.l4", "cb1.pmv1.l5", "cb1.pmv1.l6")
colnames(cb1.o3) <- c("cb1.o3v1.l1", "cb1.o3v1.l2", "cb1.o3v1.l3", "cb1.o3v1.l4", "cb1.o3v1.l5")

inla.fit <- inla(death ~ dow + cb1.pm + cb1.o3,
                 family="poisson",
                 data=chic,
                 control.fixed=list(correlation.matrix=T))
# glm.fit <- glm(death ~ cb1.pm + dow,
#                  family="poisson",
#                  data=chic)
inla.coef <- inla.fit$summary.fixed$mean
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix

# crosspred for pm3
cond <- paste("cb1.pm","[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
indvcov <- grep(cond, rownames(inla.vcov))
indcoef <- grep(cond, rownames(inla.fit$summary.fixed))
inla.coef <- inla.coef[indcoef]
inla.vcov <- inla.vcov[indvcov,indvcov,drop=FALSE]

pred1.pm <- crosspred(cb1.pm, at=0:20, bylag=0.2, cumul=T, coef=inla.coef, vcov=inla.vcov)
# pred1.pm <- crosspred(cb1.pm, model=inla.fit, at=0:20, bylag=0.2, cumul=T)
