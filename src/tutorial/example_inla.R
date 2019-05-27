library("INLA") # assuming the package has already been installed

# First, define the formula
formula <- Petal.Length ~ 1 + Petal.Width
# Then run the INLA algorithm
output <- inla(formula, family="gaussian", data=iris)
summary(output)

beta1_posterior <- output$marginals.fixed$Petal.Width
# plot the marginal posterior of beta_1
plot(beta1_posterior, type="l",
     xlab=expression(beta[1]),
     ylab=expression(tilde(p)(paste(beta[1], "|", y))))