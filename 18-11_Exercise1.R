invlogit = function(x) 1/(1+exp(-x))

x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2)
p = invlogit(eta)
y = rbinom(200, 1, p)

m = glm(y ~ x, family = binomial)

coefs = summary(m)$coef
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)

plot(x, y, las=1)

#Compute the value of x corresponding to a probability of 0.5
#coefs[1,1] = intercept (B0); coefs[2,1] = slope (B1)
#eta = B0 + B1x
#p = 0,5 -> eta = 0
#0 = B0 + B1x
#x = -B0/B1
x_05 = -coefs[1,1]/coefs[2,1]

#Add lines to the plot to illustrate the results
#x = calculated value
abline(v=x_05, lty=2)

#predicted probabilities of y = 1 for each value of x.
#(how the probability of the event changes as the predictor variable x increases)
lines(x_pred, p_hat, col="red", lwd=2)