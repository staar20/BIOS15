dat = read.csv("datasets/dormancy.csv")

names(dat)
str(dat)
summary(dat)

plot(germ2 ~ timetosowing, data=dat,
     xlab="Time to sowing (days)",
     ylab="Germination proportion")

subdat = dat[dat$pop=="CC",]

germ = subdat$germ2 * subdat$nseed
notgerm = subdat$nseed - germ

mod1 = glm(cbind(germ, notgerm) ~ timetosowing,
           family="binomial", data=subdat)

coefs = coef(mod1)
t_50 = -coefs[1]/coefs[2]
t_50

summary(mod1)

x_pred = seq(min(subdat$timetosowing),
             max(subdat$timetosowing),
             by=0.1)

y_hat = predict(mod1,
                newdata=data.frame(timetosowing=x_pred),
                type="link")

p_hat = invlogit(y_hat)

plot(germ2 ~ timetosowing, data=subdat,
     xlab="Time to sowing (days)",
     ylab="Germination proportion",
     main="Population CC")

lines(x_pred, p_hat, col="red", lwd=2)

abline(h=0.5, lty=2)
abline(v=t_50, lty=2)

populations = unique(dat$pop)
t50_values = numeric(length(populations))

for (i in seq_along(populations)) {
  
  subdat = dat[dat$pop == populations[i], ]
  
  germ = subdat$germ2 * subdat$nseed
  notgerm = subdat$nseed - germ
  
  mod = glm(cbind(germ, notgerm) ~ timetosowing,
            family="binomial", data=subdat)
  
  coefs = coef(mod)
  t50_values[i] = -coefs[1]/coefs[2]
}

data.frame(population=populations, t50=t50_values)

mod_pop = glm(germ2 ~ timetosowing * pop,
              family="binomial",
              weights=nseed,
              data=dat)

summary(mod_pop)

plot(germ2 ~ timetosowing, data=dat,
     col=as.factor(pop), pch=16,
     xlab="Time to sowing (days)",
     ylab="Germination proportion")

cols = c("red", "blue", "darkgreen", "orange")

for (i in seq_along(populations)) {
  
  subdat = dat[dat$pop == populations[i], ]
  
  mod = glm(germ2 ~ timetosowing,
            family="binomial",
            weights=nseed,
            data=subdat)
  
  x_pred = seq(min(subdat$timetosowing),
               max(subdat$timetosowing), by=0.1)
  
  p_hat = predict(mod,
                  newdata=data.frame(timetosowing=x_pred),
                  type="response")
  
  lines(x_pred, p_hat, col=cols[i], lwd=2)
}

legend("topleft", legend=populations,
       col=cols, lwd=2)

plot(germ2 ~ MCseed, data=dat,
     xlab="Population-mean-centered seed mass (mg)",
     ylab="Germination proportion",
     col=as.factor(pop),
     pch=16)

legend("topright",
       legend=populations,
       col=1:length(populations),
       pch=16)

mod_pop = glm(germ2 ~ timetosowing * pop,
              family="binomial",
              weights=nseed,
              data=dat)

mod_seed = glm(germ2 ~ timetosowing * pop + MCseed,
               family="binomial",
               weights=nseed,
               data=dat)

summary(mod_seed)
anova(mod_pop, mod_seed, test="Chisq")

mod_seed_pop = glm(germ2 ~ timetosowing * pop +
                     MCseed * pop,
                   family="binomial",
                   weights=nseed,
                   data=dat)

summary(mod_seed_pop)
anova(mod_seed, mod_seed_pop, test="Chisq")

deviance(mod_seed) / df.residual(mod_seed)
length(unique(dat$mother))
length(unique(dat$blocktray))
length(unique(dat$crossID))

table(dat$pop)