dat = read.csv("datasets/Eulaema.csv")
head(dat)

#A.Poisson distribution

#Given model
#m1 = glm(Eulaema_nigrita ~ effort + altitude + MAT,
          #"poisson", data=dat)
#Overdispersion trial
#disp1 = sum(residuals(m1, type = "pearson")^2) / df.residual(m1)
#disp1

#Extended model
#m2 = glm(Eulaema_nigrita ~ effort + altitude + MAT +
          #MAP + Tseason + Pseason + forest. + lu_het,
          #"poisson", data = dat)
#Overdispersion trial
#disp2 = sum(residuals(m2, type = "pearson")^2) / df.residual(m2)
#disp2

#Both showed substantial overdispersion, therefore,
#we considered instead the negative binomial distribution

#B.Negative binomial distribution

library(MASS)

#Given model
#m1_nb = glm.nb(Eulaema_nigrita ~ effort + altitude + MAT,
                #data = dat)

#Extended model
m2_nb = glm.nb(Eulaema_nigrita ~ effort + altitude + MAT +
               MAP + Tseason + Pseason + forest. + lu_het,
               data = dat)
summary(m2_nb)

#Model selection based on AIC comparison:
#The extended model one is lower (1777.9 vs 1845.8),
#therefore we selected m2_nb.

#Exponentiated coefficients
exp(coef(m2_nb))

#Plotting
ggplot(
  data = AllData,
  mapping = aes (x = LarvalHost, y = Mean, color = MaternalHost)
) +
  geom_point() +
  geom_line(aes(group = MaternalHost)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1, color = "black") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(
    title = "Effects of maternal and larval host plants on three biological traits",
    x = "Larval host genus",
    y = "Mean response",
    color = "Maternal host genus"
  ) +
  #AI assisted
  theme(plot.title = element_text(hjust = 0.5))

ggsave("butterflies_plot.png", width = 30, height = 12.5, units = "cm", dpi = 300)



#Plotting

plot_effect = function(variable, x_label) {
  
  predictors = c("effort", "altitude", "MAT", "MAP",
                 "Tseason", "Pseason", "forest.", "lu_het")
  
  # Keep all predictors at their mean values
  newdata = as.data.frame(
    lapply(dat[, predictors], mean, na.rm = TRUE)
  )
  
  # Allow the selected predictor to vary
  newdata = newdata[rep(1, 100), ]
  newdata[[variable]] = seq(
    min(dat[[variable]], na.rm = TRUE),
    max(dat[[variable]], na.rm = TRUE),
    length.out = 100
  )
  
  # Calculate model predictions
  pred = predict(m2_nb, newdata = newdata,
                 type = "link", se.fit = TRUE)
  
  pred_mean = exp(pred$fit)
  pred_lower = exp(pred$fit - 1.96 * pred$se.fit)
  pred_upper = exp(pred$fit + 1.96 * pred$se.fit)
  
  # Create plot
  plot(dat[[variable]], dat$Eulaema_nigrita,
       type = "n",
       xlab = x_label,
       ylab = "Eulaema nigrita abundance",
       main = x_label,
       ylim = range(c(dat$Eulaema_nigrita,
                      pred_lower, pred_upper)))
  
  polygon(c(newdata[[variable]], rev(newdata[[variable]])),
          c(pred_lower, rev(pred_upper)),
          col = "lightgreen", border = NA)
  
  points(dat[[variable]], dat$Eulaema_nigrita,
         pch = 16, col = rgb(0, 0, 0, 0.25))
  
  lines(newdata[[variable]], pred_mean,
        col = "darkgreen", lwd = 2)
}

par(mfrow = c(2, 2))

plot_effect("MAT", "Mean annual temperature")
plot_effect("MAP", "Mean annual precipitation")
plot_effect("Tseason", "Temperature seasonality")
plot_effect("Pseason", "Precipitation seasonality")

par(mfrow = c(1, 1))

par(mfrow = c(1, 2))

plot_effect("forest.", "Forest cover")
plot_effect("lu_het", "Land-use heterogeneity")

par(mfrow = c(1, 1))