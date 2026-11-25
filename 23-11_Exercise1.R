dat = read.csv("datasets/Eulaema.csv")
head(dat)

#A) Poisson distribution

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


#B) Negative binomial distribution
library(MASS)

#Use the same observations in all candidate models;
#eliminate the NAs (AI assisted)
dat_model = na.omit(dat[, c(
  "Eulaema_nigrita", "effort", "altitude", "MAT",
  "MAP", "Tseason", "Pseason", "forest.", "lu_het"
)])

#Candidate models
#"effort" was included in all models because it's a 
#methodological variable, not an ambiental factor

#Null model
m0 = glm.nb(Eulaema_nigrita ~ 1, data = dat_model)

#Sampling effort
m1 = glm.nb(Eulaema_nigrita ~ effort, data = dat_model)

#Topography
m2 = glm.nb(Eulaema_nigrita ~ effort + altitude,
            data = dat_model)

#Climate
m3 = glm.nb(Eulaema_nigrita ~ effort + MAT + MAP +
              Tseason + Pseason, data = dat_model)

#Land use
m4 = glm.nb(Eulaema_nigrita ~ effort + forest. + lu_het,
            data = dat_model)

#Climate and land use
m5 = glm.nb(Eulaema_nigrita ~ effort + MAT + MAP +
              Tseason + Pseason + forest. + lu_het,
            data = dat_model)

#Global model
m6 = glm.nb(Eulaema_nigrita ~ effort + altitude + MAT +
              MAP + Tseason + Pseason + forest. + lu_het,
            data = dat_model)

#Model selection based on AIC
mlist = list(Null = m0,
             Effort = m1,
             Topography = m2,
             Climate = m3,
             Land_use = m4,
             Climate_land_use = m5,
             Global = m6)

#Table construction
AICTab = AIC(m0, m1, m2, m3, m4, m5, m6)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]

#Calculate delta AIC
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)

#Calculate Akaike weights
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)

AICTab

#m6 shows the highest support
summary(m6)

#Plotting
library(ggplot2)
library(tidyr)

#Function to calculate model predictions (AI assisted)
Predictions = function(variable) {
  Values = seq(min(dat_model[[variable]]),
               max(dat_model[[variable]]),
               length.out = 100)
  
  NewData = data.frame(
    effort = mean(dat_model$effort),
    altitude = mean(dat_model$altitude),
    MAT = mean(dat_model$MAT),
    MAP = mean(dat_model$MAP),
    Tseason = mean(dat_model$Tseason),
    Pseason = mean(dat_model$Pseason),
    forest. = mean(dat_model$forest.),
    lu_het = mean(dat_model$lu_het))
  
  #Repeat the row 100 times
  NewData = NewData[rep(1, 100), ]
  
  #Replace the selected predictor with its 100 values
  NewData[, variable] = Values
  
  Pred = predict(m6, newdata = NewData,
                 type = "link", se.fit = TRUE)
  
  Data = data.frame(
    Variable = variable,
    Value = Values,
    Mean = exp(Pred$fit),
    Lower = exp(Pred$fit - 1.96 * Pred$se.fit),
    Upper = exp(Pred$fit + 1.96 * Pred$se.fit))
}

#Generating predictions for all variables
AllPredictions = rbind(Predictions("MAT"),
                       Predictions("MAP"),
                       Predictions("Tseason"),
                       Predictions("Pseason"),
                       Predictions("forest."),
                       Predictions("lu_het"))

#Conserving the observed data to represent the points (AI assisted)
ObservedData = pivot_longer(
  dat_model,
  cols = c(MAT, MAP, Tseason, Pseason, forest., lu_het),
  names_to = "Variable",
  values_to = "Value")

#Climatic data vs predictors (AI assisted)
ClimateData = subset(ObservedData,
                     Variable %in% c("MAT", "MAP", "Tseason", "Pseason"))
ClimatePredictions = subset(AllPredictions,
                            Variable %in% c("MAT", "MAP", "Tseason", "Pseason"))

#Actual plotting (climate)
ggplot() +
  geom_point(data = ClimateData,
             mapping = aes(x = Value, y = Eulaema_nigrita),
             alpha = 0.3) +
  geom_ribbon(data = ClimatePredictions,
              mapping = aes(x = Value, ymin = Lower, ymax = Upper),
              alpha = 0.2) +
  geom_line(data = ClimatePredictions,
            mapping = aes(x = Value, y = Mean),
            color = "darkgreen") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Effects of climatic predictors on bee abundance",
       x = "Predictor value",
       y = "Eulaema nigrita abundance") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("climate_plot.png", width = 30, height = 15,
       units = "cm", dpi = 300)

#Land use data vs predictors (AI assisted)
LandUseData = subset(ObservedData,
                     Variable %in% c("forest.", "lu_het"))
LandUsePredictions = subset(AllPredictions,
                            Variable %in% c("forest.", "lu_het"))
#Actual plotting (land use)
ggplot() +
  geom_point(data = LandUseData,
             mapping = aes(x = Value, y = Eulaema_nigrita),
             alpha = 0.3) +
  geom_ribbon(data = LandUsePredictions,
              mapping = aes(x = Value, ymin = Lower, ymax = Upper),
              alpha = 0.2) +
  geom_line(data = LandUsePredictions,
            mapping = aes(x = Value, y = Mean),
            color = "darkgreen") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Effects of land-use predictors on bee abundance",
       x = "Predictor value",
       y = "Eulaema nigrita abundance") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("land_use_plot.png", width = 30, height = 15,
       units = "cm", dpi = 300)