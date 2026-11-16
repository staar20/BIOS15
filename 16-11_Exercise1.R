plants = read.csv("datasets/alpineplants.csv")
names(plants)
summary(plants)
str(plants)

#Boxplots to see possible outliers
par(mfrow=c(2,5), mar=c(2,2,2,1))
for(i in 3:12){
  boxplot(plants[[i]],
          main=names(plants)[i],
          col="lightgray")
}

#Quartile analysis to find the outliers and decide if removing them or not
for(i in 3:12){
  x = plants[[i]]
  
  Q1 = quantile(x, 0.25, na.rm=TRUE)
  Q3 = quantile(x, 0.75, na.rm=TRUE)
  IQR_value = IQR(x, na.rm=TRUE)
  
  lower = Q1 - 1.5*IQR_value
  upper = Q3 + 1.5*IQR_value
  
  outliers = which(x < lower | x > upper)
  
  if(length(outliers) > 0){
    print(names(plants)[i])
    print(outliers)
  }
}

#Outliers found
#L5 - Max_T_winter = 4,5
#L64 - Max_T_summer = 27,25
#L65 - Soil_moist = 41
#None of the outliers seemed obviously impossible so none of them was removed
#Maybe do the model with and without the outlier if really unsure

#Hypothesis 1: Mean T winter
m_carex_meanTw = lm(Carex.bigelowii ~ mean_T_winter, data=plants)
summary(m_carex_meanTw)
par(mfrow=c(2,2))
plot(m_carex_meanTw)
#residuals NOT normally distributed

m_thali_meanTw = lm(Thalictrum.alpinum ~ mean_T_winter, data=plants)
summary(m_thali_meanTw)
par(mfrow=c(2,2))
plot(m_thali_meanTw)
#residuals NOT normally distributed

#Hypothesis 2: Max T winter
m_carex_maxTw = lm(Carex.bigelowii ~ max_T_winter, data=plants)
summary(m_carex_maxTw)
par(mfrow=c(2,2))
plot(m_carex_maxTw)
#residuals NOT normally distributed

m_thali_maxTw = lm(Thalictrum.alpinum ~ max_T_winter, data=plants)
summary(m_thali_maxTw)
par(mfrow=c(2,2))
plot(m_thali_maxTw)
#residuals NOT normally distributed

#Hypothesis 3: Min T winter
m_carex_minTw = lm(Carex.bigelowii ~ min_T_winter, data=plants)
summary(m_carex_minTw)
par(mfrow=c(2,2))
plot(m_carex_minTw)
#residuals KINDA normally distributed

m_thali_minTw = lm(Thalictrum.alpinum ~ min_T_winter, data=plants)
summary(m_thali_minTw)
par(mfrow=c(2,2))
plot(m_thali_minTw)
#residuals NOT normally distributed

#Hypothesis 4: Mean T summer
m_carex_meanTs = lm(Carex.bigelowii ~ mean_T_summer, data=plants)
summary(m_carex_meanTs)
par(mfrow=c(2,2))
plot(m_carex_meanTs)
#residuals NOT normally distributed

m_thali_meanTs = lm(Thalictrum.alpinum ~ mean_T_summer, data=plants)
summary(m_thali_meanTs)
par(mfrow=c(2,2))
plot(m_thali_meanTs)
#residuals KINDA normally distributed

#Hypothesis 5: Max T summer
m_carex_maxTs = lm(Carex.bigelowii ~ max_T_summer, data=plants)
summary(m_carex_maxTs)
par(mfrow=c(2,2))
plot(m_carex_maxTs)
#residuals NOT normally distributed

m_thali_maxTs = lm(Thalictrum.alpinum ~ max_T_summer, data=plants)
summary(m_thali_maxTs)
par(mfrow=c(2,2))
plot(m_thali_maxTs)
#residuals NOT normally distributed

#Hypothesis 6: Min T summer
m_carex_minTs = lm(Carex.bigelowii ~ min_T_summer, data=plants)
summary(m_carex_minTs)
par(mfrow=c(2,2))
plot(m_carex_minTs)
#residuals KINDA normally distributed

m_thali_minTs = lm(Thalictrum.alpinum ~ min_T_summer, data=plants)
summary(m_thali_minTs)
par(mfrow=c(2,2))
plot(m_thali_minTs)
#residuals KINDA normally distributed

#Hypothesis 7: Light
m_carex_light = lm(Carex.bigelowii ~ light, data=plants)
summary(m_carex_light)
par(mfrow=c(2,2))
plot(m_carex_light)
#residuals NOT normally distributed

m_thali_light = lm(Thalictrum.alpinum ~ light, data=plants)
summary(m_thali_light)
par(mfrow=c(2,2))
plot(m_thali_light)
#residuals KINDA normally distributed

#Hypothesis 8: Snow
m_carex_snow = lm(Carex.bigelowii ~ snow, data=plants)
summary(m_carex_snow)
par(mfrow=c(2,2))
plot(m_carex_snow)
#residuals KINDA normally distributed

m_thali_snow = lm(Thalictrum.alpinum ~ snow, data=plants)
summary(m_thali_snow)
par(mfrow=c(2,2))
plot(m_thali_snow)
#residuals NOT normally distributed

#Hypothesis 9: Soil moist
m_carex_soil = lm(Carex.bigelowii ~ soil_moist, data=plants)
summary(m_carex_soil)
par(mfrow=c(2,2))
plot(m_carex_soil)
#residuals KINDA normally distributed

m_thali_soil = lm(Thalictrum.alpinum ~ soil_moist, data=plants)
summary(m_thali_soil)
par(mfrow=c(2,2))
plot(m_thali_soil)
#residuals KINDA normally distributed

#Hypothesis 10: Altitude
m_carex_alt = lm(Carex.bigelowii ~ altitude, data=plants)
summary(m_carex_alt)
par(mfrow=c(2,2))
plot(m_carex_alt)
#residuals NOT normally distributed

m_thali_alt = lm(Thalictrum.alpinum ~ altitude, data=plants)
summary(m_thali_alt)
par(mfrow=c(2,2))
plot(m_thali_alt)
#residuals NOT normally distributed

#AI asisted
models = list(
  Carex_meanTw = m_carex_meanTw,
  Thali_meanTw = m_thali_meanTw,
  Carex_maxTw = m_carex_maxTw,
  Thali_maxTw = m_thali_maxTw,
  Carex_minTw = m_carex_minTw,
  Thali_minTw = m_thali_minTw,
  Carex_meanTs = m_carex_meanTs,
  Thali_meanTs = m_thali_meanTs,
  Carex_maxTs = m_carex_maxTs,
  Thali_maxTs = m_thali_maxTs,
  Carex_minTs = m_carex_minTs,
  Thali_minTs = m_thali_minTs,
  Carex_light = m_carex_light,
  Thali_light = m_thali_light,
  Carex_snow = m_carex_snow,
  Thali_snow = m_thali_snow,
  Carex_soil = m_carex_soil,
  Thali_soil = m_thali_soil,
  Carex_alt = m_carex_alt,
  Thali_alt = m_thali_alt
)

results = data.frame(
  Slope = sapply(models, function(m) coef(m)[2]),
  p_value = sapply(models, function(m) summary(m)$coefficients[2,4]),
  R_squared = sapply(models, function(m) summary(m)$r.squared)
)

results

#Correlation test (>0,7)
environment = plants[, c("mean_T_winter", "max_T_winter",
                         "min_T_winter", "mean_T_summer",
                         "max_T_summer", "min_T_summer",
                         "light", "snow", "soil_moist", "altitude")]

cor(environment, use="pairwise.complete.obs")
#mean_T_winter is cor to min_T_winter and snow
#max_T_winter is NOT cor
#min_T_winter is cor to mean_T_winter and snow
#mean_T_summer is cor to max_T_summer and altitude
#max_T_summer is cor to mean_T_summer
#min_T_summer is NOT cor
#light is NOT cor
#snow is cor to mean_T_winter and min_T_winter
#soil_moist is NOT cor
#altitude is cor to mean_T_summer

#Final model build
m_carex_final = lm(Carex.bigelowii ~ mean_T_winter +
                     min_T_summer + altitude,
                   data=plants)
summary(m_carex_final)
par(mfrow=c(2,2))
plot(m_carex_final)

m_thali_final = lm(Thalictrum.alpinum ~ mean_T_winter + 
                     mean_T_summer + min_T_summer + light +
                     soil_moist,
                   data=plants)
summary(m_thali_final)

par(mfrow=c(2,2))
plot(m_thali_final)

#Statistical approach: full models
m_carex_full = lm(Carex.bigelowii ~ mean_T_summer +
                    min_T_summer + altitude,
                  data=plants)
summary(m_carex_full)

#min_T_winter is not significant p = 0,90
#max_T_winter is not significant p = 0,90
#light is not significant p = 0,88
#snow is not significant p = 0,59
#mean_T_winter is not significant p = 0,58
#soil_moist is not significant p = 0,62
#max_T_summer is not significant p = 0,21

m_thali_full = lm(Thalictrum.alpinum ~ mean_T_winter +
                         mean_T_summer + max_T_summer +
                         min_T_summer,
                       data=plants)
summary(m_thali_full)

#min_T_winter is not significant p = 0,76
#max_T_winter is not significant p = 0,67
#soil_moist is not significant p = 0,62
#altitude is not significant p = 0,30
#snow is not significant p = 0,12
#light is not significant p = 0,13

#FINAL CONCLUSSIONS - Comparison between biological and statistical approaches

# Carex bigelowii:
# Both approaches include min_T_summer and altitude.
# The biological approach includes mean_T_winter,
# whereas backward selection includes mean_T_summer.

# Thalictrum alpinum:
# Both approaches include mean_T_winter, mean_T_summer
# and min_T_summer.
# The biological approach additionally includes light and soil_moist,
# whereas backward selection includes max_T_summer.
