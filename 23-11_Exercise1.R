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

# Calculate Akaike weights
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)

AICTab

#m6 shows the highest support
summary(m6)
