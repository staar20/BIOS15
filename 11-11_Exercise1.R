dat = read.csv("datasets/butterflies.csv")
names(dat)

#Development Time
m1 = lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)
anova(m1)
means1 = tapply(dat$DevelopmentTime,
               list(dat$MaternalHost, dat$LarvalHost), mean)
sd1 = tapply(dat$DevelopmentTime,
                list(dat$MaternalHost, dat$LarvalHost), sd)
n1 = tapply(dat$DevelopmentTime,
                list(dat$MaternalHost, dat$LarvalHost), length)
SE1 = sd1/sqrt(n1)

#Adult Weight
m2 = lm(AdultWeight ~ MaternalHost * LarvalHost, data = dat)
anova(m2)
means2 = tapply(dat$AdultWeight,
                list(dat$MaternalHost, dat$LarvalHost), mean)
sd2 = tapply(dat$AdultWeight,
             list(dat$MaternalHost, dat$LarvalHost), sd)
n2 = tapply(dat$AdultWeight,
            list(dat$MaternalHost, dat$LarvalHost), length)
SE2 = sd2/sqrt(n2)

#Growth Rate
m3 = lm(GrowthRate ~ MaternalHost * LarvalHost, data = dat)
anova(m3)
means3 = tapply(dat$GrowthRate,
                list(dat$MaternalHost, dat$LarvalHost), mean)
sd3 = tapply(dat$GrowthRate,
             list(dat$MaternalHost, dat$LarvalHost), sd)
n3 = tapply(dat$GrowthRate,
            list(dat$MaternalHost, dat$LarvalHost), length)
SE3 = sd3/sqrt(n3)

#Results presented in tables
DevelopmentTime = data.frame(
  MaternalHost = rep(c("Barbarea", "Berteroa"), each = 2),
  LarvalHost = rep(c("Barbarea", "Berteroa"), 2),
  Mean = as.vector(means1),
  SE = as.vector(SE1)
)
DevelopmentTime

AdultWeight = data.frame(
  MaternalHost = rep(c("Barbarea", "Berteroa"), each = 2),
  LarvalHost = rep(c("Barbarea", "Berteroa"), 2),
  Mean = as.vector(means2),
  SE = as.vector(SE2)
)
AdultWeight

GrowthRate = data.frame(
  MaternalHost = rep(c("Barbarea", "Berteroa"), each = 2),
  LarvalHost = rep(c("Barbarea", "Berteroa"), 2),
  Mean = as.vector(means3),
  SE = as.vector(SE3)
)
GrowthRate

#AI assisted: binding into one dataset to present the 3 plots as an only one
DevelopmentTime$Variable = "Larval development time (days)"
AdultWeight$Variable = "Adult weight (mg)"
GrowthRate$Variable = "Larval growth rate"

AllData = rbind(DevelopmentTime, AdultWeight, GrowthRate)

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
    title = "Effects of maternal and larval host plant on three biological traits",
    x = "Larval host genus",
    y = "Mean response",
    color = "Maternal host genus"
  ) +
  #AI assisted
  theme(plot.title = element_text(hjust = 0.5))

ggsave("butterflies_plot.png", width = 30, height = 12.5, units = "cm", dpi = 300)