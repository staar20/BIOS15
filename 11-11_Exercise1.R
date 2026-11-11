dat = read.csv("datasets/butterflies.csv")
names(dat)

m = lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)
anova(m)
summary(m)

ggplot(
  data = dat,
  mapping = aes(x = LarvalHost, y = DevelopmentTime, colour = MaternalHost)
) +
  geom_point()

