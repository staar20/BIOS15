dat = read.csv("datasets/Eulaema.csv")
head(dat)

m = glm(Eulaema_nigrita~effort+altitude+MAT, "poisson", data=dat)

