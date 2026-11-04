#Use simulated data to show the close relationship between the SD of log-transformed data and the CV on arithmetic scale.

computeCV = function(data){
  n = sum(!is.na(data), na.rm=T)
  CV = sqrt(var(data, na.rm=T))/mean(data, na.rm=T)
  return(CV)
}

computeSDlog = function(data){
  n = sum(!is.na(data), na.rm=T)
  SDlog = sqrt(var(log(data), na.rm=T))
  return(SDlog)
}

#You may need e.g. the rnorm function and a for-loop to achieve this.

x = rnorm(50, 10, 2)
cv_x = computeCV(x)
sdlog_x = computeSDlog(x)

#start with comparing the two values for a single case,

c(CV = cv_x, SDlog = sdlog_x)

#then build a matrix to hold the paired values,

#two single values comparison:
#pairedvals = matrix(c(cv_x, sdlog_x), ncol = 2)

pairedvals = matrix(NA, nrow = 1000, ncol = 2)
colnames(pairedvals) = c("CV", "SDlog")

#and finally use a for-loop to populate the matrix.

for(i in 1:1000){
  sample1 = sample(x, replace=TRUE)
  pairedvals[i, 1] = computeCV(sample1)
  pairedvals[i, 2] = computeSDlog(sample1)
}

#Graph generation
plot(pairedvals[, 1], pairedvals[, 2],
     type = "p",
     main = "Similarity of the SD of log-transformed data to the CV",
     xlab = "CV(x)",
     ylab = "SD(log[x])")
