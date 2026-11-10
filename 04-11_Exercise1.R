#Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
#Start by writing a function that computes the CV for a variable.

computeCV = function(data){
  n = sum(!is.na(data), na.rm=T)
  CV = sqrt(var(data, na.rm=T))/mean(data, na.rm=T)
  return(CV)
}

#Then, simulate a random variable and

x = rnorm(50, 10, 2)
cv_x = computeCV(x)

#write a loop that samples many times from this variable and computes the CV.

out = NULL
for(i in 1:1000){
  sample1 = sample(x, replace=TRUE)
  out[i] = computeCV(sample1)
}

#Actual bootstrap on a 95% CI

quantile(out, c(0.025, 0.975))

