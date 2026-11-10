#Use non-parametric bootstrapping to derive a standard error
#for the slope of the linear regression above.

set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

#To do so, produce a data frame holding the x and y values,

df = data.frame(x, y)
head(df)

#sample from this dataset (with replacement),

out = NULL
for(i in 1:200){
  sample1 = df[sample(1:nrow(df), replace=TRUE), ]
  
  #fit the model,
  
  m = lm(y~x, data = sample1)
  
  #and save each estimate for the slope of y on x.
  
  cf = m$coef
  out[i] = cf[2]
}

#Deriving the SE (estimated by SD)

SE = sd(out)

