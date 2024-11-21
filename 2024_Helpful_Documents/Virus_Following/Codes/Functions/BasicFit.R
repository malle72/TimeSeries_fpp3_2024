BasicFit=function(x,y){

# Calculate forecasts using 4 simple forecasting methods  applied to myts.train.
fit1 <- meanf(x,h=y)
fit2 <- naive(x,h=y)
fit3 <- snaive(x,h=y)
fit4 <-rwf(x,drift=T,h=y)
lstemp=list(fit1,fit2,fit3,fit4)
return(lstemp)
}

# One function "BasicFit" does the four fits and saves the four fits.