library(TSA)
#input of the data
data("co2")

# we may want to know about the format of the dataset, so we can pick
# the variable correclty 

?co2
#lets plot the data and see how the data in generals look like
plot(co2,main = "Monthly Co2 level")


#we can see there is a increase trend and seasonal compoment in
#this dataset, in order to predict the further value
#we will first need to pick out these compoments 
#so it is stationary 
#We can only do time series/ prediction correctly when the data is stationary 
#later on, we can add these compments back to complete the prediction 

#lets remove the trend by now
#the data is clearly not stationary
#this can be verified with the adf.test or kpss.test
adf.test(x)
#not a significant p-value so not stationary
kpss.test(x)
#significant p-value means not stationary

#take the first difference
y = diff(x,differences = 1)

#plot the difference
plot(y, type = "l", main = "After Removing the Trend")


n = length(t)
t=1:length(y)
t = (t) / n

# make matrix of the harmonics
d=12 #number of time pionts in each season
n.harm = 6 #set to [d/2]
harm = matrix(nrow=length(t), ncol=2*n.harm)
for(i in 1:n.harm){
  harm[,i*2-1] = sin(n/d * i *2*pi*t)
  harm[,i*2] = cos(n/d * i *2*pi*t)
}
colnames(harm)= 
  paste0(c("sin", "cos"), rep(1:n.harm, each = 2))

#fit on all of the sines and cosines
dat = data.frame(y, harm)
fit = lm(y~., data=dat)
summary(fit)

# setup the full model and the model with only an intercept
full = lm(y~.,data=dat)
reduced = lm(y~1, data=dat)

#stepwise regression starting with the full model
fit.back = step( full, scope = formula(reduced), direction = "both")

#get back the original t so that we can plot over this range
t = 1:length(y)

#plot the estimated seasonal components
plot(t,y, type="l", col="darkgrey", ylab="", main = " Residuals with Harmonic")
lines(t, fitted(fit.back), col="red")


#plot the residuals after seasonal component is removed
ts.plot(residuals(fit.back), main="After seasonal componenets removed", ylab = "", xlab="t")

acf(resid(fit.back))
pacf(resid(fit.back))

# Use H-K algorithm to determine best model
require(forecast)
arma.fit = auto.arima(resid(fit.back),allowmean = F, step=F)

# examine the residuals of the arima fit
wn = resid(arma.fit)

acf(wn)
pacf(wn)

# since there were some significant correlations in the plots, test
# to see if there is enough to reject independence
Box.test(wn, type="Ljung-Box",lag = min(2*d, floor(n/5)) )

# forecast the next year of noise

noise.f = forecast.Arima(arma.fit, 12,level=0.95)
plot(noise.f)


# forecast the seasonal component with the noise
# Since the seasonal component just repeats for every year
# the forecast is just the estimated seasonal components for 1,...,12
season.f = fitted(fit.back)[1:12]

plot(season.f+noise.f$mean)
lines(132:143, season.f, col="red")

#now undo the difference 
#first combine the differenced series with the forecast
y.fc = c(y,season.f+noise.f$mean)
fc.all = diffinv(y.fc, difference=1, xi =  x[1] )
fc = fc.all[132:144]

ts.plot(fc.all, col="red")
lines(x)


#plot the forecasts on top of the true values
x = as.vector(co2)
length(x)
plot(1:132, x, type="l", col="darkgrey")
lines( 132: 144,fc, col="red")
#note that we are translated over one due to taking one difference


shapiro.test(as.vector(arma.fit$residuals))


x = as.vector(co2)
x[133:144] = ""
plot(1:144, x, type = "l",ylim=c(350,400))
lines(132:144, c(x[132],fc),col = "red")
lines(133:144, fc+noise.f$lower, col = "red", lty = 2)
lines(133:144, fc+noise.f$upper, col = "red", lty = 2)





