#Read data and Store in data variable
data<- read.csv(file="./BTC-USD.csv", header=TRUE, sep=",")
date<- as.POSIXct(data$Date)

price<- data$Close
data <- na.omit(data) # Remove NA if needed
data = subset(data, select = -c(2,3,4,6,7) )


#Plot of Stock price against date(From August to Now)
plot(date, price, type="l", lwd=2, cex=0, col="light blue",
     xlab="Time", ylab="Stock price", first.panel=grid())

#calculating increments of log-price
log.inc<- c()
price1<- price[-1]
price1.lag<- head(price, -1)
log.ratio<- log(price1/price1.lag)
#plotting histogram
library(rcompanion)
plotNormalHistogram(log.ratio,main="(a)", xlab="Log-price increments",
                    col="light blue")

#estimating parameters
print(mu.hat<- mean(log.ratio))
print(sigma.hat<- sd(log.ratio))

#Geometric Brownian Motion Simulation:

#specifying Brownian motion as vector
BM<- c()
#specifying initial value
BM[1]<- 0
#specifying seed
set.seed(1234321457)
#simulating Brownian motion with drift and volatility
for (i in 2:92)
  BM[i]<- mu.hat+BM[i-1] + sigma.hat*rnorm(1)
#computing values for geometric Brownian motion
GBM<-price[1]*exp(BM)
#plotting actual and simulated trajectories
plot(date, price, type="l", lty=1, lwd=3.5, col="yellow",
     xlab="Time", ylab="Stock Price",main="(b)" ,first.panel=grid())
lines(date, GBM, lwd=3, col="black")
legend("bottomleft", c("Actual Price", "Simulated Price"),
       lty=1, col=c("yellow", "black"))


#Monte Carlo Simulation for Geometric Brownian Motion SDE:
set.seed(231431)
nsim <- 500
S0 <- price[92]
mu <- mu.hat
sigma <- sigma.hat
t = 30
gbm <- matrix(ncol = nsim, nrow = t)
for (simu in 1:nsim) {
  for (day in 2:t) {
    epsilon <- rnorm(t)
    dt = 1 / t
    gbm[1, simu] <- S0
    gbm[day, simu] <- exp((mu - sigma * sigma / 2) * dt 
                          + sigma * epsilon[day] * sqrt(dt))
  }
}
gbm <- apply(gbm, 2, cumprod)
ts.plot(gbm,main = "(c)" ,ylab = "Stock Price",gpars = list(col=rainbow(10)))

