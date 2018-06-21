setwd("DSSC/stat_methods_4_data/quantile-regression/")

house <- read.csv("house_from_gitsklearn.csv")

house <- house[ complete.cases(house), ]

dim(house)

to_scale <- c("median_house_value", "median_income", "households", "population", "total_bedrooms", "total_rooms", "housing_median_age", "latitude", "longitude")

for( str in to_scale){
  house[str] = ((house[str]) - min(house[str])) /
    ( max(house[str]) - min(house[str]) )
}

summary(house)


library(quantreg)

taus <- c(.05, .10, .25, .50, .75, .90, .95)





##########################
### all variables fit #######
############################



set.seed(2)
res.house <- house[ sample(1:nrow(house), 1000-5), ]
dim(res.house)

v <- which(house$ocean_proximity == 'ISLAND')
v

res.house <- rbind(res.house, house[v, ])
dim(res.house)

sum(duplicated(res.house))

summary(res.house)

fit <- rq(median_house_value ~ ., tau=taus, data=res.house)


summary(fit)
summary.rqs(fit)
plot.summary.rqs(summary.rqs(fit))

par(mfrow=c(1,1))





v <- which(house$ocean_proximity == 'NEAR BAY' | house$ocean_proximity == '<1H OCEAN')
length(v)

house.2 <- house[v, ]
house.3 = house[v,]
summary(house.3)

fit <- rq(median_house_value ~ median_income + ocean_proximity, tau=taus, data=house.3)
summary.rqs(fit)
plot.summary.rqs(summary.rqs(fit))




##

v <- which(house$ocean_proximity == 'INLAND' | house$ocean_proximity == 'ISLAND')
house.3 = house[v,]


fit <- rq(median_house_value ~ median_income + ocean_proximity, tau=taus, data=house.3)
sum(duplicated(house.3))
plot.summary.rqs(summary.rqs(fit))









###############
##   ANOVA   ##
###############

####################
# anova for slopes
####################


plot(house$median_income, house$median_house_value, cex=.25, cex.lab=1,cex.axis=1,
     lheight=0.5,
     xlab="Median Income", ylab="Median House Value")
fit <- rq( median_house_value ~ median_income, tau=taus, data=house )

for(i in 1:length(taus)){
  if( i == 4 ){
    abline(a=fit$coefficients[1,i], b=fit$coefficients[2,i], col="4", lwd=2)
  }
  else{
    abline(a=fit$coefficients[1,i], b=fit$coefficients[2,i], col="green3", lwd=2)
  }
  
}
anova.rqs(fit)



# here is less significant
tauss <- c(.75, .90, .95)
fit <- rq( median_house_value ~ median_income, tau=tauss, data=house )
anova.rqs(fit)




# removing ones
v <- which(house$median_house_value == 1)
house.no1 <- house[ -v, ]

plot(house.no1$median_income, house.no1$median_house_value, cex=.25, cex.lab=1,cex.axis=1,
     lheight=0.5,
     xlab="Median Income", ylab="Median House Value")
fit <- rq( median_house_value ~ median_income, tau=taus, data=house.no1 )

for(i in 1:length(taus)){
  if( i == 4 ){
    abline(a=fit$coefficients[1,i], b=fit$coefficients[2,i], col="4", lwd=2)
  }
  else{
    abline(a=fit$coefficients[1,i], b=fit$coefficients[2,i], col="green3", lwd=2)
  }
  
}
anova.rqs(fit)



###########################
# anova for nested models
###########################

# what makes the difference ??? not the Rsquared like in lm


# median income, longitude
plot(house$median_income, house$median_house_value)
fit1 <- rq( median_house_value ~ median_income, tau=.5, data=house )
abline(fit1, col='red')

fit2 <- rq( median_house_value ~ median_income + longitude, tau=.5, data=house )
summary(fit2)

anova.rq(fit1, fit2)


# latitude, longitude

plot(house$latitude, house$median_house_value)
plot(house$longitude, house$median_house_value)

fit1 <- rq( median_house_value ~ longitude, tau=.5, data=house )
fit2 <- rq( median_house_value ~ latitude + longitude, tau=.5, data=house )

anova.rq(fit1, fit2)


# total_rooms, total_bedrooms
plot(house$total_rooms, house$median_house_value)
plot(house$total_bedrooms, house$median_house_value)
plot(house$total_bedrooms, house$total_rooms)

fit1 <- rq( median_house_value ~ total_rooms, tau=.5, data=house )
fit2 <- rq( median_house_value ~ total_rooms + total_bedrooms, tau=.5, data=house )

anova.rq(fit1, fit2)



# always same fucking p-value !!!!!!!!!!

# at least here the F value decreses
q <- 0.1
fit1 <- rq( median_house_value ~ median_income, tau=q, data=house )
fit2 <- rq( median_house_value ~ median_income + total_bedrooms, tau=q, data=house )
fit3 <- rq( median_house_value ~ median_income + total_bedrooms + total_rooms, tau=q, data=house )
fit4 <- rq( median_house_value ~ median_income + total_bedrooms + total_rooms + latitude, tau=q, data=house )
fit5 <- rq( median_house_value ~ median_income + total_bedrooms + total_rooms + latitude + longitude, tau=q, data=house )
fit6 <- rq( median_house_value ~ median_income + total_bedrooms + total_rooms + latitude + longitude + population, tau=q, data=house )

anova.rq(fit1, fit2, fit3, fit4, fit5, fit6)



# try with factors
q <- 0.5
fit1 <- rq( median_house_value ~ median_income, tau=q, data=house )
fit2 <- rq( median_house_value ~ median_income + ocean_proximity, tau=q, data=house )

anova.rq(fit1, fit2)



###############
##   LPRQ    ##
###############

plot(house$longitude, house$median_house_value, xlab="Longitude", ylab="Median House Value", cex=.25, cex.lab=1,cex.axis=1, lheight=0.5)

for(tau in taus){
  fit <- lprq(house$longitude, house$median_house_value, h=0.1, tau=tau, m=50)
  if( tau == 0.5){
    lines(fit$xx, fit$fv, col="4", lwd=2)
  }
  else{
    lines(fit$xx, fit$fv, col="green3", lwd=2)
  }
}



###############
##   NLRQ    ##
###############

par(mfrow=c(1,1))
n <- (2^10)+1
x <- seq(0, 4*pi, length=n)
y <- cos(x) + rnorm(n)

df <- data.frame(x, y)
plot(x, y)

fit <- nlrq(y ~ a*cos(b*x), tau=0.5, start = list(a = 2, b = 1))
fit


x <- exp(rnorm(50))
y <- exp(1 + .5*x) + rnorm(50)
plot(x, y)
nlrq(y ~ exp(a  + b * x), start = list(a = 2, b = 1)) 


###################
#####  BOOT  ######
###################


# seeds <- c(3, 4, 7)

set.seed(4)
ns <- c(10, 12, 14)
  
par(mfrow = c(1, length(ns)))
for( i in ns ){
  n <- (2^i)+1
  x <- seq(0, 2, length=n)
  y <- -x + rnorm(n)
  df <- data.frame(x, y)
  b <- boot.rq(x=x, y=y, tau=.5, R = 1000)
  main <- paste(n, "Points")
  m <- mean(b$B) + 1
  s <- sd(b$B)
  hist(b$B + 1, breaks = 30, prob=TRUE, xlab = "B* - B", main = main)
  abline(v=0, add=TRUE, col='red', lty=2, lwd=2)
  abline(v=m, add=TRUE, col='green', lty=2, lwd=2)
  curve(dnorm(x, m, s), add=TRUE)
}


