setwd("DSSC/stat_methods_4_data/quantile-regression/")

library(ggplot2)

# from book of aurelien geron:
# data refers to block groups or districts, population 600-3000 ca

# median_income has been capped at 15.000 $
# the	data	has	been	scaled	and
# capped	at	15	(actually	15.0001)	for	higher	median	incomes,	and	at	0.5	(actually	0.4999)	for
# lower	median	incomes
# The	housing	median	age	and	the	median	house	value	were	also	capped

house <- read.csv("house_from_gitsklearn.csv")
house
dim(house)
summary(house)

sum(complete.cases(house))
print(paste("there are ", dim(house)[1]-sum(complete.cases(house)), " rows with NA"))

house <- house[ complete.cases(house), ]
dim(house)

is.factor(house$ocean_proximity)
levels(house$ocean_proximity)
ocean_prox <- unique(house$ocean_proximity)
ocean_prox
rooms <- unique(house$total_rooms)
rooms

# this command makes Rstudio go crazy
# pairs(house)

ggplot(house) + geom_point(mapping=aes(x=latitude, y=longitude, col=median_house_value)) +
  scale_color_gradientn(colours = rainbow(7))


ggplot(house) + geom_histogram(aes(x=total_rooms), bins = 100)
ggplot(house) + geom_histogram(aes(x=total_bedrooms), bins = 100)
ggplot(house) + geom_histogram(aes(x=population), bins = 100)
ggplot(house) + geom_histogram(aes(x=ocean_proximity), bins = 100, stat='count')


ggplot(house) + geom_point(mapping=aes(x=median_income, y=median_house_value))
ggplot(house) + geom_point(mapping=aes(x=population, y=households))
ggplot(house) + geom_point(mapping=aes(x=total_rooms, y=total_bedrooms))

colnames(house)

library(quantreg)


fit <- rq(median_house_value ~ ., tau=.5, data=house, method="br")
fit
fit <- rq(median_house_value ~ latitude, tau=.5, data=house)

taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

coefs <- matrix(NA, nrow = length(taus), ncol = 2)
coefs

for( i in 1:length(taus) ){
  print(i)
  fit <- fit <- rq(median_house_value ~ latitude, tau=taus[i], data=house)
  coefs[i, ] <- coef(fit)
}
coefs

ggplot(data = house) + geom_point(mapping = aes(x=latitude, y=median_house_value)) +
  geom_abline( aes(intercept = coefs[1,1], slope = coefs[1,2], color='red')) +
  geom_abline( aes(intercept = coefs[2,1], slope = coefs[2,2], color='red')) +
  geom_abline( aes(intercept = coefs[3,1], slope = coefs[3,2], color='red')) +
  geom_abline( aes(intercept = coefs[4,1], slope = coefs[4,2], color='red')) +
  geom_abline( aes(intercept = coefs[5,1], slope = coefs[5,2], color='red')) +
  geom_abline( aes(intercept = coefs[6,1], slope = coefs[6,2], color='red')) +
  geom_abline( aes(intercept = coefs[7,1], slope = coefs[7,2], color='red'))



plot(house$median_income, house$median_house_value, xlab = "Median Income", ylab="Median House Value")

fit <- rq(median_house_value ~ median_income, tau=0.5, data=house)
ggplot(data = house) + geom_point(mapping = aes(x=median_income, y=median_house_value)) +
  geom_abline( aes(intercept = coef(fit)[1], slope = coef(fit)[2], color='red'))

?boot.rq
# takes a lot of time
summary(fit, se="boot", bsmethod="xy")
               






taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
fit <- rq(median_house_value ~ latitude+longitude, tau=0.5, data=house)                     
summary(fit)               
c <- coef(fit)
c
m <- matrix(NA, nrow=7, ncol=3)

# matrix (quantiles)x(n_covariates+intercept+quantiles)
# or a data frame with quantiles as rownames

rownames(house)
m <- matrix(NA, nrow=length(taus), ncol=)
m[,1] <- taus
m
m[4, ] <- coef(fit)
m
df <- as.data.frame(m)
df
fit
rownames(df) <- taus
colnames(df) <- c("intercept", "latitude", "longitude")
# colnames(df) <- c("intercept", colnames(house))
# c <- c("intercept", colnames(house))
# c
df
