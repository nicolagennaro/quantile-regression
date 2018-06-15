setwd("DSSC/stat_methods_4_data/quantile-regression/")

house <- read.csv("house_from_gitsklearn.csv")

dim(house)
colnames(house)
summary(house)



# clean from NA
print(paste("there are ", dim(house)[1]-sum(complete.cases(house)), " rows with NA"))
house <- house[ complete.cases(house), ]
dim(house)


# factors
levels(house$ocean_proximity)

library(ggplot2)

# new variables
# new covariate: total_bedrooms/population, total_rooms/population, households/population

ggplot(house) + geom_point(mapping=aes(x=population, y=total_bedrooms))
house$bedroom_per_population <- house$total_bedrooms / house$population

ggplot(house) + geom_point(mapping=aes(x=population, y=total_rooms))
house$rooms_per_population <- house$total_rooms / house$population

ggplot(house) + geom_point(mapping=aes(x=population, y=households))
house$households_per_population <- house$households / house$population

summary(house)  



# normalization
# housing_median_age
to_scale <- c("median_house_value", "median_income", "households", "population", "total_bedrooms", "total_rooms", "housing_median_age", "latitude", "longitude")

for( str in to_scale){
  house[str] = ((house[str]) - min(house[str])) /
  ( max(house[str]) - min(house[str]) )
}

summary(house)



# some plots
ggplot(house) + geom_point(mapping=aes(x=households_per_population, y=median_house_value))
ggplot(house) + geom_point(mapping=aes(x=total_bedrooms, y=median_house_value))

ggplot(house) + geom_point(mapping=aes(x=latitude, y=median_house_value))
ggplot(house) + geom_point(mapping=aes(x=longitude, y=median_house_value))


ggplot(house) + geom_point(mapping=aes(x=median_income, y=median_house_value))


ggplot(house) + geom_boxplot(mapping=aes(x=ocean_proximity, y=median_house_value, color=ocean_proximity)) +
  xlab("Ocean Proximity") + ylab("Median House Value")   +
  scale_color_manual(values = c("violet", "blue", "green", "orange", "red")) +
  theme(legend.position = "none")

ggplot(house) + geom_histogram(aes(x=ocean_proximity), stat='count')+
  xlab("Ocean Proximity")




# quantile regression

library(quantreg)


plot(house$median_income, house$median_house_value, cex=.25, # type='n',
     xlab="Median Income", ylab="Median House Value")

#points(house$median_income, house$median_house_value,cex=.5,col="blue")
abline(rq( median_house_value ~ median_income, data=house, tau=.5), col="blue")
abline(lm(median_house_value~median_income, data=house),lty=2,col="red") #the dreaded ols line

taus <- c(.05,.1,.25,.75,.90,.95)

for( i in 1:length(taus)){
  abline(rq( median_house_value ~ median_income, data=house,tau=taus[i]),col="orange")
}


fit <- lm( median_house_value ~ median_income + (median_income*median_income), data=house )
summary(fit)

plot(house$median_income, house$median_house_value, cex=.25, # type='n',
     xlab="Median Income", ylab="Median House Value")
abline(fit, col="red")


house$median_income2 = house$median_income*house$median_income

fit <- lm( median_house_value ~ median_income + median_income2, data=house )
summary(fit)
coef(fit)[1]
curve( coef(fit)[1] + coef(fit)[2]*x + coef(fit)[3]*x^2, col="blue", add=TRUE)

fit <- rq( median_house_value ~ median_income + median_income2, data=house,tau=.5)
summary.rq(fit)
curve( coef(fit)[1] + coef(fit)[2]*x + coef(fit)[3]*x^2, col="green", add=TRUE)




to_fit <- c("median_income", "total_bedrooms", "total_rooms")

z <- c()
z <- c(z, coef(fit))
z
str = "median_house_value ~ median_income + total_bedrooms + total_rooms"
fit <- rq(str, data=house, tau=0.5)
summary(fit)
coef(fit)

fit <- rq(str, data=house, tau=quantile[1])
s <- summary.rq(fit, ci="boot")
s$coefficients

?summary.rq
quantile <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

# matrix (quantiles)x(n_covariates+intercept)
m <- matrix(NA, nrow=length(quantile), ncol=(3+1))

for(i in 1:length(quantile)){
  fit <- rq(str, data=house, tau=quantile[i])
  m[i,] <- coef(fit)
}

df <- data.frame(m)
colnames(df) <- names(coef(fit))
df

df <- cbind(quantile, df)

class(df$`(Intercept)`)

df$intercept = df$`(Intercept)`
df
df$`(Intercept)` <- NULL
df

plot(df$quantile, df$`(Intercept)`)

plot(df$quantile, df$`(Intercept)`)

ggplot(data=df, aes(x=df$quantile)) + 
  geom_line(aes(y=df$intercept)) +
  geom_line(aes(y=median_income)) +
  geom_line(aes(y=total_bedrooms)) +
  geom_line(aes(y=total_rooms))

library(gridExtra)

plot1 <- ggplot(data=df) + geom_line(aes(x=df$quantile, y=df$intercept)) +
  xlab("Quantile")       
plot2 <- ggplot(data=df) + geom_line(aes(x=df$quantile, y=df$median_income)) +
  xlab("Quantile")         

grid.arrange(plot1, plot2, ncol=1)
