# from rq.pdf


library(quantreg)
library(ggplot2)

??quantreg

?rq
data(engel)
engel
head(engel)
plot(engel)

ggplot(data = engel) + geom_point(mapping = aes(x=income, y=foodexp)) +
  xlab("Income") + ylab("Food Expenditure") 


# fit the median
fit1 <- rq(foodexp ~ income, tau=.5, data=engel, method="br")
fit1
fit <- lm(foodexp ~ income, data=engel)

plot(engel$income, engel$foodexp)
abline(fit, col='red')
abline(fit1, col='blue')

coef(fit)[1]

ggplot(data = engel) + geom_point(mapping = aes(x=income, y=foodexp)) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], aes( color='Mean') ) +
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], aes(col='Median') ) +
  scale_color_manual("", breaks=c("Mean, Median"), values=c("red", "blue"))

ggplot(data = engel) + geom_point(mapping = aes(x=income, y=foodexp)) +
  geom_abline( aes(intercept = coef(fit)[1], slope = coef(fit)[2], color='Mean'), size=1 ) +
  geom_abline( aes(intercept = coef(fit1)[1], slope = coef(fit1)[2], col='Median') ) +
  scale_color_manual("Legend", values=c("red", "blue")) +
  xlab("Income") + ylab("Food Expenditure") +
  labs(title="TITOLO")



summary(fit1)
res1 <- resid(fit1)
res1
head(res1)

class(res1)

new <- cbind(engel, res1)
new

ggplot(data=cbind(engel, res1)) + geom_point(mapping = aes(x=income, y=res1))
plot(engel$income, res1)

c1 <- coef(fit1)
c1

summary(fit1)
summary(fit1, se='ker')
summary(fit1, se='nid')
summary(fit1, se='boot')





df <- data.frame(y = rnorm(200))

ggplot(df, aes(sample = y)) + 
  stat_qq() +
  geom_abline(aes(slope=1, intercept=0, colour="Test"), size=1) +
  coord_equal(xlim=range(df$y)) +
  labs(colour="") +
  scale_colour_manual(values="red")







taus <- c(0.05, 0.1, 0.5, 0.9, 0.95)
fit <- lm(foodexp ~ income, data=engel)
plt <- ggplot(data = engel) + geom_point(mapping = aes(x=income, y=foodexp)) +
        geom_abline( aes(intercept = coef(fit)[1], slope = coef(fit)[2], color='Mean'), size=1 )

plt        
for( tau in taus){
  print(tau)
  fit <- rq(foodexp ~ income, tau=tau, data=engel, method="br")
  plt + geom_abline( aes(intercept = coef(fit)[1], slope = coef(fit)[2], color='Quant') )
}

plt + scale_color_manual("Legend", values=c("red", "blue", "blue", "blue", "blue", "blue")) +
  xlab("Income") + ylab("Food Expenditure") +
  labs(title="TITOLO")

plt




# NONPARAMETRIC QUANTILE REGRESSION

library(MASS)

data("mcycle")
mcycle
ggplot(mcycle) + geom_point(aes(x=times, y=accel))

# locally polynomial
?lprq

dev.off()

hs <- c(1,2,3,4)
attach(mcycle)
plot(times,accel,xlab = "milliseconds", ylab = "acceleration")

for(i in hs){
  h = hs[i]
  print(h)
  fit <- lprq(mcycle$times, mcycle$accel, h=h, tau=.5)
  summary(fit)
  lines(fit$xx, fit$fv)
}

legend(45,-70,c("h=1","h=2","h=3","h=4"),lty=1:length(hs))



attach(engel)
plot(income,foodexp,cex=.25,type="n",xlab="Household Income", ylab="Food Expenditure")
points(income,foodexp,cex=.5,col="blue")
abline(rq(foodexp~income,tau=.5),col="blue")
abline(lm(foodexp~income),lty=2,col="red") #the dreaded ols line
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(foodexp~income,tau=taus[i]),col="gray")
}






library(splines)

plot(times,accel,xlab = "milliseconds", ylab = "acceleration",type="n")
points(times,accel,cex = .75)

?model.matrix
# see construct design matrices
?bs  # Generate the B-spline basis matrix for a polynomial spline

X <- model.matrix(accel ~ bs(times, df=15))
X
dim(X)

for(tau in 1:3/4){
  fit <- rq(accel ~ bs(times, df=15), tau=tau, data=mcycle)
  accel.fit <- X %*% fit$coef
  lines(times,accel.fit)
}
