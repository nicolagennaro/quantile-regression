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
