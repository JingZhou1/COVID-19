setwd('/Users/jing/Desktop/Github')

# read excel file
library("readxl")
sc <- read_excel("SupplyChain.xlsx")
attach(sc)

# remove rows with na in r
sc <- na.omit(sc) 
dim(sc)

m1 <- lm(confirmed_case ~ Major_Airports + healthcare_worker +
         Hourly_mean_wage + population_density +Freight_Railroad)

summary(m1)

res = resid(m1)

# plot(sc$Major_Airports, res, ylab="Residuals", 
#      xlab="Major_Airports", main="R") 
# 
# abline(0, 0) # the horizon

library("ggplot2")

head(fortify(m1))

residPlot = ggplot(aes(x=.fitted, y=.resid), data=m1) +
geom_point() + geom_hline(yintercept=0) +labs(x="Fitted vlaues", y ="Residuals")

residPlot
