setwd('/Users/jing/Desktop/Github')

# read excel file
library("readxl")
sc <- read_excel("SupplyChain.xlsx")
attach(sc)

# remove rows with na in r
sc <- na.omit(sc) 
dim(sc)

m1 <- lm(confirmed_case ~ Major_Airports + healthcare_worker +
         Hourly_mean_wage + population_density +Freight_Railroad, data=sc)

summary(m1)

# residual plot check for heteroskedasticity
res = resid(m1)

library("ggplot2")

head(fortify(m1))

residPlot = ggplot(aes(x=.fitted, y=.resid), data=m1) +
geom_point() + geom_hline(yintercept=0) +labs(x="Fitted vlaues", y ="Residuals")

residPlot

# Breusch-Pagan test check for heteroskedasticity
library(lmtest)
bptest(m1, data = sc)

# pvalue > 0.05, fail to reject the null hypthesis that there is
# no heteroskedasticity issue in the original model. 
# residuals were homoscedastic with 95/90% leve of statistic confidence. 

# white test check for heteroskedasticity
#library(vars)
# dataset <- data.frame(sc)
# model2 <- VAR(dataset, p = 1)
#whites.htest(m1)

#The whites.htest() function implements White's test for 
# heteroskedasticity for vector autoregressions (VAR). 
# It requires a varest object as input. However, 
# from your description it seems that your model is 
# not a VAR (vector autoregression) but a simple linear model.

# use Robust to eliminate heteroscedasticity
library(robustbase)
lmrobfit <- lmrob(confirmed_case ~ Major_Airports + healthcare_worker +
                    Hourly_mean_wage + population_density +Freight_Railroad, 
                  data = sc)

summary(lmrobfit)

bptest(lmrobfit, data = sc)

# residual plot
sc$resi <- lmrobfit$residuals
sc$fitted <- lmrobfit$fitted

library(ggplot2)
ggplot(data = sc, aes(y = resi, x = fitted)) + 
  geom_point(col = 'blue') + geom_abline(slope = 0)

# standardize the data 
library(easynls)
test = sc[,2:7]
test = scale(test)
head(test)
m2 <- lm(confirmed_case ~ Major_Airports + healthcare_worker +
           Hourly_mean_wage + population_density +Freight_Railroad)

summary(m2)
bptest(m2, data = sc)

# use log function on DV
m3 <- lm(log(confirmed_case) ~ Major_Airports + healthcare_worker +
           Hourly_mean_wage + population_density +Freight_Railroad, data=sc)

summary(m3)
bptest(m3, data = sc)

# residual plot
sc$resi <- m3$residuals
sc$fitted <- m3$fitted

library(ggplot2)
ggplot(data = sc, aes(y = resi, x = fitted)) + 
  geom_point(col = 'blue') + geom_abline(slope = 0)

# use log function on whole data
m4 <- lm(log(confirmed_case) ~ log(Major_Airports) + log(healthcare_worker) +
           log(Hourly_mean_wage) + log(population_density) +log(Freight_Railroad), data=sc)

summary(m4)
bptest(m4, data = sc)

# residual plot
sc$resi <- m4$residuals
sc$fitted <- m4$fitted

library(ggplot2)
ggplot(data = sc, aes(y = resi, x = fitted)) + 
  geom_point(col = 'blue') + geom_abline(slope = 0)












