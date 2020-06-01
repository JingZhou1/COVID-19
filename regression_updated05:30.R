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

# residual plot
res = resid(m1)

library("ggplot2")

head(fortify(m1))

residPlot = ggplot(aes(x=.fitted, y=.resid), data=m1) +
geom_point() + geom_hline(yintercept=0) +labs(x="Fitted vlaues", y ="Residuals")

residPlot

# Breusch-Pagan test
bptest(m1, data = sc)

# pvalue > 0.05, fail to reject the null hypthesis that there is
# no heteroskedasticity issue in the original model. 
# residuals were homoscedastic with 95/90% leve of statistic confidence. 

#library(vars)
# dataset <- data.frame(sc)
# model2 <- VAR(dataset, p = 1)
#whites.htest(m1)

#12 The whites.htest() function implements White's test for 
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

# residual plot
# res = resid(m1)
# 
# library("ggplot2")
# 
# head(fortify(m1))
# 
# residPlot = ggplot(aes(x=.fitted, y=.resid), data=m1) +
#   geom_point() + geom_hline(yintercept=0) +labs(x="Fitted vlaues", y ="Residuals")
# 
# residPlot

# standardize the data 
library(easynls)
test = sc[,2:7]
test = scale(test)
head(test)
m2 <- lm(confirmed_case ~ Major_Airports + healthcare_worker +
           Hourly_mean_wage + population_density +Freight_Railroad)

summary(m2)


