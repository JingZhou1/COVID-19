setwd('/Users/jing/Desktop/Github')

# read excel file
library("readxl")
sc <- read_excel("SupplyChain.xlsx")
attach(sc)

m1 <- lm(confirmed_case ~ Major_Airports + healthcare_worker +
         Hourly_mean_wage + population_density +Freight_Railroad)

summary(m1)
