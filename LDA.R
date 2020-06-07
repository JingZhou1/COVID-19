setwd('/Users/jing/Desktop/Github')

# read excel file
library("readxl")
sc <- read_excel("confirmed_case.xlsx")
attach(sc)

sc = scale(sc[,3:10])
head(sc)

library(nlme)
library(lmerTest)
library(lme4)

sc <- data.frame(sc)
              
m1<-lmer(confirmed_cases ~ day + Major_Airports + healthcare_worker
        +Hourly_mean_wage + population_density +Freight_Railroad
        +hospital_beds +Major_Airports*day + healthcare_worker*day
        +Hourly_mean_wage*day + population_density*day +Freight_Railroad*day
        +hospital_beds*day 
        + (1+day|County), data=sc)

summary(m1)

# m2 = lmer(confirmed_cases ~ day+population_density+day*population_density+(1+day|County), data=sc)
# summary(m2)

# m3 = lmList(confirmed_cases ~ day | County, data=sc)
# summary(m3)
# plot(intervals(m3))



