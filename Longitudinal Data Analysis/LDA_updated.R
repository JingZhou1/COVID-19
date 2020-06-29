setwd('/Users/jing/Desktop/Github/LDA')

# read excel file
library("readxl")
data <- read_excel("covid19.xlsx")
attach(data)

data = scale(data[,3:11])
head(data)

library(nlme)
library(lmerTest)
library(lme4)

data <- data.frame(data)
              
m1<-lmer(confirmed_cases ~ day + Major_Airports + healthcare_worker
        +Hourly_mean_wage + population_density +Freight_Railroad
        +hospital_beds +Major_Airports*day + healthcare_worker*day
        +Hourly_mean_wage*day + population_density*day +Freight_Railroad*day
        +hospital_beds*day 
        + (1+day|County), data=data)

summary(m1)

m2 = lmer(confirmed_cases ~ day + stay_at_home + (day| County) + (day| Major_Airports) + (day| healthcare_worker) + (day| Hourly_mean_wage) + (day| population_density) + (day| Freight_Railroad) + (day| hospital_beds) , data=data)

summary(m2)

library("devtools")
library("psycho")
library("statnet.common")
results <- analyze(m2, CI = 95)

# m2 = lmer(confirmed_cases ~ day+population_density+day*population_density+(1+day|County), data=data)
# summary(m2)

# m3 = lmList(confirmed_cases ~ day | County, data=data)
# summary(m3)
# plot(intervals(m3))

