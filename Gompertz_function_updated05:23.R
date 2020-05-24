setwd('/Users/jing/Desktop/Github')
# read excel file
library("readxl")

#gompertz model10 = "y~a*exp(-b*exp(-c*x)" 

patient <- read_excel("confirmed_case.xlsx")
attach(patient)

#plot data
# plot(date, Maricopa, type="l",col="red")
# lines(date,Burlington, col="green")
# lines(date,`Los Angeles`, col="yellow")
# lines(date, `New York City`, col="blue")

# lets take Maricopa county as an example 
#plot data
plot(day, patient$Maricopa, type="l",col="red")

#install.packages('easynls')
library(easynls)

#fit data to the gompertz model
Maricopa = patient[,2:3]

model1 = nlsfit(Maricopa, model = 10, start = c(3,20,2))

model2 = nlsfit(scale(Maricopa), model = 10, start = c(3,20,2))
model2

# starting from time =4.6, # of patients stays at 3.88
# day =226 (09/03/2020), # of patients = 11223

