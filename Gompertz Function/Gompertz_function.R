setwd('/Users/jing/Desktop/Github')
# read excel file
library("readxl")
patient <- read_excel("confirmed_case.xlsx")
attach(patient)
head(patient)

#plot data
plot(date, Maricopa, type="l",col="red")
lines(date,Burlington, col="green")
lines(date,`Los Angeles`, col="yellow")
lines(date, `New York City`, col="blue")

#install.packages('easynls')
library(easynls)

#fit data to the gompertz model
mydata = patient[, 5:6]
head(mydata)
model1 = nlsfit(mydata, model = 10, start = c(a = 100, b = 2, c = .5))
model1







