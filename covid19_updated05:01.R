setwd('/Users/jing/Desktop/Github')
# read excel file
library("readxl")
patient <- read_excel("confirmed_case.xlsx")
attach(patient)
head(patient)

#plot data
plot(date, Maricopa, type="l",col="red")
lines(date,Burlington, col="green")
lines(date, `Los Angeles`, col="yellow")
lines(date, `New York City`, col="blue")

legend("topleft",
       c("Maricopa", "Burlington", "Los Angeles", "New York City"),
       fill=c("red", "green", "yellow", "blue")
)

#install.packages('easynls')
library(easynls)

# Step1: Fit data to the gompertz model Maricopa
mydata = patient[, 2:3]
head(mydata)
mydata <- as.data.frame(mydata) 
model1 = nlsfit(mydata, model = 10, start = c(a = 100, b = 1, c = .1))
model1 # represents the number of patients in the given time period t


#plot data
nlsplot(mydata, model = 10, start = c(a = 100, b = 1, c = .1), 
        xlab = "Patient" , ylab = "Date", position = 1)


# Step1: Fit data to the gompertz model
# mydata = patient[, 6:7]
# head(mydata)
# mydata <- as.data.frame(mydata) 
# model1 = nlsfit(mydata, model = 10, start = c(a = 100, b = 1, c = .1))
# model1 # represents the number of patients in the given time period t
# 
# 
# #plot data
# nlsplot(mydata, model = 10, start = c(a = 100, b = 1, c = .1), 
#         xlab = "Date" , ylab = "Patient", position = 1)
# Step2: Run regression on supply chain factors


