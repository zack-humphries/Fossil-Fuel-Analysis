# R Data Project GT 4801
# Zachary Humphries
# zhumphries6@gatech.edu

install.packages("magrittr")
install.packages("tidyr")
install.packages("readr")
install.packages("ggplot2")

library(readr)
library(magrittr)
library(tidyr)
library(ggplot2)

setwd(choose.dir())

R_Dataset <- read_csv("R Dataset.csv")

R_Dataset$Date <- as.Date( R_Dataset$Date, '%m/%d/%Y')

#December, January, and Febuary (winter) are blue. March, April, and May (spring) are green. 
#June, July, and August (summer) are yellow. September, October, and November (fall) are red
plot(R_Dataset$Date, log(R_Dataset$`Coal Consumption (Quadrillion Btu)`), pch = 16, col = c("blue", "blue", "green", "green", "green",
                                                                                            "yellow", "yellow", "yellow", 
                                                                                            "red", "red", "red", "blue"))
abline(h = 0)


plot(R_Dataset$Date, log(R_Dataset$`Natural Gas Consumption`), pch = 16, col = c("blue", "blue", "green", "green", "green",
                                                                                "yellow", "yellow", "yellow", 
                                                                                "red", "red", "red", "blue"))

plot(R_Dataset$Date, log(R_Dataset$`Crude Oil Consumption`), pch = 16, col = c("blue", "blue", "green", "green", "green",
                                                                               "yellow", "yellow", "yellow", 
                                                                               "red", "red", "red", "blue"))


regression0 <- lm(log(R_Dataset$`Real Natural Gas`) ~ 
                    log(R_Dataset$`Total Fossil Fuels Production`))
summary(regression0)

plot(log(R_Dataset$`Total Fossil Fuels Production`), log(R_Dataset$`Real Natural Gas`), pch = 16)
abline(regression0, lwd = 2, col = "red")



regression1 <- lm(log(R_Dataset$`Total Fossil Fuels Production`) ~
                    log(R_Dataset$`Real Natural Gas`) +
                    log(R_Dataset$`Real Crude Oil ($/barrel)`)+
                    log(R_Dataset$`Real Coal`))

summary(regression1)
