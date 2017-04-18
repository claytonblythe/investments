#Date: 1/28/17
#Author: Clayton Blythe
#email: blythec1@central.edu
#Purpose: This is a program for calculating the expected return for an investment in the S&P500 over different time horizons, taking into account reinvested dividends and controlling for inflation

#remove the corresponding current environmental variables and load the libraries and initial raw data
rm(list=ls())
#set the working directory to the appropriate
setwd('/users/claytonblythe/Desktop/Mega/Statistics_Machine_Learning/')
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(lattice)
library(animation)
#get the data in the appropriate format
import <- read.csv('sp_data.csv')
data <- select(import, 1:9)
#create new variables
data <- mutate(data, appreciation_multiplier = NA, real_account_value = NA, real_cash_multiplier = NA, years_passed = NA)

#enter in the amount of years passed into years_passed column
for (i in 1:480){data$years_passed[i] = 1* i / 12}

#lets go for 40 years into the future and make a performance table
performance <- tbl_df(data$years_passed[1:480])

#A big loop only going to 1976, as 40 year time horizon performances won't be available past this date
for (j in 1:1267){
#import the data and create relevant columns for first month's calculations, could be more efficient but it works so meh
data <- select(import, 1:9)
data <- mutate(data, appreciation_multiplier = NA, dividends_balance = NA, real_account_value = NA, real_cash_multiplier = NA, years_passed=NA)
data <- slice(data, j:(j+479))

#enter in the years_passed column values into data
for (i in 1:480){data$years_passed[i] = 1* i / 12}

#set initial values appropriately prior to loop
data$appreciation_multiplier[1] = 1
data$real_account_value[1] = data$Real.Price[1]
data$real_cash_multiplier[1] = 1

#calculate the values for the new columns and what our cash multipliers will be
for (i in 2:480){
  data$appreciation_multiplier[i] = data$Real.Price[i] / data$Real.Price[i-1]
  data$real_account_value[i] = (data$real_account_value[i-1] * data$appreciation_multiplier[i]) + (data$Real.Dividend[i-1]/data$Real.Price[i-1])*(data$real_account_value[i-1]/12) 
  data$real_cash_multiplier[i] = data$real_account_value[i] / data$real_account_value[1]
}

#lets make a temporary directory to put these cash multiplier values into the "peformance" dataset where we are aggregating all of our different starting months performances
temp <- data[,13]
performance <- mutate(performance, temp)
names(performance)[j+1] <- c(paste("Cash Multiplier beginning on ", as.character(data[1,1]), sep=""))
}

names(performance)[1] = "years_passed"
performance <- mutate(performance, averages = mean(performance[-1]))
performance$means <- rowMeans(performance[-1], na.rm=TRUE)

# #this uses the animation package to create a gif from the plots that are generated in the loop
# #this can take a while..i'm sure there are ways I could have done it more efficiently
# saveGIF((
#   for (i in seq(from=1, to=480, by=1)){
#     record2 <- unname(unlist(performance[i,2:1268]))
#     d <- density(record2, adjust=.8)
#     plot(d, main="Density of Cash Multipliers Over Various Time Horizons", col="red", border="blue", xlim = c(0,25), ylim = c(0,1), xlab = "Cash Multiplier", ylab = "Density", cex.lab=1.25)
#     polygon(d, col="red")
#     text(x=13.5, y=.9, labels=paste("Time Horizon (Years): ", format(round(i/12,2),nsmall=2), sep=""), col='blue', cex =2)
#     text(x=13.5, y=.79, labels=paste("Average Multiplier: ", format(round(performance$means[i],2),nsmall=2),  sep=""), col='blue', cex =2)
#     abline(v=format(round(performance$means[i],2),nsmall=2), col="green", lwd=3, lty=2)}
#   ), movie.name = "testing_animation_feb_13.gif", interval = 0.08, nmax = ifelse(interactive(), 30, 2), ani.width = 600, ani.height = 600, clean=TRUE)

#
#uncomment for a squatty version
#this uses the animation package to create a gif from the plots that are generated in the loop
saveGIF((
  for (i in seq(from=1, to=480, by=1)){
    record2 <- unname(unlist(performance[i,2:1268]))
    d <- density(record2, adjust=.8)
    plot(d, main="Density of Cash Multipliers Over Various Time Horizons", col="red", border="blue", xlim = c(0,25), ylim = c(0,.5), xlab = "Cash Multiplier", ylab = "Density")
    polygon(d, col="red")
    text(x=15.5, y=.45, labels=paste("Time Horizon (Years): ", format(round(i/12,2),nsmall=2), sep=""), col='blue', cex =2)
    text(x=15.5, y=.375, labels=paste("Average Multiplier: ", format(round(performance$means[i],2),nsmall=2),  sep=""), col='blue', cex =2)
    abline(v=format(round(performance$means[i],2),nsmall=2), col="green", lwd=3, lty=2)}
), movie.name = "animation_squatty_with_average.gif", interval = 0.08, nmax = ifelse(interactive(), 30, 2), ani.width = 600, ani.height = 600, clean=TRUE)

#
#
#
#
