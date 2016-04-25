#### 37301 Data Driven Marketing ######
#### Homework 3 #######

library(psych)
library(lattice)
library(plyr)

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 3/37103-HW3")
load("Slot-Data.RData")

########################################################
################## Question 1 ##########################

# Export Frequency Tables of Denomination
denomination_table <- count(slot_DF$denomination)
write.csv(denomination_table, "denomination_table.csv")

# Create tables of Month and Location
table(slot_DF$month)
table(slot_DF$location)

# Make a histogram of the locations
histogram(slot_DF$location, type = "count", main = "Histogram of Slot Locations", xlab = "Location")

########################################################
################## Question 2 ##########################

# Back out Coin In (total slot play) from winnings and hold.
slot_DF$coin_in <- 100*slot_DF$winnings/slot_DF$hold

########################################################
################## Question 3 ##########################

# Create a histogram and summary statiscics for hold percentage.
histogram(slot_DF$hold, type = "count", main = "Histogram of Slot Hold Percentage", xlab = "Hold Percentage")
describe(slot_DF$hold)

########################################################
################## Question 4 ##########################

# Create a histogram and summary statistics for days active
histogram(slot_DF$days_active, type = "count", main = "Histogram of Days Active", xlab = "Days Active")
describe(slot_DF$days_active)

# We could control for days active in two ways:
# 1. We could add it to our regression.
# 2. We could divide coin in by days active to get a new variable, coin-in per active day

slot_DF$coin_in_day <- slot_DF$coin_in/slot_DF$days_active

########################################################
################## Question 5 ##########################

# Estimate the elasticity of coin-in with respect to hold percentage

# Method 1: Control for days active in the regression
reg1 <- lm(log(coin_in) ~ log(hold) + days_active, data = slot_DF)
summary(reg1)

# Method 2: Control for days active using the variable coin-in per active day
reg2 <- lm(log(coin_in_day) ~ log(hold), data = slot_DF)
summary(reg2)

########################################################
################## Question 6 ##########################

# Control for Denomination

# Run a regression where the factor does not interact with the elasticity.
reg3 <- lm(log(coin_in_day) ~ log(hold) + factor(denomination) - 1, data = slot_DF)
summary(reg3)

# Allow slope and intercept to vary with denomination categroy
# This is like running separate regresssions on subsets of the data
# This regression is not used in the write up
reg3.1 <- lm(log(coin_in_day) ~ log(hold)*factor(denomination) - log(hold) - 1, data = slot_DF)
summary(reg.1)

# Show the previous regression is indeed just like running regression of a subset of the data
# This regression is not used in the write up
reg3.2 <- lm(log(coin_in_day) ~ log(hold), data = slot_DF[which(slot_DF$denomination == 10),])
summary(reg3.2)

# Show conditional means of hold by denomination
aggregate(hold ~ denomination, FUN = "mean", data = slot_DF)

########################################################
################## Question 7 ##########################

# Control for Location
reg4 <- lm(log(coin_in_day) ~ log(hold) + factor(denomination) + location - 1, data = slot_DF)
summary(reg4)
