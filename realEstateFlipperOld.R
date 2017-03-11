library(lubridate)

# work computer address
setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
#setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")


# Constants for the program

# The ratio of house square footage / lot size
houseRatio <- numeric()

# The cost/sqft to build a new place
newConsCostSqFt <- 250




# Start of sold property data analysis

propListings <- read.csv("flipperStatsOld.csv", stringsAsFactors = FALSE)

propListings[ , 24] <- as.numeric(gsub(",", "", as.character(propListings[ , 18])))
names(propListings)[24] <- "houseSqFt"
propListings[ , 25] <- as.numeric(gsub(",", "", as.character(propListings[ , 12])))
names(propListings)[25] <- "lotSqFt"


propListings[ , 26] <- propListings[ , 24] / propListings[ , 25]
names(propListings)[26] <- "house.To.Lot.Size.Ratio"

propListings[,27] <- (gsub(",", "", as.character(propListings[,17])))
propListings[,27] <- as.numeric(gsub("\\$", "", as.character(propListings[,27])))
names(propListings)[27] <- "sell.Price.Num"

propListings[ , 28] <- propListings[ , 27] / propListings[ , 24]
names(propListings)[28] <- "$/SqFt.House.Num"

propListings[,29] <- (gsub(",", "", as.character(propListings[,10])))
propListings[,29] <- as.numeric(gsub("\\$", "", as.character(propListings[,29])))
names(propListings)[29] <- "list.Price.Num"

# Eliminate all the listings with HOA fees, as they are probably
# mistakenly listed as detached single family homes

propListings <- propListings[propListings$HOA.Fee == 0 | is.na(propListings$HOA.Fee),]

# Use loess to fit a line to the scatter plot.

# Looks like the right ratio is 0.375. Victor likes 0.41, but I think he's
# ignoring the data.

# Switching to a completely data driven approach to determine the right ratio.

# Cleaning data

propListings[,11] <- as.Date(propListings[,11], "%m/%d/%Y")
propListings[,16] <- as.Date(propListings[,16], "%m/%d/%Y")

# Remove all the properties that don't have a "sold" price
propListingsSold <- propListings[complete.cases(propListings[,27]),]

propListingsSold <- propListingsSold[propListingsSold$Sale.Date < Sys.Date() - years(3), ]

propListingsSold$HOA.Fee <- 0

propListingsSold <- propListingsSold[complete.cases(propListingsSold),]

# This is the calculation to figure out the proper ratio of house to lot size to build.

# Even before that first thing written below, we'll filter out the smaller
# lot sizes. Victor thinks this will help us get the proper size ratio.
# So, everything with a lot size of less than 8000 sqft goes.

propListSoldBig <- propListingsSold[propListingsSold$lotSqFt >= 2500, ]

# First, figure out the most expensive 5% of houses:
#mostExp5Per <- quantile(propListSoldBig[,27], probs = 0.95)
#mostExpRatios <- propListSoldBig[propListSoldBig[,27] >= mostExp5Per, 25]

# Once you have the list of size ratios associated with the most expensive $/sqft, figure out the mean value:
#mostExpHouseRatio <- mean(mostExpRatios)

# Rather than the above, just find the biggest possible ratio on everything
# with a lot size bigger than 8000 sqft

# Assign that value to the house ratio constant
#houseRatio <- mostExpHouseRatio

houseRatio <- quantile(propListSoldBig$house.To.Lot.Size.Ratio, probs = 0.9)

# lubridate to change date: Sys.Date() - years(1)
oneYear <- Sys.Date() - years(4)
#sixMonths <- Sys.Date() - years(3.5)
#oneMonth <- Sys.Date() - years(3.1)

# Select only houses on lots over 8000 sqft, and that are less
# than a year old. Than check the sale prices on those houses.

propListSoldBigNew <- propListSoldBig[propListSoldBig$Age <= 3, ]

propSoldOneYear <- propListSoldBigNew[propListSoldBigNew[,16] >= oneYear, ]
propSoldSixMonths <- propListSoldBigNew[propListSoldBigNew[,16] >= sixMonths, ]
propSoldOneMonth <- propListSoldBigNew[propListSoldBigNew[,16] >= oneMonth, ]


# Use the "FD" method for making histograms

#meanOneYear <- numeric()
#for(i in 1:25) {
#   meanOneYear[i] <- mean(propSoldOneYear[propSoldOneYear[,22] >= (i*1000) & propSoldOneYear[,22] < ((i+1)*1000), 25])
# }

meanOverTime <- numeric()
meanOverTime[1] <- mean(propSoldOneYear[,28])
meanOverTime[2] <- mean(propSoldSixMonths[,28])
meanOverTime[3] <- mean(propSoldOneMonth[,28])

# Find highest priced sales (95% quantile)

highest95 <- numeric()
highest95[1] <- quantile(propSoldOneYear[,28], probs = .95)
highest95[2] <- quantile(propSoldSixMonths[,28], probs = .95)
highest95[3] <- quantile(propSoldOneMonth[,28], probs = .95)


# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

propListings$Status <- factor(propListings$Status)
propListings$Postal.City <- factor(propListings$Postal.City)
propListings$Zoning <- factor(propListings$Zoning)

propListingsActive <- propListings[propListings$Status == "Active", ]

propListingsActive[,30] <- propListingsActive$lotSqFt * houseRatio
names(propListingsActive)[30] <- "houseSizeSqFt"

# Add a column for figuring out cost to build a new house

propListingsActive[,31] <- propListingsActive$houseSizeSqFt * newConsCostSqFt
names(propListingsActive)[31] <- "costToBuildHouse"

# Figure out cost to buy the place and build a new house

propListingsActive[,32] <- propListingsActive[,29] + propListingsActive[,31]
names(propListingsActive)[32] <- "totalCostToBuild"

# Predicted sale price of newly built house.
# Figure out the predicted $/sqft by using a combination of the 1 month, 6 month, and 1 year values.

# This figures out the predicted price based on the highest prices
#predPrice <- (highest95[1] + 2*highest95[2] + 3*highest95[3]) / 6

# This figures out the price based on new construction on big lot sizes
# Use only this one or the above--not both

predPrice <- (meanOverTime[1] + 2*meanOverTime[2] + 3*meanOverTime[3]) / 6

# Deduct 7% for commission, fees, etc.

predPriceFinal <- predPrice * 0.93

propListingsActive[,33] <- propListingsActive[,30] * predPriceFinal
names(propListingsActive)[33] <- "predictedSalePrice"

# Figure out potential profit by comparing the difference between the predicted price and cost to build

propListingsActive[,34] <- propListingsActive[,33] - propListingsActive[,32]
names(propListingsActive)[34] <- "potentialProfit"

# Sort the listings to see which are the most profitable

#propListingsActive <- propListingsActive[order(-propListingsActive[,34]), ]

# Sort by the profit percentage instead

propListingsActive[,35] <- propListingsActive$potentialProfit / propListingsActive$totalCostToBuild
names(propListingsActive)[35] <- "profitPercentOfInvestment"

propListingsActive <- propListingsActive[order(-propListingsActive[,35]), ]




# This stuff is the data on the sold stuff for the last year of the listings:

propListingsOldSales <- propListings[complete.cases(propListings[,27]),][propListings[complete.cases(propListings[,27]),]$Sale.Date >= Sys.Date() - years(3),]

propListingsOldSales$HOA.Fee <- 0

propListingsOldSales <- propListingsOldSales[complete.cases(propListingsOldSales),]

propListingsOldSales[,30] <- propListingsOldSales$lotSqFt * houseRatio
names(propListingsOldSales)[30] <- "houseSizeSqFt"

# Add a column for figuring out cost to build a new house

propListingsOldSales[,31] <- propListingsOldSales$houseSizeSqFt * newConsCostSqFt
names(propListingsOldSales)[31] <- "costToBuildHouse"

# Figure out cost to buy the place and build a new house

propListingsOldSales[,32] <- propListingsOldSales[,29] + propListingsOldSales[,31]
names(propListingsOldSales)[32] <- "totalCostToBuild"

# Predicted sale price of newly built house.
# Figure out the predicted $/sqft by using a combination of the 1 month, 6 month, and 1 year values.

# This figures out the predicted price based on the highest prices
#predPrice <- (highest95[1] + 2*highest95[2] + 3*highest95[3]) / 6

# This figures out the price based on new construction on big lot sizes
# Use only this one or the above--not both

#predPrice <- (meanOverTime[1] + 2*meanOverTime[2] + 3*meanOverTime[3]) / 6
predPrice <- meanOverTime[1]

# Deduct 7% for commission, fees, etc.

predPriceFinal <- predPrice * 0.93

propListingsOldSales[,33] <- propListingsOldSales[,30] * predPriceFinal
names(propListingsOldSales)[33] <- "predictedSalePrice"

# Figure out potential profit by comparing the difference between the predicted price and cost to build

propListingsOldSales[,34] <- propListingsOldSales[,33] - propListingsOldSales[,32]
names(propListingsOldSales)[34] <- "potentialProfit"

# Sort the listings to see which are the most profitable

#propListingsOldSales <- propListingsOldSales[order(-propListingsOldSales[,34]), ]

# Sort by the profit percentage instead

propListingsOldSales[,35] <- propListingsOldSales$potentialProfit / propListingsOldSales$totalCostToBuild
names(propListingsOldSales)[35] <- "profitPercentOfInvestment"

propListingsOldSales <- propListingsOldSales[order(-propListingsOldSales[,35]), ]