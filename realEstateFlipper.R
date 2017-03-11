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

propListings <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)

propListings[ , 25] <- as.numeric(gsub(",", "", as.character(propListings[ , 19])))
names(propListings)[25] <- "houseSqFt"
propListings[ , 26] <- as.numeric(gsub(",", "", as.character(propListings[ , 12])))
names(propListings)[26] <- "lotSqFt"


propListings[ , 27] <- propListings[ , 25] / propListings[ , 26]
names(propListings)[27] <- "house.To.Lot.Size.Ratio"

propListings[,28] <- (gsub(",", "", as.character(propListings[,18])))
propListings[,28] <- as.numeric(gsub("\\$", "", as.character(propListings[,28])))
names(propListings)[28] <- "sell.Price.Num"

propListings[ , 29] <- propListings[ , 28] / propListings[ , 25]
names(propListings)[29] <- "$/SqFt.House.Num"

propListings[,30] <- (gsub(",", "", as.character(propListings[,10])))
propListings[,30] <- as.numeric(gsub("\\$", "", as.character(propListings[,30])))
names(propListings)[30] <- "list.Price.Num"

# Eliminate all the listings with HOA fees, as they are probably
# mistakenly listed as detached single family homes

propListings$HOA.Fee <- as.numeric(gsub(",", "", as.character(propListings$HOA.Fee)))

propListings <- propListings[propListings$HOA.Fee == 0 | is.na(propListings$HOA.Fee),]

# Use loess to fit a line to the scatter plot.

# Looks like the right ratio is 0.375. Victor likes 0.41, but I think he's
# ignoring the data.

# Switching to a completely data driven approach to determine the right ratio.

# Cleaning data

propListings[,11] <- as.Date(propListings[,11], "%m/%d/%Y")
propListings[,17] <- as.Date(propListings[,17], "%m/%d/%Y")

# Remove all the properties that don't have a "sold" price
propListingsSold <- propListings[complete.cases(propListings[,28]),]

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
oneYear <- Sys.Date() - years(1)
sixMonths <- Sys.Date() - months(6)
oneMonth <- Sys.Date() - months(1)

# Select only houses on lots over 8000 sqft, and that are less
# than a year old. Than check the sale prices on those houses.

propListSoldBigNew <- propListSoldBig[propListSoldBig$Age <= 3, ]

propSoldOneYear <- propListSoldBigNew[propListSoldBigNew[,17] >= oneYear, ]
propSoldSixMonths <- propListSoldBigNew[propListSoldBigNew[,17] >= sixMonths, ]
propSoldOneMonth <- propListSoldBigNew[propListSoldBigNew[,17] >= oneMonth, ]


# Use the "FD" method for making histograms

#meanOneYear <- numeric()
#for(i in 1:25) {
#   meanOneYear[i] <- mean(propSoldOneYear[propSoldOneYear[,22] >= (i*1000) & propSoldOneYear[,22] < ((i+1)*1000), 25])
# }

meanOverTime <- numeric()
meanOverTime[1] <- mean(propSoldOneYear[,29])
meanOverTime[2] <- mean(propSoldSixMonths[,29])
meanOverTime[3] <- mean(propSoldOneMonth[,29])

# Find highest priced sales (95% quantile)

highest95 <- numeric()
highest95[1] <- quantile(propSoldOneYear[,29], probs = .95)
highest95[2] <- quantile(propSoldSixMonths[,29], probs = .95)
highest95[3] <- quantile(propSoldOneMonth[,29], probs = .95)


# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

propListings$Status <- factor(propListings$Status)
propListings$Postal.City <- factor(propListings$Postal.City)
propListings$Zoning <- factor(propListings$Zoning)

propListingsActive <- propListings[propListings$Status == "Active", ]

propListingsActive[,31] <- propListingsActive$lotSqFt * houseRatio
names(propListingsActive)[31] <- "houseSizeSqFt"

# Add a column for figuring out cost to build a new house

propListingsActive[,32] <- propListingsActive$houseSizeSqFt * newConsCostSqFt
names(propListingsActive)[32] <- "costToBuildHouse"

# Figure out cost to buy the place and build a new house

propListingsActive[,33] <- propListingsActive[,30] + propListingsActive[,32]
names(propListingsActive)[33] <- "totalCostToBuild"

# Predicted sale price of newly built house.
# Figure out the predicted $/sqft by using a combination of the 1 month, 6 month, and 1 year values.

# This figures out the predicted price based on the highest prices
#predPrice <- (highest95[1] + 2*highest95[2] + 3*highest95[3]) / 6

# This figures out the price based on new construction on big lot sizes
# Use only this one or the above--not both

#predPrice <- (meanOverTime[1] + 2*meanOverTime[2] + 3*meanOverTime[3]) / 6

#predPrice <- (meanOverTime[1] + 2*meanOverTime[2]) / 3

predPrice <- meanOverTime[1]

# Deduct 7% for commission, fees, etc.

predPriceFinal <- predPrice * 0.93

propListingsActive[,34] <- propListingsActive[,31] * predPriceFinal
names(propListingsActive)[34] <- "predictedSalePrice"

# Figure out potential profit by comparing the difference between the predicted price and cost to build

propListingsActive[,35] <- propListingsActive[,34] - propListingsActive[,33]
names(propListingsActive)[35] <- "potentialProfit"

# Sort the listings to see which are the most profitable

#propListingsActive <- propListingsActive[order(-propListingsActive[,34]), ]

# Sort by the profit percentage instead

propListingsActive[,36] <- propListingsActive$potentialProfit / propListingsActive$totalCostToBuild
names(propListingsActive)[36] <- "profitPercentOfInvestment"

propListingsActive <- propListingsActive[order(-propListingsActive[,36]), ]