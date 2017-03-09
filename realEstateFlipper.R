library(lubridate)

# work computer address
setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
#setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")


# Constants for the program

# The ratio of house square footage / lot size
houseRatio <- 0.4

# The cost/sqft to build a new place
newConsCostSqFt <- 200




# Start of sold property data analysis

propListings <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)

propListings[ , 21] <- as.numeric(gsub(",", "", as.character(propListings[ , 15])))
names(propListings)[21] <- "houseSqFt"
propListings[ , 22] <- as.numeric(gsub(",", "", as.character(propListings[ , 9])))
names(propListings)[22] <- "lotSqFt"


propListings[ , 23] <- propListings[ , 21] / propListings[ , 22]
names(propListings)[23] <- "house.To.Lot.Size.Ratio"

propListings[,24] <- (gsub(",", "", as.character(propListings[,14])))
propListings[,24] <- as.numeric(gsub("\\$", "", as.character(propListings[,24])))
names(propListings)[24] <- "sell.Price.Num"

propListings[ , 25] <- propListings[ , 24] / propListings[ , 21]
names(propListings)[25] <- "$/SqFt.House.Num"

# Use loess to fit a line to the scatter plot.

# Looks like the right ratio is 0.375. Victor likes 0.41, but I think he's
# ignoring the data.

# Cleaning data

propListings[,8] <- as.Date(propListings[,8], "%m/%d/%Y")
propListings[,13] <- as.Date(propListings[,13], "%m/%d/%Y")

# Remove all the properties that don't have a "sold" price
propListingsSold <- propListings[complete.cases(propListings[,24]),]

propListingsSold <- propListingsSold[complete.cases(propListingsSold),]

# lubridate to change date: Sys.Date() - years(1)
oneYear <- Sys.Date() - years(1)
sixMonths <- Sys.Date() - months(6)
oneMonth <- Sys.Date() - months(1)

propSoldOneYear <- propListingsSold[propListingsSold[,13] >= oneYear, ]
propSoldSixMonths <- propListingsSold[propListingsSold[,13] >= sixMonths, ]
propSoldOneMonth <- propListingsSold[propListingsSold[,13] >= oneMonth, ]


# Use the "FD" method for making histograms

#meanOneYear <- numeric()
#for(i in 1:25) {
#   meanOneYear[i] <- mean(propSoldOneYear[propSoldOneYear[,22] >= (i*1000) & propSoldOneYear[,22] < ((i+1)*1000), 25])
# }

meanOverTime <- numeric()
meanOverTime[1] <- mean(propSoldOneYear[,25])
meanOverTime[2] <- mean(propSoldSixMonths[,25])
meanOverTime[3] <- mean(propSoldOneMonth[,25])

# Find highest priced sales (95% quantile)

highest95 <- numeric()
highest95[1] <- quantile(propSoldOneYear[,25], probs = .95)
highest95[2] <- quantile(propSoldSixMonths[,25], probs = .95)
highest95[3] <- quantile(propSoldOneMonth[,25], probs = .95)


# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

propListings$Status <- factor(propListings$Status)
propListings$Postal.City <- factor(propListings$Postal.City)
propListings$Zoning <- factor(propListings$Zoning)

propListingsActive <- propListings[propListings$Status == "Active", ]

propListingsActive[,26] <- propListingsActive$lotSqFt * houseRatio
names(propListingsActive)[26] <- "houseSizeSqFt"