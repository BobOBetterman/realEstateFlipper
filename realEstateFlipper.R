library(lubridate)

setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")

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

# Remove all the properties that don't have a "sold" price
propListings <- propListings[complete.cases(propListings[,24]),]

propListings[ , 25] <- propListings[ , 24] / propListings[ , 21]
names(propListings)[25] <- "$/SqFt.House.Num"

# Use loess to fit a line to the scatter plot.

# Looks like the right ratio is 0.375. Victor likes 0.41, but I think he's
# ignoring the data.

# Cleaning data

propListings[,8] <- as.Date(propListings[,8], "%m/%d/%Y")
propListings[,13] <- as.Date(propListings[,13], "%m/%d/%Y")

propListings <- propListings[complete.cases(propListings),]

# lubridate to change date: Sys.Date() - years(1)
oneYear <- Sys.Date() - years(1)
sixMonths <- Sys.Date() - months(6)
oneMonth <- Sys.Date() - months(1)

propSoldOneYear <- propListings[propListings[,13] >= oneYear, ]
propSoldSixMonths <- propListings[propListings[,13] >= sixMonths, ]
propSoldOneMonth <- propListings[propListings[,13] >= oneMonth, ]