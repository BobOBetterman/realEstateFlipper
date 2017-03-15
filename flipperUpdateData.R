# Run this program once a week, or only when you have new data to add to the database.
# The current database is called "flipperAll.csv", and the file of changes since the database was
# last updated is called "flipperStats.csv". This program combines the two files into the output file
# "flipperAll.csv" and outputs a second file with the likely prospects in the new data
# called "newListingsReport.csv".

library(lubridate)
library(mailR)

# work computer address
#setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")



# Constants for the program

# The ratio of house square footage / lot size
#houseRatio <- numeric()

# The cost/sqft to build a new place
newConsCostSqFt <- 250

# Lowest possible build price
lowestBuildCost <- 250

# Necessary number of samples to be statistically significant
minSampleSize <- 9

# This is the lowest percentage of profit we're willing to risk investing for
lowestProfit <- 0.1




# This section opens the past file and combines it with the new information

# Open the archive data file
propListings <- read.csv("flipperAll.csv")

# Open the new file with the last week's data
propListingsUpdate <- read.csv("flipperStats.csv")

# Figure out the MLS numbers in the new data that already exist in the data, and note the numbers that don't appear
updatedListings <- na.omit(match(propListingsUpdate$MLS.Number, propListings$MLS.Number))
naRows <- attr(updatedListings, "na.action")

# Remove the rows that don't exist in both from the new data
propChanges <- propListingsUpdate[-naRows,]

# Overwrite all the old values with the new values
propListings[updatedListings, ] <- propChanges

# Make a data frame with only the new MLS numbers, and then attach it to the full data file
propNew <- propListingsUpdate[naRows, ]
propCombined <- rbind(propListings, propNew)

propListings <- propCombined

write.csv(propCombined, "flipperAll.csv", row.names = FALSE)






# Start of sold property data analysis

propListings[ , 26] <- as.numeric(gsub(",", "", as.character(propListings[ , 20])))
names(propListings)[26] <- "houseSqFt"
propListings[ , 27] <- as.numeric(gsub(",", "", as.character(propListings[ , 13])))
names(propListings)[27] <- "lotSqFt"


propListings[ , 28] <- propListings[ , 26] / propListings[ , 27]
names(propListings)[28] <- "house.To.Lot.Size.Ratio"

propListings[,29] <- (gsub(",", "", as.character(propListings[,19])))
propListings[,29] <- as.numeric(gsub("\\$", "", as.character(propListings[,29])))
names(propListings)[29] <- "sell.Price.Num"

propListings[ , 30] <- propListings[ , 29] / propListings[ , 26]
names(propListings)[30] <- "$/SqFt.House.Num"

propListings[,31] <- (gsub(",", "", as.character(propListings[,11])))
propListings[,31] <- as.numeric(gsub("\\$", "", as.character(propListings[,31])))
names(propListings)[31] <- "list.Price.Num"

# Eliminate all the listings with HOA fees, as they are probably
# mistakenly listed as detached single family homes

propListings$HOA.Fee <- as.numeric(gsub(",", "", as.character(propListings$HOA.Fee)))

propListings <- propListings[propListings$HOA.Fee == 0 | is.na(propListings$HOA.Fee),]

# Cleaning data

propListings[,12] <- as.Date(propListings[,12], "%m/%d/%Y")
propListings[,18] <- as.Date(propListings[,18], "%m/%d/%Y")


# Repeat all of the above on the updated data

propListingsUpdate[ , 26] <- as.numeric(gsub(",", "", as.character(propListingsUpdate[ , 20])))
names(propListingsUpdate)[26] <- "houseSqFt"
propListingsUpdate[ , 27] <- as.numeric(gsub(",", "", as.character(propListingsUpdate[ , 13])))
names(propListingsUpdate)[27] <- "lotSqFt"


propListingsUpdate[ , 28] <- propListingsUpdate[ , 26] / propListingsUpdate[ , 27]
names(propListingsUpdate)[28] <- "house.To.Lot.Size.Ratio"

propListingsUpdate[,29] <- (gsub(",", "", as.character(propListingsUpdate[,19])))
propListingsUpdate[,29] <- as.numeric(gsub("\\$", "", as.character(propListingsUpdate[,29])))
names(propListingsUpdate)[29] <- "sell.Price.Num"

propListingsUpdate[ , 30] <- propListingsUpdate[ , 29] / propListingsUpdate[ , 26]
names(propListingsUpdate)[30] <- "$/SqFt.House.Num"

propListingsUpdate[,31] <- (gsub(",", "", as.character(propListingsUpdate[,11])))
propListingsUpdate[,31] <- as.numeric(gsub("\\$", "", as.character(propListingsUpdate[,31])))
names(propListingsUpdate)[31] <- "list.Price.Num"

# Eliminate all the listings with HOA fees, as they are probably
# mistakenly listed as detached single family homes

propListingsUpdate$HOA.Fee <- as.numeric(gsub(",", "", as.character(propListingsUpdate$HOA.Fee)))

propListingsUpdate <- propListingsUpdate[propListingsUpdate$HOA.Fee == 0 | is.na(propListingsUpdate$HOA.Fee),]

# Cleaning data

propListingsUpdate[,12] <- as.Date(propListingsUpdate[,12], "%m/%d/%Y")
propListingsUpdate[,18] <- as.Date(propListingsUpdate[,18], "%m/%d/%Y")




# Remove all the properties that don't have a "sold" price
propListingsSold <- propListings[complete.cases(propListings[,29]),]

propListingsSold$HOA.Fee <- 0

propListingsSold <- propListingsSold[complete.cases(propListingsSold),]

propListingsSold <- propListingsSold[propListingsSold$lotSqFt>0,]
propListingsSold <- propListingsSold[propListingsSold$houseSqFt>0,]

# Figure out the stats for each area (Area > Zip Code > City > County)
# I figure... Go back one month at a time, up to a year, to get a statistical large enough sampling.
# Never mind--just do one year for each category, make sure there are at least 10 points, then do the check.
# If there aren't enough in a category, go up to the next largest group and repeat.

houseRatioByArea <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)
houseRatioByArea <- merge(houseRatioByArea, table(factor(propListingsSold$Area..)), by.x = "Group.1", by.y = "Var1")
houseRatioByZip <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Zip.Code), FUN = quantile, probs = 0.9)
houseRatioByZip <- merge(houseRatioByZip, table(factor(propListingsSold$Zip.Code)), by.x = "Group.1", by.y = "Var1")
houseRatioByCity <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Postal.City), FUN = quantile, probs = 0.9)
houseRatioByCity <- merge(houseRatioByCity, table(factor(propListingsSold$Postal.City)), by.x = "Group.1", by.y = "Var1")
houseRatioByCounty <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$County), FUN = quantile, probs = 0.9)
houseRatioByCounty <- merge(houseRatioByCounty, table(factor(propListingsSold$County)), by.x = "Group.1", by.y = "Var1")
#houseRatioByCounty <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)


# Figure out the biggest possible house in the area--this will put a cap on the biggest build size.

houseSizeCap <- aggregate(propListingsSold$houseSqFt, list(propListingsSold$Area..), FUN = quantile, probs = 0.85)

# Also, figure out the highest sale price in the area to cap the sale price, as well -- the 0.93 is for fees

housePriceCap <- aggregate(propListingsSold$sell.Price.Num, list(propListingsSold$Area..), FUN = max)

housePriceCap[,2] <- housePriceCap[,2] * 0.93

# lubridate to change date: Sys.Date() - years(1)
oneYear <- Sys.Date() - years(1)

# Select only houses on lots over 2500 sqft, and that are less
# than three years old. Than check the sale prices on those houses.

propListSoldNew <- propListingsSold[propListingsSold$Age <= 9, ]

propSoldOneYear <- propListSoldNew[propListSoldNew[,18] >= oneYear, ]

medianLastYearArea <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Area..), FUN = median)
medianLastYearArea <- merge(medianLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
medianLastYearZip <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Zip.Code), FUN = median)
medianLastYearZip <- merge(medianLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
medianLastYearCity <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Postal.City), FUN = median)
medianLastYearCity <- merge(medianLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")
medianLastYearCounty <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$County), FUN = median)
medianLastYearCounty <- merge(medianLastYearCounty, table(factor(propSoldOneYear$County)), by.x = "Group.1", by.y = "Var1")

# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

propListings$Status <- factor(propListings$Status)
propListings$Postal.City <- factor(propListings$Postal.City)
propListings$Zoning <- factor(propListings$Zoning)


# Repeat the above for the updated prop listings

propListingsUpdate$Status <- factor(propListingsUpdate$Status)
propListingsUpdate$Postal.City <- factor(propListingsUpdate$Postal.City)
propListingsUpdate$Zoning <- factor(propListingsUpdate$Zoning)



propListingsActive <- propListingsUpdate[propListingsUpdate$Status == "Active", ]

# This determines the proper house size ratio to use (making sure the numbers of samples are high enough)

propListingsActive[,32] <- ifelse(houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),3]>minSampleSize & 
                                    !is.na(houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),3]>minSampleSize),
                                  houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),2],
                                  ifelse(houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),3]>minSampleSize & 
                                           !is.na(houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),3]>minSampleSize),
                                         houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),2],
                                         ifelse(houseRatioByCity[match(propListingsActive$Postal.City, houseRatioByCity$Group.1),3]>minSampleSize & 
                                                  !is.na(houseRatioByCity[match(propListingsActive$Postal.City, houseRatioByCity$Group.1),3]>minSampleSize),
                                                houseRatioByCity[match(propListingsActive$Postal.City, houseRatioByCity$Group.1),2], 
                                                ifelse(houseRatioByCounty[match(propListingsActive$County, houseRatioByCounty$Group.1),3]>minSampleSize & 
                                                         !is.na(houseRatioByCounty[match(propListingsActive$County, houseRatioByCounty$Group.1),3]>minSampleSize),
                                                       houseRatioByCounty[match(propListingsActive$County, houseRatioByCounty$Group.1),2], 1000000))))

names(propListingsActive)[32] <- "houseRatioNewBuild"

# This determines the proper $/sf to use (making sure the numbers of samples are high enough)

propListingsActive[,33] <- ifelse(medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),3]>minSampleSize & 
                                    !is.na(medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),3]>minSampleSize),
                                  medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),2],
                                  ifelse(medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),3]>minSampleSize & 
                                           !is.na(medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),3]>minSampleSize),
                                         medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),2],
                                         ifelse(medianLastYearCity[match(propListingsActive$Postal.City, medianLastYearCity$Group.1),3]>minSampleSize & 
                                                  !is.na(medianLastYearCity[match(propListingsActive$Postal.City, medianLastYearCity$Group.1),3]>minSampleSize),
                                                medianLastYearCity[match(propListingsActive$Postal.City, medianLastYearCity$Group.1),2], 
                                                ifelse(medianLastYearCounty[match(propListingsActive$County, medianLastYearCounty$Group.1),3]>minSampleSize & 
                                                         !is.na(medianLastYearCounty[match(propListingsActive$County, medianLastYearCounty$Group.1),3]>minSampleSize),
                                                       medianLastYearCounty[match(propListingsActive$County, medianLastYearCounty$Group.1),2], 1000000000))))

names(propListingsActive)[33] <- "houseDollarPerSFNewBuild"

# This is the size of house that can be built

# propListingsActive[,33] <- pmin(propListingsActive$lotSqFt * houseRatio, houseSizeCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
propListingsActive[,34] <- pmin(propListingsActive$lotSqFt * propListingsActive$houseRatioNewBuild, houseSizeCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
names(propListingsActive)[34] <- "houseSizeSqFt"

# Add a column for figuring out cost to build a new house

propListingsActive[,35] <- propListingsActive$houseSizeSqFt * newConsCostSqFt
names(propListingsActive)[35] <- "costToBuildHouse"

# Figure out cost to buy the place and build a new house

propListingsActive[,36] <- propListingsActive[,31] + propListingsActive[,35]
names(propListingsActive)[36] <- "totalCostToBuild"

# Deduct 7% for commission, fees, etc.

#predPriceFinal <- predPrice * 0.93

predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 0.93

propListingsActive[,37] <- pmin(propListingsActive[,34] * predPriceFinal, housePriceCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
names(propListingsActive)[37] <- "predictedSalePrice"

# Figure out potential profit by comparing the difference between the predicted price and cost to build

propListingsActive[,38] <- propListingsActive[,37] - propListingsActive[,36]
names(propListingsActive)[38] <- "potentialProfit"

# Sort the listings to see which are the most profitable

#propListingsActive <- propListingsActive[order(-propListingsActive[,34]), ]

# Sort by the profit percentage instead

propListingsActive[,39] <- propListingsActive$potentialProfit / propListingsActive$totalCostToBuild
names(propListingsActive)[39] <- "profitPercentOfInvestment"

propListingsActive <- propListingsActive[order(-propListingsActive[,39]), ]

propProspectsReport <- subset(propListingsActive, profitPercentOfInvestment >= lowestProfit, 
                              select = c("profitPercentOfInvestment", "MLS.Number", 
                                         "houseRatioNewBuild", "houseSizeSqFt", "list.Price.Num",
                                         "houseDollarPerSFNewBuild", "totalCostToBuild",
                                         "predictedSalePrice", "potentialProfit", "lotSqFt",
                                         "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                                         "Postal.City", "County"))

propProspectsReportShort <- subset(propListingsActive, breakEvenBuildCost >= lowestBuildCost,
                                   select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                              "houseSizeSqFt", "breakEvenBuildCost", "profitPercentOfInvestment"))
names(propProspectsReportShort)[5] <- "projectedHouseSizeSqFt"

propProspectsReportShort <- propProspectsReportShort[order(-propProspectsReportShort$breakEvenBuildCost), ]


# Use this print command to get rid of the annoying scientific notation in the printed table
#format(propProspectsReport, scientific=FALSE)

write.csv(format(propProspectsReport, scientific=FALSE), "newListingsReport.csv", row.names = FALSE)
write.csv(format(propProspectsReportShort, scientific=FALSE), "newListingsReportShort.csv", row.names = FALSE)





# This code at the end is to send an email to interested parties with the new
# listings in it.

sender <- "andy@tfo.vc"
recipients <- c("andy@tiberiusfo.com", "vchiang@gmail.com")

subject <- "This Week's Results"
body <- "Attached are the results for the week"

hostName <- "smtp.gmail.com"
port <- 465
userName <- "andy@tiberiusfo.com"
passWord <- "uwmsjyfyxeinavll"

if (nrow(propProspectsReport) > 0) {
  send.mail(from = sender, to = "andy@tiberiusfo.com", subject = subject, body = body,
            smtp = list(host.name = hostName, port = port,
                        user.name = userName, passwd = passWord, ssl = TRUE),
            authenticate = TRUE, send = TRUE,
            attach.files = "newListingsReport.csv")
}

if (nrow(propProspectsReport) > 0) {
  send.mail(from = sender, to = recipients, subject = subject, body = body,
            smtp = list(host.name = hostName, port = port,
                        user.name = userName, passwd = passWord, ssl = TRUE),
            authenticate = TRUE, send = TRUE,
            attach.files = "newListingsReportShort.csv")
}