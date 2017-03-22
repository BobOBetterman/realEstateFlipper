# This program can be run anytime. It doesn't change any files. It just loads in the database
# stored in "flipperAll.csv" and crunches the numbers on all "Active" listings. It then outputs a report
# titled "currentProspectsReport.csv" which contains all the prospects currently listed in the
# full database.


library(lubridate)

# work computer address
#setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")


# Constants for the program

# The ratio of house square footage / lot size
#houseRatio <- numeric()

# The cost/sqft to build a new place
newConsCostSqFt <- 300

# Lowest possible build price
lowestBuildCost <- 300

# Necessary number of samples to be statistically significant
minSampleSize <- 9

# This is the lowest percentage of profit we're willing to risk investing for
lowestProfit <- 0.1



# Start of sold property data analysis

# This can be used for a self-contained file (i.e., five years of data on one city)
#propListings <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)

# This is the file that contains all the different download files from the MLS site--obtained 
# by running the combineFlipperStats.R program
propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)

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

# Use loess to fit a line to the scatter plot.

# Looks like the right ratio is 0.375. Victor likes 0.41, but I think he's
# ignoring the data.

# Switching to a completely data driven approach to determine the right ratio.

# Cleaning data

propListings[,12] <- as.Date(propListings[,12], "%m/%d/%Y")
propListings[,18] <- as.Date(propListings[,18], "%m/%d/%Y")

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

houseSizeCap <- aggregate(propListingsSold$houseSqFt, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)

# Also, figure out the highest sale price in the area to cap the sale price, as well -- the 0.93 is for fees

housePriceCap <- aggregate(propListingsSold$sell.Price.Num, list(propListingsSold$Area..), FUN = max)

#housePriceCap[,2] <- housePriceCap[,2] * 0.93
housePriceCap[,2] <- housePriceCap[,2] * 1

# This is the calculation to figure out the proper ratio of house to lot size to build.

# Even before that first thing written below, we'll filter out the smaller
# lot sizes. Victor thinks this will help us get the proper size ratio.
# So, everything with a lot size of less than 2500 sqft goes.

#propListSoldBig <- propListingsSold[propListingsSold$lotSqFt >= 2500, ]

# First, figure out the most expensive 5% of houses:
#mostExp5Per <- quantile(propListSoldBig[,27], probs = 0.95)
#mostExpRatios <- propListSoldBig[propListSoldBig[,27] >= mostExp5Per, 25]

# Once you have the list of size ratios associated with the most expensive $/sqft, figure out the mean value:
#mostExpHouseRatio <- mean(mostExpRatios)

# Rather than the above, just find the biggest possible ratio on everything
# with a lot size bigger than 8000 sqft

# Assign that value to the house ratio constant
#houseRatio <- mostExpHouseRatio

#houseRatio <- quantile(propListSoldBig$house.To.Lot.Size.Ratio, probs = 0.9)

# lubridate to change date: Sys.Date() - years(1)
oneYear <- Sys.Date() - years(1)
#sixMonths <- Sys.Date() - months(6)
#oneMonth <- Sys.Date() - months(1)

# Select only houses on lots over 2500 sqft, and that are less
# than three years old. Than check the sale prices on those houses.

propListSoldNew <- propListingsSold[propListingsSold$Age <= 9, ]

propSoldOneYear <- propListSoldNew[propListSoldNew[,18] >= oneYear, ]
#propSoldSixMonths <- propListSoldBigNew[propListSoldBigNew[,17] >= sixMonths, ]
#propSoldOneMonth <- propListSoldBigNew[propListSoldBigNew[,17] >= oneMonth, ]


# Use the "FD" method for making histograms

#meanOneYear <- numeric()
#for(i in 1:25) {
#   meanOneYear[i] <- mean(propSoldOneYear[propSoldOneYear[,22] >= (i*1000) & propSoldOneYear[,22] < ((i+1)*1000), 25])
# }

#meanOverTime <- numeric()
# meanLastYearArea <- aggregate(propSoldOneYear[,29], list(propSoldOneYear$Area..), FUN = mean)
# meanLastYearArea <- merge(meanLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
# meanLastYearZip <- aggregate(propSoldOneYear[,29], list(propSoldOneYear$Zip.Code), FUN = mean)
# meanLastYearZip <- merge(meanLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
# meanLastYearCity <- aggregate(propSoldOneYear[,29], list(propSoldOneYear$Postal.City), FUN = mean)
# meanLastYearCity <- merge(meanLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")

medianLastYearArea <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Area..), FUN = median)
medianLastYearArea <- merge(medianLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
medianLastYearZip <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Zip.Code), FUN = median)
medianLastYearZip <- merge(medianLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
medianLastYearCity <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Postal.City), FUN = median)
medianLastYearCity <- merge(medianLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")
medianLastYearCounty <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$County), FUN = median)
medianLastYearCounty <- merge(medianLastYearCounty, table(factor(propSoldOneYear$County)), by.x = "Group.1", by.y = "Var1")
#meanOverTime[2] <- mean(propSoldSixMonths[,29])
#meanOverTime[3] <- mean(propSoldOneMonth[,29])

# Find highest priced sales (95% quantile)

#highest95 <- numeric()
#highest95[1] <- quantile(propSoldOneYear[,29], probs = .95)
#highest95[2] <- quantile(propSoldSixMonths[,29], probs = .95)
#highest95[3] <- quantile(propSoldOneMonth[,29], probs = .95)


# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

propListings$Status <- factor(propListings$Status)
propListings$Postal.City <- factor(propListings$Postal.City)
propListings$Zoning <- factor(propListings$Zoning)

propListingsActive <- propListings[propListings$Status == "Active", ]

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
# propListingsActive[,32] <- if(meanLastYearArea[match(propListingsActive$Area.., meanLastYearArea$Group.1),3]>minSampleSize) {
#                                   meanLastYearArea[match(propListingsActive$Area.., meanLastYearArea$Group.1),2]} else {
#                                   if(meanLastYearZip[match(propListingsActive$Zip.Code, meanLastYearZip$Group.1),3]>minSampleSize) {
#                                          meanLastYearZip[match(propListingsActive$Zip.Code, meanLastYearZip$Group.1),2]} else {
#                                          if(meanLastYearCity[match(propListingsActive$Postal.City, meanLastYearCity$Group.1),3]>minSampleSize) {
#                                                 meanLastYearCity[match(propListingsActive$Postal.City, meanLastYearCity$Group.1),2]} else 1000}}
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

# Predicted sale price of newly built house.
# Figure out the predicted $/sqft by using a combination of the 1 month, 6 month, and 1 year values.

# This figures out the predicted price based on the highest prices
#predPrice <- (highest95[1] + 2*highest95[2] + 3*highest95[3]) / 6

# This figures out the price based on new construction on big lot sizes
# Use only this one or the above--not both

#predPrice <- (meanOverTime[1] + 2*meanOverTime[2] + 3*meanOverTime[3]) / 6

#predPrice <- (meanOverTime[1] + 2*meanOverTime[2]) / 3

#predPrice <- meanLastYearArea[1,2]

# Deduct 7% for commission, fees, etc.

#predPriceFinal <- predPrice * 0.93

#predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 0.93
predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 1

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

propListingsActive[,40] <- (propListingsActive$predictedSalePrice - 
                              propListingsActive$list.Price.Num) / 
  propListingsActive$houseSizeSqFt
names(propListingsActive)[40] <- "breakEvenBuildCost"

#propListingsActive[,40] <- propListingsActive$MLS.Number
#names(propListingsActive) [40] <- "MLSNumberEOD"

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

write.csv(format(propProspectsReport, scientific=FALSE), "currentProspectsReport.csv", row.names = FALSE)
write.csv(format(propProspectsReportShort, scientific=FALSE), "currentProspectsReportShort.csv", row.names = FALSE)