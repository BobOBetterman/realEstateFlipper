# This program can be run anytime. It doesn't change any files. The goal is that it will check all previous 
# results in the database, figure out which properties were flipped, and see how the real life results match 
# up with my predictions.


library(lubridate)
library(ggplot2)
library(plotly)

# work computer address
setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
#setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")


# Constants for the program

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



# Done with initial calculations.
# This is the stuff that will have to be done on each property
# to see if it passes snuff.

# This is the start of the section to determine make predictions on real world sales.


# This takes the sold listings and gets rid of everything that only has one parcel number listed.
propParcelUnique <- unique(propListingsSold$Parcel.Number[duplicated(propListingsSold$Parcel.Number)])
multParcels <- propListingsSold$Parcel.Number %in% propParcelUnique
propDupes <- propListingsSold[multParcels,]
propDupes$Parcel.Number <- factor(propDupes$Parcel.Number)

propSplit <- split(propDupes, propDupes$Parcel.Number)
flipParcelIndex <- sapply(seq_along(propSplit), function(x) {any(propSplit[[x]]$Age < 10, na.rm = TRUE) & any(propSplit[[x]]$Age >= 10, na.rm = TRUE)})
flipParcelConfirm <- propSplit[flipParcelIndex]
propFlipped <- do.call(rbind, unname(flipParcelConfirm))
propFlipped <- propFlipped[propFlipped$Parcel.Number != "",]


propFlipped[,c("houseRatioNewBuild", "houseDollarPerSFNewBuild", "houseSizeSqFt", "costToBuildHouse", 
               "totalCostToBuild", "predictedSalePrice", "potentialProfit", "profitPercentOfInvestment", 
               "breakEvenBuildCost")] <- NA


for(i in 1:nrow(propFlipped)){
  
  propListSoldPast <- subset(propListingsSold, Sale.Date < propFlipped$Sale.Date[i])
  
  
  # Figure out the stats for each area (Area > Zip Code > City > County)
  # I figure... Go back one month at a time, up to a year, to get a statistical large enough sampling.
  # Never mind--just do one year for each category, make sure there are at least 10 points, then do the check.
  # If there aren't enough in a category, go up to the next largest group and repeat.
  
  houseRatioByArea <- aggregate(propListSoldPast$house.To.Lot.Size.Ratio, list(propListSoldPast$Area..), FUN = quantile, probs = 0.9)
  houseRatioByArea <- merge(houseRatioByArea, table(factor(propListSoldPast$Area..)), by.x = "Group.1", by.y = "Var1")
  houseRatioByZip <- aggregate(propListSoldPast$house.To.Lot.Size.Ratio, list(propListSoldPast$Zip.Code), FUN = quantile, probs = 0.9)
  houseRatioByZip <- merge(houseRatioByZip, table(factor(propListSoldPast$Zip.Code)), by.x = "Group.1", by.y = "Var1")
  houseRatioByCity <- aggregate(propListSoldPast$house.To.Lot.Size.Ratio, list(propListSoldPast$Postal.City), FUN = quantile, probs = 0.9)
  houseRatioByCity <- merge(houseRatioByCity, table(factor(propListSoldPast$Postal.City)), by.x = "Group.1", by.y = "Var1")
  houseRatioByCounty <- aggregate(propListSoldPast$house.To.Lot.Size.Ratio, list(propListSoldPast$County), FUN = quantile, probs = 0.9)
  houseRatioByCounty <- merge(houseRatioByCounty, table(factor(propListSoldPast$County)), by.x = "Group.1", by.y = "Var1")
  
  # Figure out the biggest possible house in the area--this will put a cap on the biggest build size.
  
  houseSizeCap <- aggregate(propListSoldPast$houseSqFt, list(propListSoldPast$Area..), FUN = quantile, probs = 0.9)
  
  # Also, figure out the highest sale price in the area to cap the sale price, as well -- the 0.93 is for fees
  
  housePriceCap <- aggregate(propListSoldPast$sell.Price.Num, list(propListSoldPast$Area..), FUN = max)
  
  # This determines the cap after sale fees, but I'm comparing to the actual sale prices, so I want the actual price
  #housePriceCap[,2] <- housePriceCap[,2] * 0.93
  # This should find the actual sale price
  housePriceCap[,2] <- housePriceCap[,2] * 1
  
  # lubridate to change date: Sys.Date() - years(1)
  oneYear <- propFlipped$Sale.Date[i] - years(1)
  
  # Select only houses less than some years old. Than check the sale prices on those houses.
  
  propListSoldNew <- propListSoldPast[propListSoldPast$Age <= 9, ]
  
  propSoldOneYear <- propListSoldNew[propListSoldNew[,18] >= oneYear, ]
  
  # Use the "FD" method for making histograms
  
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

  

  propFlipped[i,32] <- ifelse(houseRatioByArea[match(propFlipped$Area..[i], houseRatioByArea$Group.1),3]>minSampleSize & 
                               !is.na(houseRatioByArea[match(propFlipped$Area..[i], houseRatioByArea$Group.1),3]>minSampleSize),
                             houseRatioByArea[match(propFlipped$Area..[i], houseRatioByArea$Group.1),2],
                             ifelse(houseRatioByZip[match(propFlipped$Zip.Code[i], houseRatioByZip$Group.1),3]>minSampleSize & 
                                      !is.na(houseRatioByZip[match(propFlipped$Zip.Code[i], houseRatioByZip$Group.1),3]>minSampleSize),
                                    houseRatioByZip[match(propFlipped$Zip.Code[i], houseRatioByZip$Group.1),2],
                                    ifelse(houseRatioByCity[match(propFlipped$Postal.City[i], houseRatioByCity$Group.1),3]>minSampleSize & 
                                             !is.na(houseRatioByCity[match(propFlipped$Postal.City[i], houseRatioByCity$Group.1),3]>minSampleSize),
                                           houseRatioByCity[match(propFlipped$Postal.City[i], houseRatioByCity$Group.1),2], 
                                           ifelse(houseRatioByCounty[match(propFlipped$County[i], houseRatioByCounty$Group.1),3]>minSampleSize & 
                                                    !is.na(houseRatioByCounty[match(propFlipped$County[i], houseRatioByCounty$Group.1),3]>minSampleSize),
                                                  houseRatioByCounty[match(propFlipped$County[i], houseRatioByCounty$Group.1),2], 1000000))))
  
  
  propFlipped[i,33] <- ifelse(medianLastYearArea[match(propFlipped$Area..[i], medianLastYearArea$Group.1),3]>minSampleSize & 
                               !is.na(medianLastYearArea[match(propFlipped$Area..[i], medianLastYearArea$Group.1),3]>minSampleSize),
                             medianLastYearArea[match(propFlipped$Area..[i], medianLastYearArea$Group.1),2],
                             ifelse(medianLastYearZip[match(propFlipped$Zip.Code[i], medianLastYearZip$Group.1),3]>minSampleSize & 
                                      !is.na(medianLastYearZip[match(propFlipped$Zip.Code[i], medianLastYearZip$Group.1),3]>minSampleSize),
                                    medianLastYearZip[match(propFlipped$Zip.Code[i], medianLastYearZip$Group.1),2],
                                    ifelse(medianLastYearCity[match(propFlipped$Postal.City[i], medianLastYearCity$Group.1),3]>minSampleSize & 
                                             !is.na(medianLastYearCity[match(propFlipped$Postal.City[i], medianLastYearCity$Group.1),3]>minSampleSize),
                                           medianLastYearCity[match(propFlipped$Postal.City[i], medianLastYearCity$Group.1),2], 
                                           ifelse(medianLastYearCounty[match(propFlipped$County[i], medianLastYearCounty$Group.1),3]>minSampleSize & 
                                                    !is.na(medianLastYearCounty[match(propFlipped$County[i], medianLastYearCounty$Group.1),3]>minSampleSize),
                                                  medianLastYearCounty[match(propFlipped$County[i], medianLastYearCounty$Group.1),2], 1000000000))))
  # propFlipped[,32] <- if(meanLastYearArea[match(propFlipped$Area.., meanLastYearArea$Group.1),3]>minSampleSize) {
  #                                   meanLastYearArea[match(propFlipped$Area.., meanLastYearArea$Group.1),2]} else {
  #                                   if(meanLastYearZip[match(propFlipped$Zip.Code, meanLastYearZip$Group.1),3]>minSampleSize) {
  #                                          meanLastYearZip[match(propFlipped$Zip.Code, meanLastYearZip$Group.1),2]} else {
  #                                          if(meanLastYearCity[match(propFlipped$Postal.City, meanLastYearCity$Group.1),3]>minSampleSize) {
  #                                                 meanLastYearCity[match(propFlipped$Postal.City, meanLastYearCity$Group.1),2]} else 1000}}
  
  
  # This is the size of house that can be built
  
  propFlipped[i,34] <- pmin(propFlipped$lotSqFt[i] * propFlipped$houseRatioNewBuild[i], houseSizeCap[match(propFlipped$Area..[i], houseSizeCap$Group.1),2])
  
  # Add a column for figuring out cost to build a new house
  
  propFlipped[i,35] <- propFlipped$houseSizeSqFt[i] * newConsCostSqFt
  
  
  # Figure out cost to buy the place and build a new house
  
  propFlipped[i,36] <- propFlipped[i,31] + propFlipped[i,35]
  
  # Predicted sale price of newly built house.
  
  # Deduct 7% for commission, fees, etc.
  #predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 0.93
  # This is the full sales price, for comparing with real world sales
  predPriceFinal <- propFlipped$houseDollarPerSFNewBuild[i] * 1
  
  propFlipped[i,37] <- pmin(propFlipped[i,34] * predPriceFinal, housePriceCap[match(propFlipped$Area..[i], houseSizeCap$Group.1),2])
  
  # Figure out potential profit by comparing the difference between the predicted price and cost to build
  
  propFlipped[i,38] <- propFlipped[i,37] - propFlipped[i,36]
  
  propFlipped[i,39] <- propFlipped$potentialProfit[i] / propFlipped$totalCostToBuild[i]
  
  propFlipped[i,40] <- (propFlipped$predictedSalePrice[i] - 
                         propFlipped$list.Price.Num[i]) / 
    propFlipped$houseSizeSqFt[i]
}


# This determines the proper house size ratio to use (making sure the numbers of samples are high enough)


# This determines the proper $/sf to use (making sure the numbers of samples are high enough)


# Sort the listings to see which are the most profitable
# Sort by the profit percentage instead


propFlipped$Parcel.Number <- factor(propFlipped$Parcel.Number)

flippedSplitReport <- subset(propFlipped, select = c("profitPercentOfInvestment", "MLS.Number", "Sale.Date",
                                                     "house.To.Lot.Size.Ratio", "houseRatioNewBuild", "houseSqFt",
                                                     "houseSizeSqFt", "$/SqFt.House.Num", "houseDollarPerSFNewBuild", 
                                                     "list.Price.Num", "totalCostToBuild", "sell.Price.Num", 
                                                     "predictedSalePrice", "potentialProfit", 
                                                     "lotSqFt", "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                                                     "Postal.City", "County", "Parcel.Number"))
names(flippedSplitReport) <- c("profitPercentOfInvestment", "MLS.Number", "Sale.Date",
                               "actual.house.To.Lot.Size.Ratio", "pred.houseRatioNewBuild", "actualHouseSqFt",
                               "predictHouseSizeSqFt", "actualDollarPerSqFt.House.Num", "predHouseDollarPerSFNewBuild", 
                                "actualList.Price.Num", "totalCostToBuild", "actualSell.Price.Num", 
                               "predictedSalePrice", "potentialProfit", 
                               "lotSqFt", "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                               "Postal.City", "County", "Parcel.Number")
flippedSplitReport[,c("futureActualHouseRatio", "futureActualHouseSqFt", "futureActualDollarPerSF", 
                      "futureActualSellPrice")] <- NA

flippedSplitReport <- flippedSplitReport[order(flippedSplitReport[,23], flippedSplitReport[,3]), ]

flippedSplitReport[,24] <- sapply(1:nrow(flippedSplitReport), function(x) {
  if(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23] & 
     !is.na(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23])) {flippedSplitReport[x+1, 4]} else NA})
flippedSplitReport[,25] <- sapply(1:nrow(flippedSplitReport), function(x) {
  if(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23] & 
     !is.na(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23])) {flippedSplitReport[x+1, 6]} else NA})
flippedSplitReport[,26] <- sapply(1:nrow(flippedSplitReport), function(x) {
  if(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23] & 
     !is.na(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23])) {flippedSplitReport[x+1, 8]} else NA})
flippedSplitReport[,27] <- sapply(1:nrow(flippedSplitReport), function(x) {
  if(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23] & 
     !is.na(flippedSplitReport[x, 23] == flippedSplitReport[x+1, 23])) {flippedSplitReport[x+1, 12]} else NA})

flipReportComplete <- flippedSplitReport[complete.cases(flippedSplitReport),]


# Make some graphs showing how the actual data stacks up to the predictions.

# Add residuals (actual value - predicted value) for each comparison:
flipReportComplete[,"houseRatioResidual"] <- flipReportComplete$futureActualHouseRatio - 
  flipReportComplete$pred.houseRatioNewBuild
flipReportComplete[,"houseSizeResidual"] <- flipReportComplete$futureActualHouseSqFt - 
  flipReportComplete$predictHouseSizeSqFt
flipReportComplete[,"dollarPerSFResidual"] <- flipReportComplete$futureActualDollarPerSF - 
  flipReportComplete$predHouseDollarPerSFNewBuild
flipReportComplete[,"priceResidual"] <- flipReportComplete$futureActualSellPrice - 
  flipReportComplete$predictedSalePrice

# This is for the house size ratio:
errorPlotRatio <- ggplot(flipReportComplete, aes(x = Parcel.Number, y = futureActualHouseRatio)) +
  geom_segment(aes(xend = Parcel.Number, yend = pred.houseRatioNewBuild), alpha = .5) +
  geom_point(aes(color = houseRatioResidual, size = abs(houseRatioResidual))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = pred.houseRatioNewBuild), shape = 1) +
  theme_bw()
errorPlotInterRatio <- ggplotly(errorPlotRatio)

# This is for the house size (in sq ft):
errorPlotSize <- ggplot(flipReportComplete, aes(x = Parcel.Number, y = futureActualHouseSqFt)) +
  geom_segment(aes(xend = Parcel.Number, yend = predictHouseSizeSqFt), alpha = .5) +
  geom_point(aes(color = houseSizeResidual, size = abs(houseSizeResidual))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = predictHouseSizeSqFt), shape = 1) +
  theme_bw()
errorPlotInterSize <- ggplotly(errorPlotSize)

# This is for the dollar per SF:
errorPlotDPS <- ggplot(flipReportComplete, aes(x = Parcel.Number, y = futureActualDollarPerSF)) +
  geom_segment(aes(xend = Parcel.Number, yend = predHouseDollarPerSFNewBuild), alpha = .5) +
  geom_point(aes(color = dollarPerSFResidual, size = abs(dollarPerSFResidual))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = predHouseDollarPerSFNewBuild), shape = 1) +
  theme_bw()
errorPlotInterDPS <- ggplotly(errorPlotRatio)

# This is for the sale price:
errorPlotPrice <- ggplot(flipReportComplete, aes(x = Parcel.Number, y = futureActualSellPrice)) +
  geom_segment(aes(xend = Parcel.Number, yend = predictedSalePrice), alpha = .5) +
  geom_point(aes(color = priceResidual, size = abs(priceResidual))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = predictedSalePrice), shape = 1) +
  theme_bw()
errorPlotInterPrice <- ggplotly(errorPlotPrice)



#flippedSplit <- split(flippedSplitReport, flippedSplitReport$Parcel.Number)




propFlipped <- propFlipped[order(-propFlipped[,39]), ]

propProspectsReport <- subset(propFlipped, profitPercentOfInvestment >= lowestProfit,
                              select = c("profitPercentOfInvestment", "MLS.Number",
                                         "houseRatioNewBuild", "houseSizeSqFt", "list.Price.Num",
                                         "houseDollarPerSFNewBuild", "totalCostToBuild",
                                         "predictedSalePrice", "potentialProfit", "lotSqFt",
                                         "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                                         "Postal.City", "County"))

propProspectsReportShort <- subset(propFlipped, breakEvenBuildCost >= lowestBuildCost,
                                   select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                              "houseSizeSqFt", "breakEvenBuildCost", "profitPercentOfInvestment"))
names(propProspectsReportShort)[5] <- "projectedHouseSizeSqFt"

propProspectsReportShort <- propProspectsReportShort[order(-propProspectsReportShort$breakEvenBuildCost), ]


# Use this print command to get rid of the annoying scientific notation in the printed table
#format(propProspectsReport, scientific=FALSE)

#write.csv(format(propProspectsReport, scientific=FALSE), "predictionsReport.csv", row.names = FALSE)