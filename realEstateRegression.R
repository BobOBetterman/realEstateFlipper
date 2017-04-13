# This file contains all the functions for the wrapper that calls them for each client. If "flipperStats.csv" exists, 
# it updates the archived data file with the new results. Otherwise, it doesn't make any changes to the existing 
# data--it will just write reports and email them, assuming those functions aren't commented out in the wrapper call.



# This function updates the list of properties.
updateListings <- function() {
  # This section opens the past file and combines it with the new information
  
  # Open the archive data file
  propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)
  
  # Open the new file with the last week's data
  propListingsUpdate <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)
  
  # Figure out the MLS numbers in the new data that already exist in the data, and note the numbers that don't appear
  updatedListings <- na.omit(match(propListingsUpdate$MLS.Number, propListings$MLS.Number))
  naRows <- attr(updatedListings, "na.action")
  
  # Remove the rows that don't exist in both from the new data
  if(length(naRows) > 0) {
    propChanges <- propListingsUpdate[-naRows,]
    
    # Overwrite all the old values with the new values
    propListings[updatedListings, ] <- propChanges
    
    # Make a data frame with only the new MLS numbers, and then attach it to the full data file
    propNew <- propListingsUpdate[naRows, ]
    propCombined <- rbind(propListings, propNew)
    
    propListings <- propCombined
  } else {
    propListings[updatedListings, ] <- propListingsUpdate
    propCombined <- propListings
  }
  
  
  write.csv(propListingsUpdate, "flipperStats.csv", row.names = FALSE)
  write.csv(propCombined, "flipperAll.csv", row.names = FALSE)
}



# This function generates the list of profitable flipping properties
tearDownFlip <- function(newBuildCost, lowestNewBuildCost){
  # The cost/sqft to build a new place
  newConsCostSqFt <- newBuildCost
  
  # Lowest possible build price
  lowestBuildCost <- lowestNewBuildCost
  
  # Necessary number of samples to be statistically significant
  minSampleSize <- 9
  
  # This is the lowest percentage of profit we're willing to risk investing for
  lowestProfit <- 0.1
  
  # This is the range of properties to compare the predicted prices and sizes to
  compRangePercent <- 0.1
  
  
  highComp <- 1 + compRangePercent
  lowComp <- 1 - compRangePercent
  
  
  
  # Start of sold property data analysis
  
  # This can be used for a self-contained file (i.e., five years of data on one city)
  #propListings <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)
  
  # This is the file that contains all the different download files from the MLS site--obtained
  # by running the combineFlipperStats.R program
  propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)
  propListings <- cleanData(propListings)
  
  if(file.exists("flipperStats.csv")) {
    propListingsUpdate <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)
    propListingsUpdate <- cleanData(propListingsUpdate)
  }
  
  
  # Remove all the properties that don't have a "sold" price
  propListingsSold <- propListings[complete.cases(propListings$sell.Price.Num),]
  
  # propListingsSold <- propListingsSold[propListingsSold$lotSqFt>0,]
  # propListingsSold <- propListingsSold[propListingsSold$houseSqFt>0,]
  
  propListingsSold <- propListingsSold[complete.cases(propListingsSold$house.To.Lot.Size.Ratio),]
  
  # Figure out the stats for each area (Area > Zip Code > City > County)
  # Do one year for each category, make sure there are at least 10 points, then do the check.
  # If there aren't enough in a category, go up to the next largest group and repeat.
  
  houseRatioByArea <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)
  houseRatioByArea <- merge(houseRatioByArea, table(factor(propListingsSold$Area..)), by.x = "Group.1", by.y = "Var1")
  houseRatioByZip <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Zip.Code), FUN = quantile, probs = 0.9)
  houseRatioByZip <- merge(houseRatioByZip, table(factor(propListingsSold$Zip.Code)), by.x = "Group.1", by.y = "Var1")
  houseRatioByCity <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$Postal.City), FUN = quantile, probs = 0.9)
  houseRatioByCity <- merge(houseRatioByCity, table(factor(propListingsSold$Postal.City)), by.x = "Group.1", by.y = "Var1")
  houseRatioByCounty <- aggregate(propListingsSold$house.To.Lot.Size.Ratio, list(propListingsSold$County), FUN = quantile, probs = 0.9)
  houseRatioByCounty <- merge(houseRatioByCounty, table(factor(propListingsSold$County)), by.x = "Group.1", by.y = "Var1")
  
  
  # Figure out the biggest possible house in the area--this will put a cap on the biggest build size.
  
  houseSizeCap <- aggregate(propListingsSold$houseSqFt, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)
  
  # Also, figure out the highest sale price in the area to cap the sale price, as well
  
  housePriceCap <- aggregate(propListingsSold$sell.Price.Num, list(propListingsSold$Area..), FUN = max)
  
  # housePriceCap[,2] <- housePriceCap[,2] * 1
  
  # lubridate to change date: Sys.Date() - years(1)
  oneYear <- Sys.Date() - years(1)
  
  # propListSoldNew <- propListingsSold[as.numeric(propListingsSold$Age) <= 9, ]
  propListSoldNew <- propListingsSold[complete.cases(propListingsSold$Sale.Date),]
  
  propSoldOneYear <- propListSoldNew[propListSoldNew$Sale.Date >= oneYear, ]
  
  medianLastYearArea <- aggregate(propSoldOneYear$dollarPerSqFt.House.Num, list(propSoldOneYear$Area..), FUN = median)
  medianLastYearArea <- merge(medianLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
  medianLastYearZip <- aggregate(propSoldOneYear$dollarPerSqFt.House.Num, list(propSoldOneYear$Zip.Code), FUN = median)
  medianLastYearZip <- merge(medianLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
  medianLastYearCity <- aggregate(propSoldOneYear$dollarPerSqFt.House.Num, list(propSoldOneYear$Postal.City), FUN = median)
  medianLastYearCity <- merge(medianLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")
  medianLastYearCounty <- aggregate(propSoldOneYear$dollarPerSqFt.House.Num, list(propSoldOneYear$County), FUN = median)
  medianLastYearCounty <- merge(medianLastYearCounty, table(factor(propSoldOneYear$County)), by.x = "Group.1", by.y = "Var1")
  
  # Done with initial calculations.
  # This is the stuff that will have to be done on each property
  # to see if it passes snuff.
  
  
  if(file.exists("flipperStats.csv")){
    propListingsActive <- propListingsUpdate[propListingsUpdate$Status == "Active", ]
  } else {
    propListingsActive <- propListings[propListings$Status == "Active", ]
  }  
  # This determines the proper house size ratio to use (making sure the numbers of samples are high enough)
  
  propListingsActive[, "houseRatioNewBuild"] <- ifelse(houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),3]>minSampleSize &
                                      !is.na(houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),3]>minSampleSize),
                                    houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),2],
                                    ifelse(houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),3]>minSampleSize &
                                             !is.na(houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),3]>minSampleSize),
                                           houseRatioByZip[match(propListingsActive$Zip.Code, houseRatioByZip$Group.1),2],
                                           ifelse(houseRatioByCity[match(propListingsActive$Postal.City, houseRatioByCity$Group.1),3]>minSampleSize &
                                                    !is.na(houseRatioByCity[match(propListingsActive$Postal.City, houseRatioByCity$Group.1),3]>minSampleSize),
                                                  houseRatioByCity[match(propListingsActive$Postal.City, 
                                                                         houseRatioByCity$Group.1),2], 
                                                  houseRatioByCounty[match(propListingsActive$County, 
                                                                           houseRatioByCounty$Group.1),2])))
  
  # This determines the proper $/sf to use (making sure the numbers of samples are high enough)
  
  propListingsActive[, "houseDollarPerSFNewBuild"] <- ifelse(medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),3]>minSampleSize &
                                      !is.na(medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),3]>minSampleSize),
                                    medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),2],
                                    ifelse(medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),3]>minSampleSize &
                                             !is.na(medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),3]>minSampleSize),
                                           medianLastYearZip[match(propListingsActive$Zip.Code, medianLastYearZip$Group.1),2],
                                           ifelse(medianLastYearCity[match(propListingsActive$Postal.City, medianLastYearCity$Group.1),3]>minSampleSize &
                                                    !is.na(medianLastYearCity[match(propListingsActive$Postal.City, medianLastYearCity$Group.1),3]>minSampleSize),
                                                  medianLastYearCity[match(propListingsActive$Postal.City, 
                                                                           medianLastYearCity$Group.1),2], 
                                                  medianLastYearCounty[match(propListingsActive$County, 
                                                                             medianLastYearCounty$Group.1),2])))
  # propListingsActive[,32] <- if(meanLastYearArea[match(propListingsActive$Area.., meanLastYearArea$Group.1),3]>minSampleSize) {
  #                                   meanLastYearArea[match(propListingsActive$Area.., meanLastYearArea$Group.1),2]} else {
  #                                   if(meanLastYearZip[match(propListingsActive$Zip.Code, meanLastYearZip$Group.1),3]>minSampleSize) {
  #                                          meanLastYearZip[match(propListingsActive$Zip.Code, meanLastYearZip$Group.1),2]} else {
  #                                          if(meanLastYearCity[match(propListingsActive$Postal.City, meanLastYearCity$Group.1),3]>minSampleSize) {
  #                                                 meanLastYearCity[match(propListingsActive$Postal.City, meanLastYearCity$Group.1),2]} else 1000}}
  
  # This is the size of house that can be built
  
  propListingsActive[, "houseSizeSqFt"] <- pmin(propListingsActive$lotSqFt * propListingsActive$houseRatioNewBuild, houseSizeCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
  
  # Add a column for figuring out cost to build a new house
  
  propListingsActive[, "costToBuildHouse"] <- propListingsActive$houseSizeSqFt * newConsCostSqFt
  
  # Figure out cost to buy the place and build a new house
  
  propListingsActive[, "totalCostToBuild"] <- propListingsActive$list.Price.Num + propListingsActive$costToBuildHouse
  
  # Predicted sale price of newly built house.
  predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild
  
  propListingsActive[, "predictedSalePrice"] <- pmin(propListingsActive$houseSizeSqFt * predPriceFinal, housePriceCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
  
  # Figure out potential profit by comparing the difference between the predicted price and cost to build
  
  propListingsActive[, "potentialProfit"] <- (propListingsActive$predictedSalePrice * 0.93) - 
    propListingsActive$totalCostToBuild
  
  # Sort the listings to see which are the most profitable
  # Sort by the profit percentage instead
  
  propListingsActive[, "profitPercentOfInvestment"] <- propListingsActive$potentialProfit / propListingsActive$totalCostToBuild
  
  propListingsActive[, "breakEvenBuildCost"] <- ((propListingsActive$predictedSalePrice * 0.93) -
                                propListingsActive$list.Price.Num) / propListingsActive$houseSizeSqFt
  
  propListingsActive[, "discountNewHousePerSquareFoot"] <- (propListingsActive$list.Price.Num / propListingsActive$houseSqFt) - 
    propListingsActive$houseDollarPerSFNewBuild
  
  propListingsActive <- propListingsActive[order(-propListingsActive$profitPercentOfInvestment), ]
  
  # Add columns for each of the house ratio groups--put the counts in them, so we know which one the program is 
  # using
  propListingsActive[, "areaRatioCount"] <- houseRatioByArea[match(propListingsActive$Area.., 
                                                                   houseRatioByArea$Group.1), "Freq"]
  propListingsActive[, "zipRatioCount"] <- houseRatioByZip[match(propListingsActive$Zip.Code, 
                                                                 houseRatioByZip$Group.1), "Freq"]
  propListingsActive[, "cityRatioCount"] <- houseRatioByCity[match(propListingsActive$Postal.City, 
                                                                   houseRatioByCity$Group.1), "Freq"]
  propListingsActive[, "countyRatioCount"] <- houseRatioByCounty[match(propListingsActive$County, 
                                                                       houseRatioByCounty$Group.1), "Freq"]
  
  # Add columns for each of the price groups--put the counts in them, so we know which one the program is 
  # using
  propListingsActive[, "areaPriceCount"] <- medianLastYearArea[match(propListingsActive$Area.., 
                                                                     medianLastYearArea$Group.1), "Freq"]
  propListingsActive[, "zipPriceCount"] <- medianLastYearZip[match(propListingsActive$Zip.Code, 
                                                                   medianLastYearZip$Group.1), "Freq"]
  propListingsActive[, "cityPriceCount"] <- medianLastYearCity[match(propListingsActive$Postal.City, 
                                                                     medianLastYearCity$Group.1), "Freq"]
  propListingsActive[, "countyPriceCount"] <- medianLastYearCounty[match(propListingsActive$County, 
                                                                         medianLastYearCounty$Group.1), "Freq"]
  
  
  # This area figures out the comp properties (properties sold in the last year that are comparable in size 
  # and price)
  propMLS <- sapply(x <- seq(1, length(propListingsActive$profitPercentOfInvestment)), function(x) 
  {subset(propSoldOneYear, Area.. == propListingsActive$Area..[x] & sell.Price.Num > 
            propListingsActive$predictedSalePrice[x]*lowComp & sell.Price.Num < 
            propListingsActive$predictedSalePrice[x]*highComp & houseSqFt > 
            propListingsActive$houseSizeSqFt[x]*lowComp & houseSqFt < 
            propListingsActive$houseSizeSqFt[x]*highComp)$MLS.Number})
  
  compList <- sapply(x <- seq(1, length(propMLS)), function(x) {paste0(propMLS[[x]], collapse = ", ")})
  
  propListingsActive[, "compPropCount"] <- lengths(propMLS)
  propListingsActive[, "compProps"] <- compList
  
  return(propListingsActive)
}


# This function cleans the data--in other words, fixes some of the variable
# types and adds some useful columns
cleanData <- function(propListings) {
  propListings[ , "houseSqFt"] <- as.numeric(gsub(",", "", as.character(propListings$Sq.Ft.Total)))
  propListings[ , "lotSqFt"] <- as.numeric(gsub(",", "", as.character(propListings$Lot.Size)))
  
  propListings[ , "house.To.Lot.Size.Ratio"] <- propListings$houseSqFt / propListings$lotSqFt
  
  propListings[, "sell.Price.Num"] <- (gsub(",", "", as.character(propListings$Sale.Price)))
  propListings[, "sell.Price.Num"] <- as.numeric(gsub("\\$", "", as.character(propListings$sell.Price.Num)))
  
  propListings[ , "dollarPerSqFt.House.Num"] <- propListings$sell.Price.Num / propListings$houseSqFt
  
  propListings[, "list.Price.Num"] <- (gsub(",", "", as.character(propListings$List.Price)))
  propListings[, "list.Price.Num"] <- as.numeric(gsub("\\$", "", as.character(propListings$list.Price.Num)))
  
  # Eliminate all the listings with HOA fees, as they are probably
  # mistakenly listed as detached single family homes
  # 
  # propListings$HOA.Fee <- as.numeric(gsub(",", "", as.character(propListings$HOA.Fee)))
  # 
  # propListings <- propListings[propListings$HOA.Fee == 0 | is.na(propListings$HOA.Fee),]
  
  # Cleaning data
  
  propListings$Listing.Date <- as.Date(propListings$Listing.Date, "%m/%d/%Y")
  propListings$Sale.Date <- as.Date(propListings$Sale.Date, "%m/%d/%Y")
  
  propListings$Status <- factor(propListings$Status)
  propListings$Postal.City <- factor(propListings$Postal.City)
  propListings$Zoning <- factor(propListings$Zoning)
  
  return(propListings)
}



# This function will make the long version of the prospects report (the one I prefer)
longProspectsReport <- function(activeProp, cities) {
  # This is the lowest percentage of profit we're willing to risk investing for
  lowestProfit <- 0.1
  
  if(cities[1] == "" & length(cities) == 1){
    propProspectsReport <- subset(activeProp, profitPercentOfInvestment >= lowestProfit,
                                  select = c("profitPercentOfInvestment", "MLS.Number",
                                             "houseRatioNewBuild", "houseSizeSqFt", "list.Price.Num",
                                             "houseDollarPerSFNewBuild", "totalCostToBuild",
                                             "predictedSalePrice", "potentialProfit", "lotSqFt",
                                             "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                                             "Postal.City", "County", "areaRatioCount", "zipRatioCount", 
                                             "cityRatioCount", "countyRatioCount", "areaPriceCount", 
                                             "zipPriceCount", "cityPriceCount", "countyPriceCount", 
                                             "compPropCount", "compProps"))
  } else {
    
    propProspectsReport <- subset(activeProp, profitPercentOfInvestment >= lowestProfit & 
                                    activeProp$Postal.City %in% cities,
                                  select = c("profitPercentOfInvestment", "MLS.Number",
                                             "houseRatioNewBuild", "houseSizeSqFt", "list.Price.Num",
                                             "houseDollarPerSFNewBuild", "totalCostToBuild",
                                             "predictedSalePrice", "potentialProfit", "lotSqFt",
                                             "Age", "DOM", "Street.Address", "Area..", "Zip.Code",
                                             "Postal.City", "County", "areaRatioCount", "zipRatioCount", 
                                             "cityRatioCount", "countyRatioCount", "areaPriceCount", 
                                             "zipPriceCount", "cityPriceCount", "countyPriceCount", 
                                             "compPropCount", "compProps"))
  }
  
  return(propProspectsReport)
}



# This function will add the nearby comps to the report.
# addCompsReport <- function(baseListData) {
#   compRangePercent <- 0.1
#   
#   highComp <- 1 + compRangePercent
#   lowComp <- 1 - compRangePercent
#   
#   
#   propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)
#   propListings <- cleanData(propListings)
#   
#   propListingsSold <- propListings[complete.cases(propListings$sell.Price.Num),]
#   
#   oneYear <- Sys.Date() - years(1)
#   
#   propListSoldNew <- propListingsSold[complete.cases(propListingsSold$Sale.Date),]
#   
#   propSoldOneYear <- propListSoldNew[propListSoldNew$Sale.Date >= oneYear, ]
#   
#   propMLS <- sapply(x <- seq(1, length(baseListData$profitPercentOfInvestment)), function(x) 
#   {subset(propSoldOneYear, Area.. == baseListData$Area..[x] & sell.Price.Num > 
#             baseListData$predictedSalePrice[x]*lowComp & sell.Price.Num < 
#             baseListData$predictedSalePrice[x]*highComp & houseSqFt > 
#             baseListData$houseSizeSqFt[x]*lowComp & houseSqFt < 
#             baseListData$houseSizeSqFt[x]*highComp)$MLS.Number})
#   
#   
#   # Algorithm to determine the properties that have sold nearby
#   # propSoldOneYearLoc <- propSoldOneYear[complete.cases(propSoldOneYear$latLongConf), ]
#   # baseListLoc <- baseListData[complete.cases(baseListData$latLongConf), ]
#   # 
#   # propSoldMat <- matrix(c(propSoldOneYearLoc$longitude, propSoldOneYearLoc$latitude), ncol = 2)
#   # locMat <- matrix(c(baseListLoc$longitude, baseListLoc$latitude), ncol = 2)
#   
#   #distTest <- distHaversine(locMat[1, ], propSoldMat)
#   
#   compList <- sapply(x <- seq(1, length(propMLS)), function(x) {paste0(propMLS[[x]], collapse = ", ")})
#   
#   compReport <- baseListData
#   compReport[, "compPropCount"] <- lengths(propMLS)
#   compReport[, "compProps"] <- compList
#   
#   return(compReport)
# }



# This function makes the short version that Victor likes, and will go to clients
shortTearDownProspectsReport <- function(activeProp, lowestNewBuildCost, cities) {
  if(cities[1] == "" & length(cities) == 1) {
    propProspectsReportShort <- subset(activeProp, breakEvenBuildCost >= lowestNewBuildCost,
                                       select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                                  "houseSizeSqFt", "predictedSalePrice", 
                                                  "breakEvenBuildCost", 
                                                  "profitPercentOfInvestment", "compPropCount"))
  } else {
    propProspectsReportShort <- subset(activeProp, breakEvenBuildCost >= lowestNewBuildCost & 
                                         activeProp$Postal.City %in% cities,
                                       select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                                  "houseSizeSqFt", "predictedSalePrice", 
                                                  "breakEvenBuildCost", 
                                                  "profitPercentOfInvestment", "compPropCount"))
  }
  names(propProspectsReportShort)[5] <- "projectedHouseSizeSqFt"
  
  propProspectsReportShort <- propProspectsReportShort[order(-propProspectsReportShort$breakEvenBuildCost), ]
  
  return(propProspectsReportShort)
}


shortDiscountProspectsReport <- function(activeProp, lowestNewBuildCost, cities) {
  if(cities[1] == "" & length(cities) == 1) {
    propProspectsReportShort <- subset(activeProp, discountNewHousePerSquareFoot <= lowestDiscount,
                                       select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                                  "houseSizeSqFt", "predictedSalePrice", 
                                                  "discountNewHousePerSquareFoot",  
                                                  "compPropCount"))
  } else {
    propProspectsReportShort <- subset(activeProp, discountNewHousePerSquareFoot <= lowestDiscount & 
                                         activeProp$Postal.City %in% cities,
                                       select = c("Street.Address", "Postal.City", "lotSqFt", "list.Price.Num",
                                                  "houseSizeSqFt", "predictedSalePrice", 
                                                  "discountNewHousePerSquareFoot", 
                                                  "compPropCount"))
  }
  names(propProspectsReportShort)[5] <- "projectedHouseSizeSqFt"
  
  propProspectsReportShort <- propProspectsReportShort[order(propProspectsReportShort$discountNewHousePerSquareFoot), ]
  
  return(propProspectsReportShort)
}


# This function writes the reports to hard copies
writeReports <- function(longReport, shortTearDownReport, shortDiscountPropReport, clientName) {
  
  allFileNames <- character()
  allFileNames[1] <- paste("clientReports/", clientName, "LongProspectsReport.csv", sep = "")
  allFileNames[2] <- paste("clientReports/", clientName, "TearDownProspectsReport.csv", sep = "")
  allFileNames[3] <- paste("clientReports/", clientName, "DiscountProspectsReport.csv", sep = "")
  
  write.csv(format(longReport, scientific=FALSE), allFileNames[1], row.names = FALSE)
  write.csv(format(shortTearDownReport, scientific=FALSE), allFileNames[2], row.names = FALSE)
  write.csv(format(shortDiscountPropReport, scientific=FALSE), allFileNames[3], row.names = FALSE)
  
  return(allFileNames)
}