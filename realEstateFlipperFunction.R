# This program can be run anytime. It doesn't change any files. It just loads in the database
# stored in "flipperAll.csv" and crunches the numbers on all "Active" listings. It then outputs a report
# titled "currentProspectsReport.csv" which contains all the prospects currently listed in the
# full database.

# This function updates the list of properties.
updateListings <- function() {
  # This section opens the past file and combines it with the new information
  
  # Open the archive data file
  propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)
  
  # Open the new file with the last week's data
  propListingsUpdate <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)
  
  propListingsUpdate <- addLatLong(propListingsUpdate)
  
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


# This is the function that runs all the flipping functions for a client.
# clientFlipperReport <- function(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities = "", recipients) {
#   subject <- paste("This Week's Results", "|", clientName, sep = " ")
#   
#   clientName <- gsub(" ", "", clientName, fixed = TRUE)
#   
#   # Function call to run the numbers
#   listingRepData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
#   
#   longPropReport <- longProspectsReport(listingRepData, cities)
#   
#   shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
#   shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)
#   
#   writeReports(longPropReport, shortTearDownPropReport, shortDiscountPropReport, clientName)
#   
#   if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport > 0)) {
#     emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
#   }
# }


# This function generates the list of profitable flipping properties
tearDownFlip <- function(newBuildCost, lowestNewBuildCost){
  
  # Constants for the program
  
  # The ratio of house square footage / lot size
  #houseRatio <- numeric()
  
  # The cost/sqft to build a new place
  newConsCostSqFt <- newBuildCost
  
  # Lowest possible build price
  lowestBuildCost <- lowestNewBuildCost
  
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
  propListings <- cleanData(propListings)
  
  if(file.exists("flipperStats.csv")) {
    propListingsUpdate <- read.csv("flipperStats.csv", stringsAsFactors = FALSE)
    propListingsUpdate <- cleanData(propListingsUpdate)
  }
  
  
  # Remove all the properties that don't have a "sold" price
  propListingsSold <- propListings[complete.cases(propListings[,43]),]
  
  propListingsSold$HOA.Fee <- 0
  
#  propListingsSold <- propListingsSold[complete.cases(propListingsSold),]
  
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

  
  # Figure out the biggest possible house in the area--this will put a cap on the biggest build size.
  
  houseSizeCap <- aggregate(propListingsSold$houseSqFt, list(propListingsSold$Area..), FUN = quantile, probs = 0.9)
  
  # Also, figure out the highest sale price in the area to cap the sale price, as well -- the 0.93 is for fees
  
  housePriceCap <- aggregate(propListingsSold$sell.Price.Num, list(propListingsSold$Area..), FUN = max)
  
  #housePriceCap[,2] <- housePriceCap[,2] * 0.93
  housePriceCap[,2] <- housePriceCap[,2] * 1
  
  # lubridate to change date: Sys.Date() - years(1)
  oneYear <- Sys.Date() - years(1)
  
  # Select only houses on lots over 2500 sqft, and that are less
  # than three years old. Than check the sale prices on those houses.
  # Never mind. Don't do this. Use everything and see how that looks.
  
  # propListSoldNew <- propListingsSold[as.numeric(propListingsSold$Age) <= 9, ]
  propListSoldNew <- propListingsSold[complete.cases(propListingsSold$Sale.Date),]

  propSoldOneYear <- propListSoldNew[propListSoldNew[,18] >= oneYear, ]
  
  # Use the "FD" method for making histograms
  
  #meanOverTime <- numeric()
  # meanLastYearArea <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Area..), FUN = mean)
  # meanLastYearArea <- merge(meanLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
  # meanLastYearZip <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Zip.Code), FUN = mean)
  # meanLastYearZip <- merge(meanLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
  # meanLastYearCity <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$Postal.City), FUN = mean)
  # meanLastYearCity <- merge(meanLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")
  # meanLastYearCounty <- aggregate(propSoldOneYear[,30], list(propSoldOneYear$County), FUN = mean)
  # meanLastYearCounty <- merge(meanLastYearCounty, table(factor(propSoldOneYear$County)), by.x = "Group.1", by.y = "Var1")

  
  medianLastYearArea <- aggregate(propSoldOneYear[,44], list(propSoldOneYear$Area..), FUN = median)
  medianLastYearArea <- merge(medianLastYearArea, table(factor(propSoldOneYear$Area..)), by.x = "Group.1", by.y = "Var1")
  medianLastYearZip <- aggregate(propSoldOneYear[,44], list(propSoldOneYear$Zip.Code), FUN = median)
  medianLastYearZip <- merge(medianLastYearZip, table(factor(propSoldOneYear$Zip.Code)), by.x = "Group.1", by.y = "Var1")
  medianLastYearCity <- aggregate(propSoldOneYear[,44], list(propSoldOneYear$Postal.City), FUN = median)
  medianLastYearCity <- merge(medianLastYearCity, table(factor(propSoldOneYear$Postal.City)), by.x = "Group.1", by.y = "Var1")
  medianLastYearCounty <- aggregate(propSoldOneYear[,44], list(propSoldOneYear$County), FUN = median)
  medianLastYearCounty <- merge(medianLastYearCounty, table(factor(propSoldOneYear$County)), by.x = "Group.1", by.y = "Var1")

  # Done with initial calculations.
  # This is the stuff that will have to be done on each property
  # to see if it passes snuff.
  
  # propListings$Status <- factor(propListings$Status)
  # propListings$Postal.City <- factor(propListings$Postal.City)
  # propListings$Zoning <- factor(propListings$Zoning)
  
  if(file.exists("flipperStats.csv")){
    propListingsActive <- propListingsUpdate[propListingsUpdate$Status == "Active", ]
  } else {
    propListingsActive <- propListings[propListings$Status == "Active", ]
  }  
  # This determines the proper house size ratio to use (making sure the numbers of samples are high enough)
  
  propListingsActive[,46] <- ifelse(houseRatioByArea[match(propListingsActive$Area.., houseRatioByArea$Group.1),3]>minSampleSize &
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
  
  names(propListingsActive)[46] <- "houseRatioNewBuild"
  
  # This determines the proper $/sf to use (making sure the numbers of samples are high enough)
  
  propListingsActive[,47] <- ifelse(medianLastYearArea[match(propListingsActive$Area.., medianLastYearArea$Group.1),3]>minSampleSize &
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
  names(propListingsActive)[47] <- "houseDollarPerSFNewBuild"
  
  # This is the size of house that can be built
  
  propListingsActive[,48] <- pmin(propListingsActive$lotSqFt * propListingsActive$houseRatioNewBuild, houseSizeCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
  names(propListingsActive)[48] <- "houseSizeSqFt"
  
  # Add a column for figuring out cost to build a new house
  
  propListingsActive[,49] <- propListingsActive$houseSizeSqFt * newConsCostSqFt
  names(propListingsActive)[49] <- "costToBuildHouse"
  
  # Figure out cost to buy the place and build a new house
  
  propListingsActive[,50] <- propListingsActive[,45] + propListingsActive[,49]
  names(propListingsActive)[50] <- "totalCostToBuild"
  
  # Predicted sale price of newly built house.
  # Deduct 7% for commission, fees, etc.
  
  #predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 0.93
  predPriceFinal <- propListingsActive$houseDollarPerSFNewBuild * 1
  
  propListingsActive[,51] <- pmin(propListingsActive[,48] * predPriceFinal, housePriceCap[match(propListingsActive$Area.., houseSizeCap$Group.1),2])
  names(propListingsActive)[51] <- "predictedSalePrice"
  
  # Figure out potential profit by comparing the difference between the predicted price and cost to build
  
  propListingsActive[,52] <- (propListingsActive[,51] * 0.93) - 
    propListingsActive[,50]
  names(propListingsActive)[52] <- "potentialProfit"
  
  # Sort the listings to see which are the most profitable
  # Sort by the profit percentage instead
  
  propListingsActive[,53] <- propListingsActive$potentialProfit / propListingsActive$totalCostToBuild
  names(propListingsActive)[53] <- "profitPercentOfInvestment"
  
  propListingsActive[,54] <- ((propListingsActive$predictedSalePrice * 0.93) -
                                propListingsActive$list.Price.Num) /
    propListingsActive$houseSizeSqFt
  names(propListingsActive)[54] <- "breakEvenBuildCost"
  
  propListingsActive[, "discountNewHousePerSquareFoot"] <- (propListingsActive$list.Price.Num / propListingsActive$houseSqFt) - 
    propListingsActive$houseDollarPerSFNewBuild
  
  propListingsActive <- propListingsActive[order(-propListingsActive[,53]), ]
  
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
  
  return(propListingsActive)
}


# This function cleans the data--in other words, fixes some of the variable
# types and adds some useful columns
cleanData <- function(propListings) {
  propListings[ , 40] <- as.numeric(gsub(",", "", as.character(propListings[ , 20])))
  names(propListings)[40] <- "houseSqFt"
  propListings[ , 41] <- as.numeric(gsub(",", "", as.character(propListings[ , 13])))
  names(propListings)[41] <- "lotSqFt"
  
  
  propListings[ , 42] <- propListings[ , 40] / propListings[ , 41]
  names(propListings)[42] <- "house.To.Lot.Size.Ratio"
  
  propListings[,43] <- (gsub(",", "", as.character(propListings[,19])))
  propListings[,43] <- as.numeric(gsub("\\$", "", as.character(propListings[,43])))
  names(propListings)[43] <- "sell.Price.Num"
  
  propListings[ , 44] <- propListings[ , 43] / propListings[ , 40]
  names(propListings)[44] <- "$/SqFt.House.Num"
  
  propListings[,45] <- (gsub(",", "", as.character(propListings[,11])))
  propListings[,45] <- as.numeric(gsub("\\$", "", as.character(propListings[,45])))
  names(propListings)[45] <- "list.Price.Num"
  
  # Eliminate all the listings with HOA fees, as they are probably
  # mistakenly listed as detached single family homes
  
  propListings$HOA.Fee <- as.numeric(gsub(",", "", as.character(propListings$HOA.Fee)))
  
  propListings <- propListings[propListings$HOA.Fee == 0 | is.na(propListings$HOA.Fee),]
  
  # Cleaning data
  
  propListings[,12] <- as.Date(propListings[,12], "%m/%d/%Y")
  propListings[,18] <- as.Date(propListings[,18], "%m/%d/%Y")
  
  propListings$Status <- factor(propListings$Status)
  propListings$Postal.City <- factor(propListings$Postal.City)
  propListings$Zoning <- factor(propListings$Zoning)
  
  return(propListings)
}



# This function will add the latitude and longitude to each of the new listings for the week.
addLatLong <- function(propListingsUpdate) {
  propAddressList <- paste(propListingsUpdate$Street.Address, propListingsUpdate$Postal.City, "CA",
                           propListingsUpdate$Zip.Code, sep = ", ")

  propListingsUpdate[, "propertyAddresses"] <- propAddressList
  
  streetList <- ldply(seq(1, nrow(propListingsUpdate)), function(x) 
    {a <- tryCatch(street2coordinates(propListingsUpdate$propertyAddresses[x]), error=function(err) as.data.frame(NA))
    if(nrow(a) == 0) {as.data.frame(NA)} else{a}})
  
  propListingsUpdate[, "latitude"] <- streetList$latitude
  propListingsUpdate[, "longitude"] <- streetList$longitude
  propListingsUpdate[, "latLongConf"] <- streetList$confidence
  
  return(propListingsUpdate)
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
addCompsReport <- function(baseListData) {
  compRangePercent <- 0.1
  
  highComp <- 1 + compRangePercent
  lowComp <- 1 - compRangePercent
  
  
  propListings <- read.csv("flipperAll.csv", stringsAsFactors = FALSE)
  propListings <- cleanData(propListings)
  
  propListingsSold <- propListings[complete.cases(propListings[,43]),]
  
  oneYear <- Sys.Date() - years(1)
  
  propListSoldNew <- propListingsSold[complete.cases(propListingsSold$Sale.Date),]
  
  propSoldOneYear <- propListSoldNew[propListSoldNew[,18] >= oneYear, ]
  
  propMLS <- sapply(x <- seq(1, length(baseListData$profitPercentOfInvestment)), function(x) 
    {subset(propSoldOneYear, Area.. == baseListData$Area..[x] & sell.Price.Num > 
              baseListData$predictedSalePrice[x]*lowComp & sell.Price.Num < 
              baseListData$predictedSalePrice[x]*highComp & houseSqFt > 
              baseListData$houseSizeSqFt[x]*lowComp & houseSqFt < 
              baseListData$houseSizeSqFt[x]*highComp)$MLS.Number})
  

  # Algorithm to determine the properties that have sold nearby
  propSoldOneYearLoc <- propSoldOneYear[complete.cases(propSoldOneYear$latLongConf), ]
  baseListLoc <- baseListData[complete.cases(baseListData$latLongConf), ]
  
  propSoldMat <- matrix(c(propSoldOneYearLoc$longitude, propSoldOneYearLoc$latitude), ncol = 2)
  locMat <- matrix(c(baseListLoc$longitude, baseListLoc$latitude), ncol = 2)
  
  #distTest <- distHaversine(locMat[1, ], propSoldMat)
  
  compList <- sapply(x <- seq(1, length(propMLS)), function(x) {paste0(propMLS[[x]], collapse = ", ")})
  
  compReport <- baseListData
  compReport[, "compPropCount"] <- lengths(propMLS)
  compReport[, "compProps"] <- compList
  
  return(compReport)
}



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
  longFileName <- paste("clientReports/", clientName, "LongProspectsReport.csv", sep = "")
  shortTearDownFileName <- paste("clientReports/", clientName, "TearDownProspectsReport.csv", sep = "")
  shortDiscountFileName <- paste("clientReports/", clientName, "DiscountProspectsReport.csv", sep = "")
  
  write.csv(format(longReport, scientific=FALSE), longFileName, row.names = FALSE)
  write.csv(format(shortTearDownReport, scientific=FALSE), shortTearDownFileName, row.names = FALSE)
  write.csv(format(shortDiscountPropReport, scientific=FALSE), shortDiscountFileName, row.names = FALSE)
}



# This function sends the report to interested parties
emailReports <- function(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport) {
  # This code at the end is to send an email to interested parties with the new
  # listings in it.
  
  sender <- "andy@tfo.vc"
  
  body <- "Attached are the results for the week"
  
  hostName <- "smtp.gmail.com"
  port <- 465
  userName <- "andy@tiberiusfo.com"
  passWord <- "uwmsjyfyxeinavll"
  
  shortTearDownFileLocation <- paste("clientReports/", clientName, "TearDownProspectsReport.csv", sep = "")
  shortDiscountFileLocation <- paste("clientReports/", clientName, "DiscountProspectsReport.csv", sep = "")
  
  # if (nrow(propProspectsReport) > 0) {
  #   send.mail(from = sender, to = "andy@tiberiusfo.com", subject = subject, body = body,
  #             smtp = list(host.name = hostName, port = port,
  #                         user.name = userName, passwd = passWord, ssl = TRUE),
  #             authenticate = TRUE, send = TRUE,
  #             attach.files = "newListingsReport.csv")
  # }
  if (nrow(shortTearDownPropReport) > 0 & nrow(shortDiscountPropReport) > 0) {
    send.mail(from = sender, to = recipients, subject = subject, body = body,
              smtp = list(host.name = hostName, port = port,
                          user.name = userName, passwd = passWord, ssl = TRUE),
              authenticate = TRUE, send = TRUE,
              attach.files = c(shortTearDownFileLocation, shortDiscountFileLocation))
  } else if (nrow(shortTearDownPropReport) > 0) {
    send.mail(from = sender, to = recipients, subject = subject, body = body,
              smtp = list(host.name = hostName, port = port,
                          user.name = userName, passwd = passWord, ssl = TRUE),
              authenticate = TRUE, send = TRUE,
              attach.files = shortTearDownFileLocation)
  } else {
    send.mail(from = sender, to = recipients, subject = subject, body = body,
              smtp = list(host.name = hostName, port = port,
                          user.name = userName, passwd = passWord, ssl = TRUE),
              authenticate = TRUE, send = TRUE,
              attach.files = shortDiscountFileLocation)
  }
}



# This function sends the long report to interested parties
emailLongReport <- function(recipients, subject, clientName, longPropReport) {
  # This code at the end is to send an email to interested parties with the new
  # listings in it.
  
  sender <- "andy@tfo.vc"
  
  body <- "Attached are the results for the week"
  
  hostName <- "smtp.gmail.com"
  port <- 465
  userName <- "andy@tiberiusfo.com"
  passWord <- "uwmsjyfyxeinavll"
  
  longTearDownFileLocation <- paste("clientReports/", clientName, "LongProspectsReport.csv", sep = "")
  
  if (nrow(longPropReport) > 0) {
    send.mail(from = sender, to = recipients, subject = subject, body = body,
              smtp = list(host.name = hostName, port = port,
                          user.name = userName, passwd = passWord, ssl = TRUE),
              authenticate = TRUE, send = TRUE,
              attach.files = longTearDownFileLocation)
  }
}