# This function calls all other real estate functions, with specifications for each individual client. It doesn't
# change any existing files itself, although the functions it calls may do so (specifically, flipperUpdateData.R 
# should be called first, as it will update the flipperAll.csv file with the new listings for the week). Unless I 
# decide that the updating functions should be part of another program. In fact, yeah, for now this won't access 
# the updating functions.

library(lubridate)
library(mailR)
#library(RDSTK)
library(geosphere)
library(plyr)
library(googlesheets)
library(dplyr)

# work computer address
#setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")

source("realEstateRegression.R")
source("privateFlipperFunctions.R")



# First, update the property list (but only if the new file exists).
if(file.exists("flipperStats.csv")) {
  updateListings()
}


clientList <- getClientList()


# Have a different loop for each type of search.

# Start with the teardown properties.

clientListTD <- clientList[clientList$teardown == TRUE & !is.na(clientList$teardown),]


# Do these things for each client


for (i in 1:nrow(clientListTD)) {
  # Client name
  clientName <- clientListTD[i, "clientName"]
  
  # The cost/sqft to build a new place
  buildCost <- clientListTD[i, "buildCost"]
  
  # Smallest discount to look at
  lowestDiscount <- clientListTD[i, "lowestDiscount"]
  
  # The cities to search in
  if(!is.na(clientListTD[i, "cities"])) {
    cities <- strsplit(clientListTD[i, "cities"], split = ", ")[[1]]
  } else {
    cities <- clientListTD[i, "cities"]
  }
  
  # Email information
  if(!is.na(clientListTD[i, "recipients"])) {
    recipients <- strsplit(clientListTD[i, "recipients"], split = ", ")[[1]]
  } else {
    recipients <- clientListTD[i, "recipients"]
  }
  #####################################################
  #####################################################
  #####################################################
  
  
  # Lowest possible build price
  lowestBreakEvenBuild <- buildCost
  
  #clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)
  
  subject <- paste("This Week's Results", "|", clientName, sep = " ")
  
  clientName <- gsub(" ", "", clientName, fixed = TRUE)
  
  # Function call to run the numbers
  listingRepData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
  
  
  longListReport <- longProspectsReport(listingRepData, cities)
  #longPropReport <- addCompsReport(longListReport)
  
  shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
  shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)
  
  allFileNames <- writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)
  
  # if ((nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) & clientListTD[i, "shortReport"] 
  #     == TRUE & !is.na(clientListTD[i, "shortReport"])) {
  #   emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport, allFileNames)
  # }
  # 
  # if (nrow(longListReport) > 0 & clientListTD[i, "longReport"] == TRUE & !is.na(clientListTD[i, "longReport"])) {
  #   emailLongReport(recipients, subject, clientName, longListReport, allFileNames)
  # }
}