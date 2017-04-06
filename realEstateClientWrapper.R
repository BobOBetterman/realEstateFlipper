# This function calls all other real estate functions, with specifications for each individual client. It doesn't
# change any existing files itself, although the functions it calls may do so (specifically, flipperUpdateData.R 
# should be called first, as it will update the flipperAll.csv file with the new listings for the week). Unless I 
# decide that the updating functions should be part of another program. In fact, yeah, for now this won't access 
# the updating functions.

library(lubridate)
library(mailR)
library(RDSTK)
library(geosphere)
library(plyr)

# work computer address
#setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")

source("realEstateFlipperFunction.R")



# First, update the property list (but only if the new file exists).
if(file.exists("flipperStats.csv")) {
  updateListings()
}


# Have a different function call for each client.

# This is for the first client.

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# Andrew Hill -- Tear Down
#####################################################################################################################
# Client name
clientName <- "Andrew Hill"

# The cost/sqft to build a new place
buildCost <- 300

# Smallest discount to look at
lowestDiscount <- -200

# The cities to search in
cities <- ""

# Email information
recipients <- c("andy@tiberiusfo.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

#clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)

subject <- paste("This Week's Results", "|", clientName, sep = " ")

clientName <- gsub(" ", "", clientName, fixed = TRUE)

# Function call to run the numbers
baseListData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
listingRepData <- addCompsReport(baseListData)

longListReport <- longProspectsReport(listingRepData, cities)
#longPropReport <- addCompsReport(longListReport)

shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)

writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)

# if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) {
#   emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
# }

if (nrow(longListReport) > 0) {
#  emailLongReport(recipients, subject, clientName, longListReport)
}
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# Victor Chiang -- Tear Down
#####################################################################################################################
# Client name
clientName <- "Victor Chiang"

# The cost/sqft to build a new place
buildCost <- 300

# Smallest discount to look at
lowestDiscount <- -200

# The cities to search in
cities <- ""

# Email information
recipients <- c("vchiang@gmail.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

#clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)

subject <- paste("This Week's Results", "|", clientName, sep = " ")

clientName <- gsub(" ", "", clientName, fixed = TRUE)

# Function call to run the numbers
baseListData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
listingRepData <- addCompsReport(baseListData)

longListReport <- longProspectsReport(listingRepData, cities)
#longPropReport <- addCompsReport(longListReport)

shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)

writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)

if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) {
#  emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
}

# if (nrow(longListReport) > 0) {
#   emailLongReport(recipients, subject, clientName, longListReport)
# }
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# Slum Lords -- Tear Down
#####################################################################################################################
# Client name
clientName <- "Slum Lords"

# The cost/sqft to build a new place
buildCost <- 300

# Smallest discount to look at
lowestDiscount <- -200

# The cities to search in
cities <- ""

# Email information
recipients <- c("andy@tiberiusfo.com", "ssoria@gmail.com", "gmattd@gmail.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

#clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)

subject <- paste("This Week's Results", "|", clientName, sep = " ")

clientName <- gsub(" ", "", clientName, fixed = TRUE)

# Function call to run the numbers
baseListData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
listingRepData <- addCompsReport(baseListData)

longListReport <- longProspectsReport(listingRepData, cities)
#longPropReport <- addCompsReport(longListReport)

shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)

writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)

# if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) {
#   emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
# }

if (nrow(longListReport) > 0) {
#  emailLongReport(recipients, subject, clientName, longListReport)
}
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# Rob Pavley -- Tear Down
#####################################################################################################################
# Client name
clientName <- "Rob Pavley"

# The cost/sqft to build a new place
buildCost <- 300

# Smallest discount to look at
lowestDiscount <- -200

# The cities to search in
cities <- ""

# Email information
recipients <- c("rob@nestoffer.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

#clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)

subject <- paste("This Week's Results", "|", clientName, sep = " ")

clientName <- gsub(" ", "", clientName, fixed = TRUE)

# Function call to run the numbers
baseListData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
listingRepData <- addCompsReport(baseListData)

longListReport <- longProspectsReport(listingRepData, cities)
#longPropReport <- addCompsReport(longListReport)

shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)

writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)

if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) {
#  emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
}

# if (nrow(longListReport) > 0) {
# #  emailLongReport(recipients, subject, clientName, longListReport)
# }
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# Marc Abramowitz -- Tear Down
#####################################################################################################################
# Client name
clientName <- "Marc Abramowitz"

# The cost/sqft to build a new place
buildCost <- 300

# Smallest discount to look at
lowestDiscount <- -200

# The cities to search in
cities <- c("Woodside", "Palo Alto", "Atherton")

# Email information
recipients <- c("andy@tiberiusfo.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

#clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)

subject <- paste("This Week's Results", "|", clientName, sep = " ")

clientName <- gsub(" ", "", clientName, fixed = TRUE)

# Function call to run the numbers
baseListData <- tearDownFlip(buildCost, lowestBreakEvenBuild)
listingRepData <- addCompsReport(baseListData)

longListReport <- longProspectsReport(listingRepData, cities)
#longPropReport <- addCompsReport(longListReport)

shortTearDownPropReport <- shortTearDownProspectsReport(listingRepData, lowestBreakEvenBuild, cities)
shortDiscountPropReport <- shortDiscountProspectsReport(listingRepData, lowestDiscount, cities)

writeReports(longListReport, shortTearDownPropReport, shortDiscountPropReport, clientName)

if (nrow(shortTearDownPropReport) > 0 | nrow(shortDiscountPropReport) > 0) {
#  emailReports(recipients, subject, clientName, shortTearDownPropReport, shortDiscountPropReport)
}

# if (nrow(longListReport) > 0) {
#     emailLongReport(recipients, subject, clientName, longListReport)
# }
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
