# This function calls all other real estate functions, with specifications for each individual client. It doesn't
# change any existing files itself, although the functions it calls may do so (specifically, flipperUpdateData.R 
# should be called first, as it will update the flipperAll.csv file with the new listings for the week). Unless I 
# decide that the updating functions should be part of another program. In fact, yeah, for now this won't access 
# the updating functions.

library(lubridate)
library(mailR)

# work computer address
setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper")
# home computer address
#setwd("D:/programming/work/realEstateFlipper/realEstateFlipper")

source("realEstateFlipperFunction.R")



# First, update the property list (but only if the new file exists).
if(file.exists("flipperStats.csv")) {
  updateListings()
}


# Have a different function call for each client.

# This is for the first client.

#####################################################################################################################
#####################################################################################################################
# Andrew Hill
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
recipients <- c("andy@tiberiusfo.com", "vchiang@gmail.com")
#####################################################
#####################################################
#####################################################


# Lowest possible build price
lowestBreakEvenBuild <- buildCost

clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
# #####################################################################################################################
# # Sunny
# #####################################################################################################################
# # Client name
# clientName <- "Sunny"
# 
# # The cost/sqft to build a new place
# buildCost <- 300
# 
# # Smallest discount to look at
# lowestDiscount <- -200
# 
# # The cities to search in
# cities <- c("Palo Alto", "Los Altos")
# 
# # Email information
# recipients <- c("andy@tiberiusfo.com", "vchiang@gmail.com")
# #####################################################
# #####################################################
# #####################################################
# 
# 
# # Lowest possible build price
# lowestBreakEvenBuild <- buildCost
# 
# clientFlipperReport(clientName, buildCost, lowestBreakEvenBuild, lowestDiscount, cities, recipients)
# #####################################################################################################################
#####################################################################################################################
