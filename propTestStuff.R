propTest <- subset(propListingsActive, profitPercentOfInvestment >= 0.1)

subset(propSoldOneYear, Area.. == propTest$Area..[1] & sell.Price.Num > propTest$predictedSalePrice[1]*0.9 & sell.Price.Num < propTest$predictedSalePrice[1]*1.1 & houseSqFt > propTest$houseSizeSqFt[1]*0.9 & houseSqFt < propTest$houseSizeSqFt[1]*1.1)$MLS.Number