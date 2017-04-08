# This short little program takes all the files in the "/allData" folder and combines them into
# one file, "flipperFields.csv". This file is a sample database with all the possible fields 
# from MLS.



library(dplyr)

# work computer address
setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper/allData")
# home computer address
#setwd("D:/programming/work/realEstateFlipper/realEstateFlipper/allData")


flip <- list()

fileNames <- list.files()

for (i in 1:length(list.files())) {
  flip[[i]] <- read.csv(fileNames[i])
}

flipAll <- bind_cols(flip)

write.csv(flipAll, "../flipperFields.csv", row.names = FALSE)