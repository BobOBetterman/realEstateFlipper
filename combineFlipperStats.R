# This short little program takes all the files in the "/allData" folder and combines them into
# one file, "flipperAll.csv". This file is a full database with all the listings contained in it.



library(data.table)

# work computer address
#setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper/allData")
# home computer address
setwd("D:/programming/work/realEstateFlipper/realEstateFlipper/allData")


flip <- list()

#flip1 <- read.csv("flipperStats01.csv")
#flip2 <- read.csv("flipperStats02.csv")

fileNames <- list.files()

for (i in 1:length(list.files())) {
  flip[[i]] <- read.csv(fileNames[i])
}

#flipAll <- rbind(flip1, flip2)

flipAll <- rbindlist(flip)

write.csv(flipAll, "../flipperAll.csv", row.names = FALSE)