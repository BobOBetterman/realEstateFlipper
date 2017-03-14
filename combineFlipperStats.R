library(data.table)

setwd("C:/cygwin64/home/hill/TFO/realEstateFlipper/allData")

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