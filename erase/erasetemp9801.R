library(lubridate)

csur98 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur98.txt", header=TRUE, quote="\"")
csuro98 <- subset(csur98, year(csur98$anno) == 1997)

temp98 <- csuro98[,c(31, 35, 43, 45, 60)]
temp98 <- temp98[order(temp98$identif),]

csur01 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur01.txt", header=TRUE, quote="\"")
csuro01 <- subset(csur01, year(csur01$anno) ==2000)

temp01 <- csuro01[,c(40, 41, 43, 45, 60)]
temp01 <- temp01[order(temp01$identif),]

merge2 <- merge(temp98, temp01, by = c("identif"), all = TRUE)

merge2$eta98n <- eta01n - 3
merge2$diff <- eta01 - eta98
