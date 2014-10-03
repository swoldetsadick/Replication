setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase")

csur92 <- read.table("./temp92.txt", header = TRUE)
csur95 <- read.table("./temp95.txt", header = TRUE)
csur98 <- read.table("./temp98.txt", header = TRUE)
csur01 <- read.table("./temp01.txt", header = TRUE)

### Generating data set

library(lubridate)

csur92$eta <- ifelse(is.na(csur92$eta92), NA, csur92$eta92)
csur92$eta <- ifelse(!is.na(csur92$eta92) & year(csur92$anno) == 1990, csur92$eta - 1, csur92$eta)
csur92$eta <- ifelse(!is.na(csur92$eta92) & year(csur92$anno) == 1989, csur92$eta - 2, csur92$eta)
csur92 <- csur92 [order(csur92$identif, csur92$anno),]

csur95$eta <- ifelse(is.na(csur95$eta95), NA, csur95$eta95)
csur95$eta <- ifelse(!is.na(csur95$eta95) & year(csur95$anno) == 1993, csur95$eta - 1, csur95$eta)
csur95$eta <- ifelse(!is.na(csur95$eta95) & year(csur95$anno) == 1992, csur95$eta - 2, csur95$eta)
csur95 <- csur95 [order(csur95$identif, csur95$anno),]

csur98$eta <- ifelse(is.na(csur98$eta98), NA, csur98$eta98)
csur98$eta <- ifelse(!is.na(csur98$eta98) & year(csur98$anno) == 1996, csur98$eta - 1, csur98$eta)
csur98$eta <- ifelse(!is.na(csur98$eta98) & year(csur98$anno) == 1995, csur98$eta - 2, csur98$eta)
csur98 <- csur98 [order(csur98$identif, csur98$anno),]

csur01$eta <- ifelse(is.na(csur01$eta01), NA, csur01$eta01)
csur01$eta <- ifelse(!is.na(csur01$eta01) & year(csur01$anno) == 1999, csur01$eta - 1, csur01$eta)
csur01$eta <- ifelse(!is.na(csur01$eta01) & year(csur01$anno) == 1998, csur01$eta - 2, csur01$eta)
csur01 <- csur01 [order(csur01$identif, csur01$anno),]

write.table(csur92, "./csur92.txt", row.names=FALSE)
write.table(csur95, "./csur95.txt", row.names=FALSE)
write.table(csur98, "./csur98.txt", row.names=FALSE)
write.table(csur01, "./csur01.txt", row.names=FALSE)
 
merge7 <- merge(merge(csur92, csur95, all.x = TRUE, all.y = TRUE), merge(csur98, csur01, all.x = TRUE, all.y = TRUE), all.x = TRUE, all.y = TRUE)
merge7 <- merge7[order(merge7$identif, merge7$anno),]

### Generating lags
library(lubridate)
merge8 <-  within(merge7, { 
        detab <- ave(eta, identif, FUN = function(x) c(NA, diff(x))) 
        roa1 <- ave(roa, identif, FUN = function(x) c(NA, x[-length(x)]))
        danno <- ave(year(merge7$anno), identif, FUN = function(x) c(NA, diff(x))) 
})

#A <- subset(merge8, detab!=1 & !is.na(detab))
#A <- data.frame(cbind(A$identif, as.character(year(A$anno))))
#B <- subset(merge8, danno!=1 & !is.na(danno))
#B <- data.frame(cbind(B$identif, as.character(year(B$anno))))
#C <- subset(merge8, detab!=1 & !is.na(detab)&danno==1)
#C <- data.frame(cbind(C$identif, as.character(year(C$anno))))
#D <- subset(merge8, danno!=1 & !is.na(danno))
#D <- data.frame(cbind(D$identif, as.character(year(D$anno))))

#View(A)
#View(B)
#View(C)
#View(D)




