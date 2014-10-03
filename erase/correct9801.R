setwd("~/Downloads/Desktop/Replication/erase")

csur98 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur98.txt", header=TRUE, quote="\"")
temp9801 <- read.table("~/Downloads/Desktop/Replication/erase/erasetemp9801.txt", header=TRUE, quote="\"")

merge5 <- merge(csur98, temp9801, by=c("identif"))
a <- ifelse(is.na(merge5$pofut), 0, 1)
b <- ifelse(is.na(merge5$persist), 10, 20)
c <- a + b
merge5$merge <- ifelse(c == 11, 1, ifelse(c == 20, 2, 3))

merge5$eta98 <- ifelse(is.na(merge5$eta98c), merge5$eta98, merge5$eta98c)

merge5[grep("^eta98c$",colnames(merge5))] <- NULL
merge5[grep("^eta01c$",colnames(merge5))] <- NULL

merge5$persist <- rep(0, times = nrow(merge5))
merge5$perdesi <- rep(0, times = nrow(merge5))

merge5 <- subset(merge5, merge5$merge == 3)
merge5[grep("^merge$",colnames(merge5))] <- NULL

csur98 <- merge5 [order(merge5$identif, merge5$anno),]
write.table(csur98, "./temp98.txt", row.names=FALSE)

csur01 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur01.txt", header=TRUE, quote="\"")
temp9801 <- read.table("~/Downloads/Desktop/Replication/erase/erasetemp9801.txt", header=TRUE, quote="\"")

merge6 <- merge(csur01, temp9801, by=c("identif"))
a <- ifelse(is.na(merge6$pofut), 0, 1)
b <- ifelse(is.na(merge6$persist), 10, 20)
c <- a + b
merge6$merge <- ifelse(c == 11, 1, ifelse(c == 20, 2, 3))

merge6$eta01 <- ifelse(is.na(merge6$eta01c), merge6$eta01, merge6$eta01c)

merge6[grep("^eta98c$",colnames(merge6))] <- NULL
merge6[grep("^eta01c$",colnames(merge6))] <- NULL

merge6 <- subset(merge6, merge6$merge == 3)
merge6[grep("^merge$",colnames(merge6))] <- NULL

csur01 <- merge6 [order(merge6$identif, merge6$anno),]
write.table(csur01, "./temp01.txt", row.names=FALSE)