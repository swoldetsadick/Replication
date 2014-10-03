

setwd("~/Downloads/Desktop/Replication/erase")
csur92 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur92.txt", header=TRUE, quote="\"")
temp9295 <- read.table("~/Downloads/Desktop/Replication/erase/erasetemp9295.txt", header=TRUE, quote="\"")


merge3 <- merge(csur92, temp9295, by=c("identif"))
a <- ifelse(is.na(merge3$pofut), 0, 1)
b <- ifelse(is.na(merge3$persist), 10, 20)
c <- a + b
merge3$merge <- ifelse(c == 11, 1, ifelse(c == 20, 2, 3))

merge3 <- subset(merge3, merge3$merge == 3)
merge3$eta92 <- merge3$eta92c
merge3$persist <- rep(0, times = nrow(merge3))

merge3[grep("^merge$",colnames(merge3))] <- NULL
merge3[grep("^eta92c$",colnames(merge3))] <- NULL
merge3[grep("^eta95c$",colnames(merge3))] <- NULL

merge3 <- subset(merge3, (is.na(merge3$eta92)|merge3$eta92 >= 0))
csur92 <- merge3 [order(merge3$identif, merge3$anno),]
write.table(csur92, "./temp92.txt", row.names=FALSE)

csur95 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur95.txt", header=TRUE, quote="\"")
temp9295 <- read.table("~/Downloads/Desktop/Replication/erase/erasetemp9295.txt", header=TRUE, quote="\"")


merge4 <- merge(csur95, temp9295, by=c("identif"))
a <- ifelse(is.na(merge4$pofut), 0, 1)
b <- ifelse(is.na(merge4$persist), 10, 20)
c <- a + b
merge4$merge <- ifelse(c == 11, 1, ifelse(c == 20, 2, 3))

merge4 <- subset(merge4, merge4$merge == 3)
merge4$eta95 <- merge4$eta95c

merge4[grep("^merge$",colnames(merge4))] <- NULL
merge4[grep("^eta92c$",colnames(merge4))] <- NULL
merge4[grep("^eta95c$",colnames(merge4))] <- NULL

merge4 <- subset(merge4, (is.na(merge4$eta95)|merge4$eta95 >= 0))
csur95 <- merge4 [order(merge4$identif, merge4$anno),]
write.table(csur95, "./temp95.txt", row.names=FALSE)

