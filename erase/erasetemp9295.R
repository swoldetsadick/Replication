setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase")

csur92 <- read.table("./erasecsur92.txt", header=T)
csur95 <- read.table("./erasecsur95.txt", header=T)

suppressMessages(library(lubridate))
csuro92 <- subset(csur92, year(csur92$anno) == 1991)
csuro92 <- data.frame(csuro92$identif, csuro92$eta92, csuro92$raz92d)
colnames(csuro92) <- c("identif", "eta92", "raz92d")

csuro95 <- subset(csur95, as.numeric(year(csur95$anno)) == 1994)
csuro95 <- data.frame(csuro95$identif, csuro95$eta95, csuro95$raz95d)
colnames(csuro95) <- c("identif", "eta95", "raz95d")

merge1 <- merge(csuro92, csuro95, by=c("identif"), all=TRUE)

ind1 <- ifelse(is.na(merge1$eta92) & is.na(merge1$raz92d), 1, 2)
ind2 <- ifelse(is.na(merge1$eta95) & is.na(merge1$raz95d), 10, 20)
ind3 <- ind1 + ind2
merge1$merge <- as.integer(ifelse(ind3 == 12, 1, ifelse(ind3 == 21, 2, ifelse(ind3 == 11, NA, 3))))

merge1 <- merge1[order(merge1$identif), ]

#uses age from one survey to compute age.survey

merge1$eta92c <- ifelse((is.na(merge1$eta92)|merge1$eta92<0) & (!is.na(merge1$eta95) ) & merge1$merge == 3, merge1$eta95 - 3, merge1$eta92)
merge1$eta95c <- ifelse((is.na(merge1$eta95)|merge1$eta95<0) & (!is.na(merge1$eta92) ) & merge1$merge == 3, merge1$eta92 + 3, merge1$eta95)    

merge1[grep("^eta92$",colnames(merge1))] <- NULL
merge1[grep("^eta95$",colnames(merge1))] <- NULL
merge1[grep("^merge$",colnames(merge1))] <- NULL

#Drops firms in the balanced sample with large errors in the age field
merge1$deta <- merge1$eta95c - merge1$eta92c
merge1 <- subset(merge1,(merge1$deta > 1 |is.na(merge1$deta)) & (merge1$deta < 5 |is.na(merge1$deta)))
#forces small differences in age to square to 3
merge1$eta95c <- ifelse(is.na(merge1$deta), merge1$eta95c,ifelse(merge1$deta == 2 | merge1$deta == 4, merge1$eta92c + 3, merge1$eta95c))

merge1$persist <- as.factor(ifelse((merge1$raz92d==1&merge1$raz95d==1), 1,ifelse((is.na(merge1$raz95d)&is.na(merge1$raz92d)),0,0)))
temp9295 <- data.frame(merge1$identif, merge1$eta92c, merge1$eta95c, merge1$persist)
colnames(temp9295) <- c("identif", "eta92c", "eta95c", "persist")

temp9295 <- temp9295 [order(temp9295$identif),]
write.table(temp9295, "./erasetemp9295.txt", row.names=FALSE)