library(foreign)
setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication")
data <- read.dta("./datasur.dta")
setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase")
names <- names(data) #storing column names in "names" vector
classes <- rep(0, times=ncol(data)) # creating vector to store classes of data column
for(i in 1:ncol(data)) {classes[i] <- class(data[,i])} # for each column in data finding class storing it in vector "classes"
names #displaying names
classes# displaying classes
suppressWarnings(for(i in 1:14) {data[,i] <- as.integer(data[,i])}) # changing class of other variables to integer
suppressWarnings(for(i in 23:ncol(data)) {data[,i] <- as.integer(data[,i])}) # changing class of other variables to integer
data$anno <- as.Date(paste(as.character(data$anno),paste("01","01",sep="-"),sep="-"),"%Y-%m-%d") # changing class for anno variable to date
data$identif <- as.factor(data$identif) # indentif number is treated as factor
classes <- rep(0, times=ncol(data)) # creating vector to store classes of data column
for(i in 1:ncol(data)) {classes[i] <- class(data[,i])} # for each column in data finding class storing it in vector "classes"
classes # displaying classes
csur92 <- subset(data,ind92==1 & dip91!="." & dip91>0)
csur92$lsize <- ifelse(csur92$sales<= 0 , NA, log(csur92$sales))
csur92$razd <- as.factor(ifelse(csur92$d17_1==3|csur92$d17_2==3|csur92$d17_3==3|csur92$d17_4==3,1,ifelse(csur92$d17_1==9 &csur92$d17_2==9 & csur92$d17_3==9 & csur92$d17_4==9,NA,0)))
csur92$raz92d <- csur92$razd
csur92$duacq <- ifelse(is.na(csur92$acqui),0,ifelse(csur92$acqui==1,1,0))
csur92$dusco<- ifelse(is.na(csur92$scorpo),0,ifelse(csur92$scorpo==1,1,0))
csur92$dupav1 <- ifelse(csur92$pav == 1, 1, 0)
csur92$dupav2 <- ifelse(csur92$pav == 2, 1, 0)
csur92$dupav3 <- ifelse(csur92$pav == 3, 1, 0)
csur92$dupav4 <- ifelse(csur92$pav == 4, 1, 0)
csur92$roa <- (csur92$util/(csur92$aco+csur92$aim))
csur92$ofut <- (csur92$of/(csur92$of+csur92$utiln))
csur92$quoimm <- (csur92$imte/(csur92$aco+csur92$aim))
csur92$lsales <- ifelse(csur92$sales<= 0 , NA, log(csur92$sales))
library(plyr)
common <- data.frame(tapply(csur92$ofut, csur92$anno, quantile, probs=.75, na.rm = TRUE))
a <- data.frame(rep(NA, times = (12 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
common <- data.frame(rbind(common,a))
csur92$pofut <- ifelse(csur92$anno=="1989-01-01",common[1,1],ifelse(csur92$anno=="1990-01-01",common[2,1],ifelse(csur92$anno=="1991-01-01",common[3,1],ifelse(csur92$anno=="1992-01-01",common[4,1],ifelse(csur92$anno=="1993-01-01",common[5,1],ifelse(csur92$anno=="1994-01-01",common[6,1],ifelse(csur92$anno=="1995-01-01",common[7,1],ifelse(csur92$anno=="1996-01-01",common[8,1],ifelse(csur92$anno=="1997-01-01",common[9,1],ifelse(csur92$anno=="1998-01-01",common[10,1],ifelse(csur92$anno=="1999-01-01",common[11,1],common[12,1])))))))))))

common <- data.frame(tapply(csur92$quoimm, csur92$anno, quantile, probs=.25, na.rm = TRUE))
b <- data.frame(rep(NA, times = (12 - nrow(common))))
names(common)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(common, b))
csur92$duofut <- as.factor(ifelse(round(csur92$ofut, digits = 4) > round(csur92$pofut, digits = 4) & round(csur92$ofut, digits = 4) != ".",1,ifelse(round(csur92$ofut, digits = 4)==".",".",0)))

csur92$pquoimm <- ifelse(csur92$anno=="1989-01-01",common[1,1],ifelse(csur92$anno=="1990-01-01",common[2,1],ifelse(csur92$anno=="1991-01-01",common[3,1],ifelse(csur92$anno=="1992-01-01",common[4,1],ifelse(csur92$anno=="1993-01-01",common[5,1],ifelse(csur92$anno=="1994-01-01",common[6,1],ifelse(csur92$anno=="1995-01-01",common[7,1],ifelse(csur92$anno=="1996-01-01",common[8,1],ifelse(csur92$anno=="1997-01-01",common[9,1],ifelse(csur92$anno=="1998-01-01",common[10,1],ifelse(csur92$anno=="1999-01-01",common[11,1],common[12,1])))))))))))
csur92$dimm <- as.factor(ifelse(((round(csur92$quoimm, digits = 4) < round(csur92$pquoimm, digits = 4)) & (round(csur92$pquoimm, digits = 4) != ".")), 1, ifelse(round(csur92$quoimm, digits = 4)==".", ".", 0)))

csur92[grep("acqui",colnames(csur92))]=NULL
csur92[grep("scorpo",colnames(csur92))]=NULL
csur92[grep("^pav$",colnames(csur92))]=NULL
csur92 <- csur92[order(csur92$identif),]
write.table(csur92, "./erasecsur92.txt", row.names=FALSE)
