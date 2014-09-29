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

csur98 <- subset(data,ind98 == 1 & dip97 != "." & dip97 > 0 & (eta98 >= 0 | is.na(eta98)))

#Dummy Rationing Baseline def.;
csur98$razd <- ifelse(csur98$desi==1 & csur98$paga==1,1,ifelse(csur98$desi=="." & csur98$paga=="."& csur98$chie==".",NA,0))
csur98$raz98d <- csur98$razd

#Dummies M&A's;
csur98$duacq <- ifelse(is.na(csur98$acqui),0,ifelse(csur98$acqui==1,1,0))
csur98$dusco<- ifelse(is.na(csur98$scorpo),0,ifelse(csur98$scorpo==1,1,0))

#Dummy Industrial Group
csur98$isgru <- ifelse(is.na(csur98$isgru),0,ifelse(csur98$isgru==1,1,0))

#4 Pavitt Sectoral Dummies;
csur98$dupav1 <- ifelse(csur98$pav==1,1,0)
csur98$dupav2 <- ifelse(csur98$pav==2,1,0)
csur98$dupav3 <- ifelse(csur98$pav==3,1,0)
csur98$dupav4 <- ifelse(csur98$pav==4,1,0)

#Defining Balance-Sheet Variables
#Return on Assets
csur98$roa <- csur98$util/(csur98$aco + csur98$aim)
#Financial Expenses/Operating Profits
csur98$ofut <- ifelse(csur98$of/(csur98$of + csur98$utiln) == Inf, NA , csur98$of/(csur98$of + csur98$utiln))
#Share of Tangible Assets over Total Assets
csur98$quoimm <- csur98$imte/(csur98$aco + csur98$aim)
#Log of Sales
csur98$lsales <- ifelse(csur98$sales<= 0 , NA, log(csur98$sales))

#Generating Percentiles for the Dummies Low Coverage - Low Collateral
library(plyr)
common <- data.frame(tapply(csur98$ofut, csur98$anno, quantile, probs=.75, na.rm = TRUE))
a <- data.frame(rep(NA, times = 6))
b <- data.frame(rep(NA, times = (6 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(a, common, b))
csur98$pofut <- ifelse(csur98$anno=="1989-01-01",common[1,1],ifelse(csur98$anno=="1990-01-01",common[2,1],ifelse(csur98$anno=="1991-01-01",common[3,1],ifelse(csur98$anno=="1992-01-01",common[4,1],ifelse(csur98$anno=="1993-01-01",common[5,1],ifelse(csur98$anno=="1994-01-01",common[6,1],ifelse(csur98$anno=="1995-01-01",common[7,1],ifelse(csur98$anno=="1996-01-01",common[8,1],ifelse(csur98$anno=="1997-01-01",common[9,1],ifelse(csur98$anno=="1998-01-01",common[10,1],ifelse(csur98$anno=="1999-01-01",common[11,1],common[12,1])))))))))))

common <- data.frame(tapply(csur98$quoimm, csur98$anno, quantile, probs=.25, na.rm = TRUE))
a <- data.frame(rep(NA, times = 6))
b <- data.frame(rep(NA, times = (6 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(a, common, b))
csur98$duofut <- as.factor(ifelse(round(csur98$ofut, digits = 5) > round(csur98$pofut, digits = 5) & round(csur98$ofut, digits = 5) != ".",1,ifelse(round(csur98$ofut, digits = 5)==".",".",0)))

csur98$pquoimm <- ifelse(csur98$anno=="1989-01-01",common[1,1],ifelse(csur98$anno=="1990-01-01",common[2,1],ifelse(csur98$anno=="1991-01-01",common[3,1],ifelse(csur98$anno=="1992-01-01",common[4,1],ifelse(csur98$anno=="1993-01-01",common[5,1],ifelse(csur98$anno=="1994-01-01",common[6,1],ifelse(csur98$anno=="1995-01-01",common[7,1],ifelse(csur98$anno=="1996-01-01",common[8,1],ifelse(csur98$anno=="1997-01-01",common[9,1],ifelse(csur98$anno=="1998-01-01",common[10,1],ifelse(csur98$anno=="1999-01-01",common[11,1],common[12,1])))))))))))
csur98$dimm <- as.factor(ifelse(((round(csur98$quoimm, digits = 5) < round(csur98$pquoimm, digits = 5)) & (round(csur98$pquoimm, digits = 5) != ".")), 1, ifelse(round(csur98$quoimm, digits = 5)==".", ".", 0)))

a <- ifelse(is.na(csur98$desi), 3,ifelse(csur98$desi == 2, 2, 1))
b <- ifelse(is.na(csur98$chie), 20, 10)
c <- ifelse(is.na(csur98$paga), 200, 100)
d <- a + b + c
csur98$desi <- ifelse(d == 223, NA, ifelse(d == 111|d == 211|d == 121|d == 221, 1, 0))

csur98$desi98 <- csur98$desi

csur98[grep("acqui",colnames(csur98))] <- NULL
csur98[grep("scorpo",colnames(csur98))] <- NULL
csur98[grep("^pav$",colnames(csur98))] <- NULL

csur98 <- csur98[order(csur98$identif),]
write.table(csur98, "./erasecsur98.txt", row.names=FALSE)