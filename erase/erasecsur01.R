library(foreign)
setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication")
data <- read.dta("./datasur.dta")
setwd("C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase")
names <- names(data) #storing column names in "names" vector
classes <- rep(0, times=ncol(data)) # creating vector to store classes of data column
for(i in 1:ncol(data)) {classes[i] <- class(data[,i])} # for each column in data finding class storing it in vector "classes"
names #displaying names
classes# displaying classes

# Borakov ---- Borakov ---- Borakov
#suppressWarnings(for(i in 1:14) {data[,i] <- as.integer(data[,i])}) # changing class of other variables to integer
#suppressWarnings(for(i in 23:ncol(data)) {data[,i] <- as.integer(data[,i])}) # changing class of other variables to integer

data$anno <- as.Date(paste(as.character(data$anno),paste("01","01",sep="-"),sep="-"),"%Y-%m-%d") # changing class for anno variable to date
data$identif <- as.factor(data$identif) # indentif number is treated as factor
classes <- rep(0, times=ncol(data)) # creating vector to store classes of data column
for(i in 1:ncol(data)) {classes[i] <- class(data[,i])} # for each column in data finding class storing it in vector "classes"
classes # displaying classes

csur01 <- subset(data,ind01 == 1 & dip00 != "." & dip00 > 0 & (eta01 >= 0 | is.na(eta01)))

#Dummy Rationing Baseline def.
z <- ifelse(is.na(csur01$desi), 3, ifelse(csur01$desi == 1, 1, 2))
y <- ifelse(is.na(csur01$chie), 30, ifelse(csur01$chie == 1, 10, 20))
x <- ifelse(is.na(csur01$paga), 200, 100)
w <- z + y + x

csur01$razd <- ifelse(w == 233, NA, ifelse(w == 111|w == 211, 1, 0))

csur01$raz01d <- csur01$razd

#Dummies M&A's;
csur01$duacq <- ifelse(is.na(csur01$acqui),0,ifelse(csur01$acqui==1,1,0))
csur01$dusco<- ifelse(is.na(csur01$scorpo),0,ifelse(csur01$scorpo==1,1,0))

#Dummy Industrial Group
csur01$isgru <- ifelse(is.na(csur01$isgru),0,ifelse(csur01$isgru==1,1,0))

#4 Pavitt Sectoral Dummies;
csur01$dupav1 <- ifelse(csur01$pav==1,1,0)
csur01$dupav2 <- ifelse(csur01$pav==2,1,0)
csur01$dupav3 <- ifelse(csur01$pav==3,1,0)
csur01$dupav4 <- ifelse(csur01$pav==4,1,0)

#Defining Balance-Sheet Variables
#Return on Assets
csur01$roa <- csur01$util/(csur01$aco + csur01$aim)
#Financial Expenses/Operating Profits
csur01$ofut <- ifelse(csur01$of/(csur01$of + csur01$utiln) == Inf, NA , csur01$of/(csur01$of + csur01$utiln))
#Share of Tangible Assets over Total Assets
csur01$quoimm <- csur01$imte/(csur01$aco + csur01$aim)
#Log of Sales
csur01$lsales <- ifelse(csur01$sales<= 0 , NA, log(csur01$sales))

#Generating Percentiles for the Dummies Low Coverage - Low Collateral
library(plyr)
common <- data.frame(tapply(csur01$ofut, csur01$anno, quantile, probs=.75, na.rm = TRUE))
a <- data.frame(rep(NA, times = 9))
b <- data.frame(rep(NA, times = (3 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(a, common, b))
csur01$pofut <- ifelse(csur01$anno=="1989-01-01",common[1,1],ifelse(csur01$anno=="1990-01-01",common[2,1],ifelse(csur01$anno=="1991-01-01",common[3,1],ifelse(csur01$anno=="1992-01-01",common[4,1],ifelse(csur01$anno=="1993-01-01",common[5,1],ifelse(csur01$anno=="1994-01-01",common[6,1],ifelse(csur01$anno=="1995-01-01",common[7,1],ifelse(csur01$anno=="1996-01-01",common[8,1],ifelse(csur01$anno=="1997-01-01",common[9,1],ifelse(csur01$anno=="1998-01-01",common[10,1],ifelse(csur01$anno=="1999-01-01",common[11,1],common[12,1])))))))))))

common <- data.frame(tapply(csur01$quoimm, csur01$anno, quantile, probs=.25, na.rm = TRUE))
a <- data.frame(rep(NA, times = 9))
b <- data.frame(rep(NA, times = (3 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(a, common, b))
csur01$duofut <- as.factor(ifelse(round(csur01$ofut, digits = 6) > round(csur01$pofut, digits = 6) & round(csur01$ofut, digits = 6) != ".",1,ifelse(round(csur01$ofut, digits = 6)==".",".",0)))

csur01$pquoimm <- ifelse(csur01$anno=="1989-01-01",common[1,1],ifelse(csur01$anno=="1990-01-01",common[2,1],ifelse(csur01$anno=="1991-01-01",common[3,1],ifelse(csur01$anno=="1992-01-01",common[4,1],ifelse(csur01$anno=="1993-01-01",common[5,1],ifelse(csur01$anno=="1994-01-01",common[6,1],ifelse(csur01$anno=="1995-01-01",common[7,1],ifelse(csur01$anno=="1996-01-01",common[8,1],ifelse(csur01$anno=="1997-01-01",common[9,1],ifelse(csur01$anno=="1998-01-01",common[10,1],ifelse(csur01$anno=="1999-01-01",common[11,1],common[12,1])))))))))))
csur01$dimm <- as.factor(ifelse(((round(csur01$quoimm, digits = 5) < round(csur01$pquoimm, digits = 5)) & (round(csur01$pquoimm, digits = 5) != ".")), 1, ifelse(round(csur01$quoimm, digits = 5)==".", ".", 0)))

a <- ifelse(is.na(csur01$desi), 3,ifelse(csur01$desi == 2, 2, 1))
b <- ifelse(is.na(csur01$chie), 20, 10)
c <- ifelse(is.na(csur01$paga), 200, 100)
d <- a + b + c
csur01$desi <- ifelse(d == 223, NA, ifelse(d == 111|d == 211|d == 121|d == 221, 1, 0))

csur01$desi01 <- csur01$desi

csur01[grep("acqui",colnames(csur01))] <- NULL
csur01[grep("scorpo",colnames(csur01))] <- NULL
csur01[grep("^pav$",colnames(csur01))] <- NULL

csur01 <- csur01[order(csur01$identif),]
write.table(csur01, "./erasecsur01.txt", row.names=FALSE)