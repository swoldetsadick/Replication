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
csur95 <- subset(data,ind95==1 & dip94!="." & dip94>0)
#Size as Proxied by Total Sales;
csur95$lsize <- ifelse(csur95$sales<= 0 , NA, log(csur95$sales))
csur95$razd <- ifelse(csur95$nonotten==1,1,ifelse(csur95$nonotten=="." & csur95$avrebacc==".",NA,0))
csur95$raz95d <- csur95$razd
csur95$duacq <- ifelse(is.na(csur95$acqui),0,ifelse(csur95$acqui==1,1,0))
csur95$dusco<- ifelse(is.na(csur95$scorpo),0,ifelse(csur95$scorpo==1,1,0))
csur95$durg <- ifelse(is.na(csur95$isgru),0,ifelse(csur95$isgru==1,1,0))
#4 Pavitt Sectoral Dummies;
csur95$dupav1 <- ifelse(csur95$pav==1,1,0)
csur95$dupav2 <- ifelse(csur95$pav==2,1,0)
csur95$dupav3 <- ifelse(csur95$pav==3,1,0)
csur95$dupav4 <- ifelse(csur95$pav==4,1,0)

#Defining Balance-Sheet Variables
#Return on Assets
csur95$roa <- csur95$util/(csur95$aco + csur95$aim)
#Financial Expenses/Operating Profits
csur95$ofut <- csur95$of/(csur95$of + csur95$utiln)
#Share of Tangible Assets over Total Assets
csur95$quoimm <- csur95$imte/(csur95$aco + csur95$aim)
#Log of Sales
csur95$lsales <- ifelse(csur95$sales<= 0 , NA, log(csur95$sales))

#Generating Percentiles for the Dummies Low Coverage - Low Collateral
library(plyr)
common <- data.frame(tapply(csur95$ofut, csur95$anno, quantile, probs=.75, na.rm = TRUE))
a <- data.frame(rep(NA, times = (12 - nrow(common))))
names(common)[1] <- "common"
names(a)[1] <- "common"
common <- data.frame(rbind(common,a))
csur95$pofut <- ifelse(csur95$anno=="1989-01-01",common[1,1],ifelse(csur95$anno=="1990-01-01",common[2,1],ifelse(csur95$anno=="1991-01-01",common[3,1],ifelse(csur95$anno=="1992-01-01",common[4,1],ifelse(csur95$anno=="1993-01-01",common[5,1],ifelse(csur95$anno=="1994-01-01",common[6,1],ifelse(csur95$anno=="1995-01-01",common[7,1],ifelse(csur95$anno=="1996-01-01",common[8,1],ifelse(csur95$anno=="1997-01-01",common[9,1],ifelse(csur95$anno=="1998-01-01",common[10,1],ifelse(csur95$anno=="1999-01-01",common[11,1],common[12,1])))))))))))

common <- data.frame(tapply(csur95$quoimm, csur95$anno, quantile, probs=.25, na.rm = TRUE))
b <- data.frame(rep(NA, times = (12 - nrow(common))))
names(common)[1] <- "common"
names(b)[1] <- "common"
common <- data.frame(rbind(common, b))
csur95$pquoimm <- ifelse(csur95$anno=="1989-01-01",common[1,1],ifelse(csur95$anno=="1990-01-01",common[2,1],ifelse(csur95$anno=="1991-01-01",common[3,1],ifelse(csur95$anno=="1992-01-01",common[4,1],ifelse(csur95$anno=="1993-01-01",common[5,1],ifelse(csur95$anno=="1994-01-01",common[6,1],ifelse(csur95$anno=="1995-01-01",common[7,1],ifelse(csur95$anno=="1996-01-01",common[8,1],ifelse(csur95$anno=="1997-01-01",common[9,1],ifelse(csur95$anno=="1998-01-01",common[10,1],ifelse(csur95$anno=="1999-01-01",common[11,1],common[12,1])))))))))))

csur95$duofut <- ifelse(csur95$ofut > csur95$pofut & csur95$ofut != ".",1,ifelse(csur95$ofut==".",".",0))
csur95$dimm <- ifelse(csur95$quoimm < csur95$pquoimm & csur95$pquoimm != ".",1,ifelse(csur95$ofut==".",".",0))

csur95[grep("acqui",colnames(csur95))] <- NULL
csur95[grep("scorpo",colnames(csur95))] <- NULL
csur95[grep("isgru",colnames(csur95))] <- NULL
csur95[grep("^pav$",colnames(csur95))] <- NULL

csur95 <- csur95[order(csur95$identif),]
write.table(csur95, "./erasecsur95.txt", row.names=FALSE)