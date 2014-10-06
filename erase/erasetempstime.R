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

csur92 <- read.table("./csur92.txt", header = TRUE)
csur95 <- read.table("./csur95.txt", header = TRUE)
csur98 <- read.table("./csur98.txt", header = TRUE)
csur01 <- read.table("./csur01.txt", header = TRUE)

merge7 <- merge(merge(csur92, csur95, all.x = TRUE, all.y = TRUE), merge(csur98, csur01, all.x = TRUE, all.y = TRUE), all.x = TRUE, all.y = TRUE)
merge7 <- merge7[order(merge7$identif, merge7$anno),]

### Generating lags
library(lubridate)
merge7 <-  within(merge7, { 
        detab <- ave(eta, identif, FUN = function(x) c(NA, diff(x))) 
        roa1 <- ave(roa, identif, FUN = function(x) c(NA, x[-length(x)]))
        danno <- ave(year(merge7$anno), identif, FUN = function(x) c(NA, diff(x))) 
})

#A <- subset(merge7, detab!=1 & !is.na(detab))
#A <- data.frame(cbind(A$identif, as.character(year(A$anno))))
#B <- subset(merge7, danno!=1 & !is.na(danno))
#B <- data.frame(cbind(B$identif, as.character(year(B$anno))))
#C <- subset(merge7, detab!=1 & !is.na(detab)&danno==1)
#C <- data.frame(cbind(C$identif, as.character(year(C$anno))))
#D <- subset(merge7, danno!=1 & !is.na(danno))
#D <- data.frame(cbind(D$identif, as.character(year(D$anno))))

#View(A)
#View(B)
#View(C)
#View(D)

merge7$dip <- ifelse(year(merge7$anno)==1989,merge7$dip89,ifelse(year(merge7$anno)==1990,merge7$dip90,ifelse(year(merge7$anno)==1991,merge7$dip91,ifelse(year(merge7$anno)==1992,merge7$dip92,ifelse(year(merge7$anno)==1993,merge7$dip93,ifelse(year(merge7$anno)==1994,merge7$dip94,ifelse(year(merge7$anno)==1995,merge7$dip95,ifelse(year(merge7$anno)==1996,merge7$dip96,ifelse(year(merge7$anno)==1997,merge7$dip97,ifelse(year(merge7$anno)==1998,merge7$dip98,ifelse(year(merge7$anno)==1999,merge7$dip99,merge7$dip00)))))))))))

merge7$identif <- as.factor(merge7$identif)

# Creating identif LAG by identif
E <- data.frame(as.factor(merge7$identif))
G <- data.frame(rbind(as.character(NA), E))
G <- data.frame(G[-nrow(G),])
H <- data.frame(cbind(E, G))
colnames(H) <- c("nlg", "lg1")
H$rl <- ifelse(H$nlg == H$lg1, as.character(H$nlg), NA)

# Creating identif LEAD by identif
I <- data.frame(rbind(E, as.character(NA)))
I <- data.frame(I[-1,])
J <- data.frame(cbind(E, I))
colnames(J) <- c("nad", "ad1")
J$ra <- ifelse(J$nad == J$ad1, as.character(J$nad), NA)

# identif LAG is called id1 and LEAD id3 (by identif)
merge7$id1 <- H$rl
merge7$id3 <- J$ra

# creating dip1, dip2 and dip3 variables which represent number of firms by identif
# lagged one time, two times and leaded one time respectively
suppressMessages(library(data.table))
merge7 <- data.table(merge7, key = "identif")
merge7[,c("dip1") := list(c(NA, dip[-.N])), by = identif]
merge7[,c("dip2") := list(c(NA, dip1[-.N])), by = identif]

merge7 <- merge7[order(merge7$identif, merge7$anno, decreasing = TRUE)]
merge7[,c("dip3") := list(c(NA, dip[-.N])), by = identif]
merge7 <- merge7[order(merge7$identif, merge7$anno, decreasing = FALSE)]

# Interpolating possible missing values of number of employees in firm
# Missing value is filled by averaging the number of employees in firm
# exactly from one year before and after if they exist for the specific firm
merge7$dip <- ifelse(is.na(merge7$dip) & !is.na(merge7$dip3) & !is.na(merge7$dip1) & merge7$id1 == merge7$id3, (merge7$dip3+merge7$dip1)/2, merge7$dip)

# Growth rate in number of employees (ddip)
merge7$ddip <- ((merge7$dip-merge7$dip1)/merge7$dip1)

# log of number of employees the same year for the specific firm, a year before
# and two years before (resp. ldip, ldip1 and ldip2)
suppressWarnings(merge7$ldip <- ifelse(is.infinite(log(merge7$dip)), NA, log(merge7$dip)))
suppressWarnings(merge7$ldip1 <- ifelse(is.infinite(log(merge7$dip1)), NA, log(merge7$dip1)))
suppressWarnings(merge7$ldip2 <- ifelse(is.infinite(log(merge7$dip2)), NA, log(merge7$dip2)))

merge7$id <- merge7$identif

lg <- function(x)c(NA,x[1:(length(x)-1)])
ld <- function(x)c(NA,x[1:(length(x)+1)])
merge7[, lid := lg(id), by = c("identif")]
merge7[, vid := ld(id), by = c("identif")]

merge7 <- data.frame(merge7)

merge7$id1 <- NULL
merge7$id3 <- NULL
merge7$dip1 <- NULL
merge7$dip2 <- NULL
merge7$dip3 <- NULL

