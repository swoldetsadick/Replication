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

merge2$eta98n <- merge2$eta01n - 3
merge2$diff <- merge2$eta01 - merge2$eta98

merge2$eta98 <- ifelse((merge2$diff !=3|is.na(merge2$diff)) & (merge2$eta01== merge2$eta01n| is.na(merge2$eta01)| is.na(merge2$eta01n)) & !(is.na(merge2$eta98n))& merge2$eta98n>=0, merge2$eta98n, merge2$eta98)
merge2$eta01 <- ifelse((merge2$diff !=3|is.na(merge2$diff)) & (merge2$eta98== merge2$eta98n|(is.na(merge2$eta98)| is.na(merge2$eta98n))) & !(is.na(merge2$eta01n))& merge2$eta01n>=0, merge2$eta01n, merge2$eta01)

merge2$deta <- merge2$eta01 - merge2$eta98
merge2 <- subset(merge2,(merge2$deta > 1| is.na(merge2$deta))&(merge2$deta < 5| is.na(merge2$deta)))

merge2$eta01 <- ifelse((merge2$deta==2|merge2$deta==4)&!is.na(merge2$deta),merge2$eta98+3,merge2$eta01)
merge2$persist <- ifelse((merge2$raz98d==1& merge2$raz01d==1)&(is.na(merge2$raz98d)&is.na(merge2$raz01d)),1,0)
merge2$perdesi <- ifelse((merge2$desi98==1& merge2$desi01==1)&(is.na(merge2$desi98)&is.na(merge2$desi01)),1,0)


temp9801 <- merge2[,c(1, 2, 6, 12, 13)]
names(temp9801)[2]="eta98c"
names(temp9801)[3]="eta01c"

temp9801 <- temp9801[order(temp9801$identif),]

write.table(temp9801, "C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase/erasetemp9801.txt")
