library(lubridate)

csur98 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur98.txt", header=TRUE, quote="\"")
csuro98 <- subset(csur98, year(csur98$anno) == 1997)

temp98 <- csuro98[,c(31, 35, 43, 45, 60)]
temp98 <- temp98[order(temp98$identif),]

csur01 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur01.txt", header=TRUE, quote="\"")
csuro01 <- subset(csur01, year(csur01$anno) == 2000)

temp01 <- csuro01[,c(40, 41, 43, 45, 60)]
temp01 <- temp01[order(temp01$identif),]

merge2 <- merge(temp98, temp01, by = c("identif"), all = TRUE)

merge2$eta98n <- merge2$eta01n - 3
merge2$diff <- merge2$eta01 - merge2$eta98

z <- ifelse(merge2$diff == 3 , 1, 0)
merge2$z <- ifelse(is.na(z), 0, z)

y <- ifelse(merge2$eta01 == merge2$eta01n, 0, 1)

merge2$x <- ifelse(!is.na(merge2$eta98n), 0, 1)

w <- ifelse(merge2$eta98n >= 0, 0, 1)
v <- z + y + x + w

merge2$eta98 <- ifelse(w == 0, merge2$eta98n, merge2$eta98)

u <- ifelse(merge2$diff == 3 | is.na(merge2$diff), 1, 0)
t <- ifelse(is.na(merge2$eta98)|is.na(merge2$eta98n), 1, ifelse(merge2$eta98 == merge2$eta98n, 0, 1))
s <- ifelse(!is.na(merge2$eta01n), 0, 1)
r <- ifelse(merge2$eta01n < 0 | is.na(merge2$eta01n), 1, 0)
q <- z + y + x + w

merge2$eta01 <- ifelse(q == 0, merge2$eta01n, merge2$eta01)

#merge2$eta98 <- ifelse(merge2$diff != 3 & merge2$eta01 == merge2$eta01n & !is.na(merge2$eta98n) & merge2$eta98n >= 0, merge2$eta98n, merge2$eta98)
#merge2$eta01 <- ifelse(merge2$diff != 3 & merge2$eta98 == merge2$eta98n & !is.na(merge2$eta01n) & merge2$eta01n >= 0, merge2$eta01n, merge2$eta01)

merge2$eta98 <- ifelse(is.na(merge2$eta98) & merge2$eta98 < 0 & !merge2$eta01, merge2$eta01 - 3, merge2$eta98)
merge2$eta01 <- ifelse(is.na(merge2$eta01) & merge2$eta01 < 0 & !merge2$eta98, merge2$eta98 + 3, merge2$eta01)

##p <- ifelse(is.na(merge2$eta98), 1, ifelse(merge2$eta98 < 0, 2, 3))
##o <- ifelse(is.na(merge2$eta01), 10, ifelse(merge2$eta01 < 0, 20, 30))
##n <- p + o

##merge2$eta98 <- ifelse(n == 21|n == 22|n == 31|n == 32, merge2$eta01 - 3, merge2$eta98)
##merge2$eta01 <- ifelse(n == 12|n == 13|n == 22|n == 23, merge2$eta98 + 3, merge2$eta01)

#merge2$eta98 <- ifelse((merge2$diff !=3|is.na(merge2$diff)) & (merge2$eta01== merge2$eta01n| is.na(merge2$eta01)| is.na(merge2$eta01n)) & !(is.na(merge2$eta98n))& merge2$eta98n>=0, merge2$eta98n, merge2$eta98)
#merge2$eta01 <- ifelse((merge2$diff !=3|is.na(merge2$diff)) & (merge2$eta98== merge2$eta98n|(is.na(merge2$eta98)| is.na(merge2$eta98n))) & !(is.na(merge2$eta01n))& merge2$eta01n>=0, merge2$eta01n, merge2$eta01)

merge2$deta <- merge2$eta01 - merge2$eta98
merge2 <- subset(merge2,(merge2$deta > 1| is.na(merge2$deta))&(merge2$deta < 5| is.na(merge2$deta)))
merge2$eta01 <- ifelse((merge2$deta==2|merge2$deta==4)&!is.na(merge2$deta),merge2$eta98+3,merge2$eta01)

a <- ifelse(is.na(merge2$raz98d), 3, ifelse(merge2$raz98d == 1, 1, 2))
b <- ifelse(is.na(merge2$raz01d), 3, ifelse(merge2$raz01d == 1, 1, 2))
c <- a + b
merge2$persist <- ifelse(c == 2, 1, 0)

d <- ifelse(is.na(merge2$desi98), 3, ifelse(merge2$desi98 == 1, 1, 2))
e <- ifelse(is.na(merge2$desi01), 3, ifelse(merge2$desi01 == 1, 1, 2))
f <- d + e
merge2$perdesi <- ifelse(f == 2, 1, 0)


temp9801 <- merge2[,c(1, 2, 6, 12, 13)]
names(temp9801)[2]="eta98c"
names(temp9801)[3]="eta01c"

temp9801 <- temp9801[order(temp9801$identif),]

write.table(temp9801, "C:/Users/anton_000/Documents/Downloads/Desktop/Replication/erase/erasetemp9801.txt")
