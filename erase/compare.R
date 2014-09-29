library("foreign")
erasecsur01 <- read.table("~/Downloads/Desktop/Replication/erase/erasecsur01.txt", header=TRUE, quote="\"")
csur01 <- read.dta("~/Downloads/Desktop/compare/csur01.dta")

names1 <- names(csur98)
names2 <- names(erasecsur98)
names1
names2
identical(names1,names2)

maxs <- rep(0, times = (length(csur98)- 44 + 1))
mins <- rep(0, times = (length(csur98)- 44 + 1))
nas <- rep(0, times = (length(csur98)- 44 + 1))

for (i in 44:length(csur98)){

        maxs[(i-43)] <- max(round((csur98[,i] - erasecsur98[,i]), digits = 4), na.rm = TRUE)
        mins[(i-43)] <- min(round((csur98[,i] - erasecsur98[,i]), digits = 4), na.rm = TRUE)
        nas[(i-43)] <- (sum(is.na(csur98[,i])) - sum(is.na(erasecsur98[,i])))
}

all <- data.frame(cbind(maxs, mins, nas))
name <- names(csur98[,44:length(csur98)])
row.names(all) <- name
View(all)

s <- rep(9, times = nrow(all))
for (i in 1:nrow(all)){
        
        s[i] <- round(rowSums(all[i,], na.rm = FALSE), digits = 1)
}
select <- data.frame(cbind(name, s))
select <- subset(select, select$s != 0)
View(select)
select$name

# Studying differences - 1992
library(lubridate)
summary(as.factor(csur01$duofut))
summary(as.factor(erasecsur01$duofut))
a <- subset(erasecsur92, identif==11636 & year(anno) == 1991)
b <- subset(csur92, identif==11636 & anno == 1991)
a$duofut
b$duofut
a$pofut-a$ofut
b$pofut-b$ofut
