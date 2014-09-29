a <- data.frame(csur95$identif, csur95$anno, csur95$ofut)
b <- data.frame(erasecsur95$identif, erasecsur95$anno, erasecsur95$ofut)
names(a)<- c("identif", "anno", "ofut")
names(b)<- c("identif", "anno", "ofut")
c <- subset(a, is.na(a$ofut) == TRUE)
d <- subset(b, is.na(b$ofut) == TRUE)
c$name <- row.names(c)
d$name <- row.names(d)
library(lubridate)
c <- c[-(2985:2987),]
c$paste <- as.numeric(paste(as.numeric(c$identif), as.numeric(c$anno), sep=""))
d$paste <- as.numeric(paste(as.numeric(d$identif), as.numeric(year(d$anno)), sep=""))

d$diff <- d$paste-c$paste
d$autre <- c$paste
c$p <-

c$p <- paste(c$anno,c$name,sep="")
d$p <- paste(year(d$anno),d$name,sep="")
c$maka <- (c$p %in% d$p)
e <- subset(d, d$maka=="FALSE")
f <- subset(csur95, identif == 11541)

summary(as.factor(csur95$dimm))
summary(as.factor(erasecsur95$dimm))
summary(as.factor(csur95$duofut))
summary(as.factor(erasecsur95$duofut))