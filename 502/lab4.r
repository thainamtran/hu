download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")
head(bdims)
mdims <- subset(bdims, sex == 1)
fdims <- subset(bdims, sex == 0)
fhgtmean <- mean(fdims$hgt)
fhgtsd   <- sd(fdims$hgt)

hist(fdims$hgt, probability = TRUE)
x <- 140:190
y <- dnorm(x = x, mean = fhgtmean, sd = fhgtsd)
lines(x = x, y = y, col = "blue")

qqnorm(fdims$hgt)
qqline(fdims$hgt)

str(fdims)
hist(fdims$elb.di, probability = TRUE)

qqnorm(fdims$age)
qqline(fdims$age)

qqnorm(fdims$elb.di)
qqline(fdims$elb.di)
fdims[order(fdims$age),"age"]
fdims[order(fdims$elb.di),"elb.di"]


qqnorm(fdims$che.de)
qqline(fdims$che.de)

qplot(sample = kne.di, data = fdims, stat = "qq")


qqnorm(fdims$kne.di)
qqline(fdims$kne.di)

fknemean <- mean(fdims$kne.di)
fknesd <- sd(fdims$kne.di)
hist(fdims$kne.di,breaks=40, probability = TRUE, main="Histogram of Female Knee Diameter",xlab = "Female Knee Diameter")
x <- 12:26
y <- dnorm(x = x, mean = fknemean, sd = fknesd)
lines(x = x, y = y, col = "blue")

