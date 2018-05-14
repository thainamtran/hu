download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
ames
str(ames)

salePrice <- ames$SalePrice
priceSample <- sample(salePrice, 50)
mean(priceSample)

sample_mean50 <- rep(NA, 5000)
for(i in 1:5000){
  sample50 <- sample(salePrice, 50)
  sample_mean50[i] <- mean(sample50)
}
hist(sample_mean50, breaks = 20, main = "Histogram of 5000 sample[50] means", xlab = "sample mean")

mean(sample_mean50)
mean(salePrice)


sample_mean150 <- rep(NA, 5000)
for(i in 1:5000){
  sample150 <- sample(salePrice, 150)
  sample_mean150[i] <- mean(sample150)
}
hist(sample_mean150, breaks = 20, main = "Histogram of 5000 sample[150] means", xlab = "sample mean[150]")

mean(sample_mean150)
mean(salePrice)