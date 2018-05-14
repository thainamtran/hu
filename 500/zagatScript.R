###Additional Excercise
  
library(readr)
zagat <- read_csv("C:/Users/mkorn/Desktop/zagat.CSV")
View(zagat)

str(zagat)

#### to calculate the man of Food variable
m=mean(zagat$Food)
m

####to calculate the std deviation of Food Variable
stddeviation<-sd(zagat$Food)

stddeviation

###summary function
summary(zagat$Food)

###empirical distriction function

p<-ecdf(zagat$Food)

plot(p)

###histogram
FoodHist<-hist(zagat$Food, breaks=seq(8.5,28.5,by=1), main = "Food Scores")
axis(1, at = seq(9,28,1))

###caclulate std deviations from mean
u1<-p(m+stddeviation)
d1<-p(m-stddeviation)
u1da<-u1-d1
u1da

u2<-p(m+2*stddeviation)
d2<-p(m-2*stddeviation)
u2da<-u2-d2
u2da

u3<-p(m+3*stddeviation)
d3<-p(m-3*stddeviation)
u3da<-u3-d3
u3da