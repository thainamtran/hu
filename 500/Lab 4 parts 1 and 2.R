#####Chapter 4 parts 1 and 2

###importdata
DealerSat <- read_excel("C:/Users/mkorn/Desktop/HarrisburgU/Performance Lawn Equipment Database.xlsx", skip = 2)

###ReviewData
View(DealerSat)

###fix data - last two columns are error
DealerSatiscation2 <- DealerSat[1:23,1:9]
View(DealerSatiscation2)

str(DealerSatiscation2)

##subset data by region

dealsat_NA<-DealerSatiscation2[1:5,]
dealsat_SA<-DealerSatiscation2[6:10,]
dealsat_EU<-DealerSatiscation2[11:15,]
dealsat_PR<-DealerSatiscation2[16:20,]
dealsat_CH<-DealerSatiscation2[21:23,]


###transpose data using t() function only taking in survey data
tdealsat_NA<-t(dealsat_NA[3:8])

###add column names
colnames(tdealsat_NA)<-c("2010","2011","2012","2013","2014")
tdealsat_NA

######Chapter 4 Part 1 - Brute Force Method
tdealerSat1_1<-tdealsat_NA[1,1]*0
tdealerSat2_1<-tdealsat_NA[2,1]*1
tdealerSat3_1<-tdealsat_NA[3,1]*2
tdealerSat4_1<-tdealsat_NA[4,1]*3
tdealerSat5_1<-tdealsat_NA[5,1]*4
tdealerSat6_1<-tdealsat_NA[6,1]*5
sumtdealer<-sum(tdealsat_NA[,1])

###calculate mean : note multiplying by 0 , 1, 2, refers to the survey scale factor
meanNA2010<-(tdealerSat1_1+tdealerSat2_1+tdealerSat3_1+tdealerSat4_1+tdealerSat5_1+tdealerSat6_1)/sumtdealer

meanNA2010

####loop condition function to get all items done quickly.
###however better to insert "0" data for china for years 2010, 2011
newrowchina2010<- c("China",2010,0,0,0,0,0,0,0)
newrowchina2011<- c("China",2011,0,0,0,0,0,0,0)
DealerSatiscation2 <- rbind(DealerSatiscation2,newrowchina2010)
DealerSatiscation2 <- rbind(DealerSatiscation2,newrowchina2011)

###order data by region and by year
DealerSatiscation2 <- DealerSatiscation2[order(DealerSatiscation2$Region,DealerSatiscation2$Year),]

View(DealerSatiscation2)

##check variables - order made my numbers characters - change them back - could use loop function here to but ill show long hand first
str(DealerSatiscation2)

DealerSatiscation2$L0<-as.numeric(DealerSatiscation2$L0)
DealerSatiscation2$L1<-as.numeric(DealerSatiscation2$L1)
DealerSatiscation2$L2<-as.numeric(DealerSatiscation2$L2)
DealerSatiscation2$L3<-as.numeric(DealerSatiscation2$L3)
DealerSatiscation2$L4<-as.numeric(DealerSatiscation2$L4)
DealerSatiscation2$`L5 `<-as.numeric(DealerSatiscation2$`L5 `)
DealerSatiscation2$SampleSize<-as.numeric(DealerSatiscation2$SampleSize)

str(DealerSatiscation2)

###now loop

counter <- 0

for(i in 1:25){
  print(i)
  counter[i] <- ((DealerSatiscation2[i,3]*0)+(DealerSatiscation2[i,4]*1)+(DealerSatiscation2[i,5]*2)+(DealerSatiscation2[i,6]*3)+(DealerSatiscation2[i,7]*4)+(DealerSatiscation2[i,8]*5))/sum(DealerSatiscation2[i,3:8])
  print(counter[i])
}

print(counter)

###convert to matrix and give rows / names columns

MatrixOfMeans<- matrix(counter, 5, byrow=FALSE)

MatrixOfMeans

row.names(MatrixOfMeans) <- paste(2010:2014)
rownames(MatrixOfMeans)


matrixnames<- c("China", "Europe","North America", "Pacific Rim", "South America")
colnames(MatrixOfMeans) <- matrixnames

MatrixOfMeans


###same process for Standard Deviation

m=0
n=0
for (j in 1:25){
  # print(j)
  m[j] <- ((DealerSatiscation2[j,4]*1) + (DealerSatiscation2[j,5]*2) +
             (DealerSatiscation2[j,6]*3) + (DealerSatiscation2[j,7]*4)
           +(DealerSatiscation2[j,8]*5))/sum(DealerSatiscation2[j,3:8])
  n[j] <- sqrt(((DealerSatiscation2[j,3] * (0 - m[j])^2) + 
                  (DealerSatiscation2[j,4] * (1 - m[j])^2) +
                  (DealerSatiscation2[j,5] * (2 - m[j])^2) +
                  (DealerSatiscation2[j,6] * (3 - m[j])^2) +
                  (DealerSatiscation2[j,7] * (4 - m[j])^2) +
                  (DealerSatiscation2[j,8] * (5 - m[j])^2))/
                 (sum(DealerSatiscation2[j,3:8])-1))
  # print(m[j])
  # print(n[j])
}
print(m)
print(n)

###same thing convert to matrix

SDMatrix<-matrix(n, 5, byrow = FALSE)

row.names(SDMatrix) <- paste(2010:2014)
rownames(SDMatrix)

colnames(SDMatrix) <- matrixnames

SDMatrix


####Perform same steps for End-User-Satisfaction
EU <- read_excel("C:/Users/mkorn/Desktop/HarrisburgU/Performance Lawn Equipment Database.xlsx", sheet = "End-User Satisfaction", skip = 2)

EU2<-EU[1:23,1:9]
EU2 <- rbind(EU2,newrowchina2010)
EU2 <- rbind(EU2,newrowchina2011)

str(EU2)
###order data by region and by year
EU2 <- EU2[order(EU2$Region,EU2$Year),]

###order EU2

###convert columns to numeric using loop this time

for(i in 3:9){
   EU2[,i]<-as.numeric(EU2[,i])
}
str(EU2)


###means
counter <- 0

for(i in 1:25){
  print(i)
  counter[i] <- ((EU2[i,3]*0)+(EU2[i,4]*1)+(EU2[i,5]*2)+(EU2[i,6]*3)+(EU2[i,7]*4)+(EU2[i,8]*5))/sum(EU2[i,3:8])
  print(counter[i])
}

print(counter)

MeansEUMatrix<- matrix(counter, 5, byrow=FALSE)

MeansEUMatrix

row.names(MeansEUMatrix) <- paste(2010:2014)
rownames(MeansEUMatrix)


matrixnames<- c("China", "Europe","North America", "Pacific Rim", "South America")
colnames(MeansEUMatrix) <- matrixnames

MeansEUMatrix


###same process for Standard Deviation

m=0
n=0
for (j in 1:25){
  # print(j)
  m[j] <- ((EU2[j,4]*1) + (EU2[j,5]*2) +
             (EU2[j,6]*3) + (EU2[j,7]*4)
           +(EU2[j,8]*5))/sum(EU2[j,3:8])
  n[j] <- sqrt(((EU2[j,3] * (0 - m[j])^2) + 
                  (EU2[j,4] * (1 - m[j])^2) +
                  (EU2[j,5] * (2 - m[j])^2) +
                  (EU2[j,6] * (3 - m[j])^2) +
                  (EU2[j,7] * (4 - m[j])^2) +
                  (EU2[j,8] * (5 - m[j])^2))/
                 (sum(EU2[j,3:8])-1))
  # print(m[j])
  # print(n[j])
}
print(m)
print(n)

###same thing convert to matrix

SDMatrixEU<-matrix(n, 5, byrow = FALSE)

row.names(SDMatrixEU) <- paste(2010:2014)
rownames(SDMatrixEU)

colnames(SDMatrixEU) <- matrixnames

SDMatrixEU



#####Part 2 

###import Data
CS2014 <- read_excel("C:/Users/mkorn/Desktop/HarrisburgU/Performance Lawn Equipment Database.xlsx", sheet = "2014 Customer Survey", skip = 1)

str(CS2014)
CS2014<-CS2014[1:200,]

###replace NA with NorthA
CS2014 <- lapply(CS2014, function(x){
  gsub("NA","NorthA",x)
})

View(CS2014)
##convert back to dataframe
CS2014<-as.data.frame(CS2014)


for(i in 2:5){
  CS2014[,i]<-as.numeric(CS2014[,i])
}
str(CS2014)

###subset data
CS2014NA<-CS2014[1:100,-1]
CS2014SA<-CS2014[101:150,-1]
CS2014EU<-CS2014[151:180,-1]
CS2014PA<-CS2014[181:190,-1]
CS2014CH<-CS2014[191:200,-1]



###describe all data
library(psych)

describe(CS2014NA)
describe(CS2014SA)
describe(CS2014EU)
describe(CS2014PA)
describe(CS2014CH)

install.packages("pastecs")
library(pastecs)


stat.desc(CS2014NA)
stat.desc(CS2014SA)
stat.desc(CS2014EU)
stat.desc(CS2014PA)
stat.desc(CS2014CH)