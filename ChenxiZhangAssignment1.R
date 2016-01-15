firstName <- "Chenxi"
lastName  <- "Zhang"
studentID <- "1505034"
print(paste(firstName,lastName,studentID))

#problem 1
library(foreign)
df.dta <- read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <- read.csv("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.table("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))

#problem 2
print("NHIS_2007_dta.dta is 189KB.
       NHIS_2007_CSV.csv is 139KB.
       NHIS_2007_TSV.txt is 139KB.
       NHIS_2007_RData.RData is 46KB.
       NHIS_2007_RData.RData is the smallest.
       Their structrures are different")

#problem 3
df.RData<-NHIS_2007_RData
print(typeof(df.RData))
print(class(df.RData))
print(length(df.RData))
print(dim(df.RData))
print(nrow(df.RData))
print(ncol(df.RData))
summary(df.RData)

print("The type of the df.RData is list
       the class of the df.RData is data.frame
       the length of df.RData is 9
       the dim of df.RData is 4785 & 9
       the nrow of df.RData is 4785 
       the ncol of df.RData is 9")

#problem 4
library(foreign) 
df<- read.dta(file = "http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta")
str(df) 
print("1119754 observations and 30 variables.")
summary(df$rw)
print("min is 1.8, 
       mean is 19.8,
       median is 15.9, 
       max is 354.8
       NA is 521279")

#problem 5
v<-c(1,2,3,4,5,6,7,4,NULL,NA)
print(length(v))
mean(v,na.rm=TRUE)
print(mean(v,na.rm=TRUE))
print("The length is 9 and the mean is 4.")

#problem 6
#a.Create the matrix and call it x.
x<-matrix(1:9, ncol=3, nrow=3, byrow=TRUE)

#b.Find its transpose
t(x) 

#c.Find the eigenvalues and eigenvectors of x.
eigen(x) 

#d.Create the matrix and call it y.
y<-matrix(c(1,2,3,3,2,1,2,3,0),nrow = 3, ncol = 3, byrow = TRUE) #

#e.Find the inverse of y.
solve(y) 

#f.Multiple y by its inverse.
solve(y) %*% y 

#g.This new matrix is called Identity Matrix.

#problem 7
#creat a dataframe
diamonds<-data.frame(carat=c(5,2,0.5,1.5,5,"NA",3), 
                     cut=c("fair","good","very good","good","fair","Ideal","fair"),
                     clarity=c("SI1","I1","VI1","VS1","IF","VVS2","NA"),
                     price=c(850,450,450,NA,750,980,420))
diamonds
#a.mean price:650
mean(diamonds$price,na.rm = T) 

#b.mean price of "fair":673.333
diamonds2<-subset(diamonds,(cut=="fair"))
mean(diamonds2$price)

#c.mean price of "good":450
diamonds3<-subset(diamonds,(cut=="good"))
mean(diamonds3$price,na.rm=T)
#mean price of "very good":450
diamonds3<-subset(diamonds,(cut=="very good"))
mean(diamonds3$price,na.rm=T)
#mean price of "Ideal":980
diamonds3<-subset(diamonds,(cut=="Ideal"))
mean(diamonds3$price,na.rm=T)

#d.median price:NA
median(diamonds[diamonds$carat>2&(diamonds$cut=="Ideal"|diamonds$cut=="very good"),]$price)
