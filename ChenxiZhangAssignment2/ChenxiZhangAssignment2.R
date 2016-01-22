#' ---
# title: Assignment 2 
# author: "Chenxi Zhang"
# date: "Jan.21 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_2.pdf
# ---


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0

ChenxiZhangAssignment2 <- list(
  firstName = "Chenxi",
  lastName  = "Zhang",
  email     = "czhang62@ucsc.edu",
  studentID = 1505034
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1

# 
library(foreign)
diamonds<-get(
  load(file=url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"))
)
ChenxiZhangAssignment2$s1a <- nrow(diamonds)
ChenxiZhangAssignment2$s1b <- ncol(diamonds)
ChenxiZhangAssignment2$s1c <- names(diamonds)
ChenxiZhangAssignment2$s1d <- summary(diamonds$price)
print(ChenxiZhangAssignment2$s1d)

print("a.There are 7 observations.
       b.There are 4 columns.
       c.The header names are:carat, cut, clarity and price.
       d.The summary of the prices are: Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
                                        420      450     600     650     825     980       1 ")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 2
df<- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
  sep = "\t",
  header = T
)

ChenxiZhangAssignment2$s2a <- nrow(df)
ChenxiZhangAssignment2$s2b <- ncol(df)
ChenxiZhangAssignment2$s2c <- names(df)
ChenxiZhangAssignment2$s2d <- mean(df$weight)
ChenxiZhangAssignment2$s2e <- median(df$weight)
print(ChenxiZhangAssignment2$s2a)
print(ChenxiZhangAssignment2$s2b)
print(ChenxiZhangAssignment2$s2c)
print(ChenxiZhangAssignment2$s2d)
print(ChenxiZhangAssignment2$s2e)
print("a.There are 4785 observations.
       b.There are 9 columns.
       c.The header names are: HHX,FMX, FPX, SEX, BMI, SLEEP, educ, height, weight.
       d.The mean weight of the weight column is 266.2357.
       e.The median weight is 175.")

##create a new colum, making weights greater or equal to 996 = NA
hist(df$weight)
table(df$weight)
df$weight2 <- ifelse(test = df$weight<966,
                       yes = df$weight,
                       no = NA)
###
ChenxiZhangAssignment2$s2f <- mean(df$weight2,na.rm=TRUE)
ChenxiZhangAssignment2$s2g <- median(df$weight2,na.rm=TRUE)

women<-subset(df,SEX==2)
ChenxiZhangAssignment2$s2h<-summary(women$weight2,na.rm=TRUE)
men<-subset(df,SEX==1)
ChenxiZhangAssignment2$s2i<-summary(men$weight2,na.rm=TRUE)
print(ChenxiZhangAssignment2$s2f)
print(ChenxiZhangAssignment2$s2g)
print(ChenxiZhangAssignment2$s2h)
print(ChenxiZhangAssignment2$s2i)
print("f.The new mean weight of this adjusted weight column is 174.0741.
       g.The new median weight of this adjusted weight column is 170.
       h.The summary of women weight is:  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
                                         100.0   130.0   150.0   158.2   178.0   274.0     329 
       i.The summary of men weight is:    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
                                         128.0   169.0   187.0   192.8   212.0   298.0     207 ")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 3
###a.
vec <-c(letters,LETTERS)
ChenxiZhangAssignment2$s3a <- vec[seq(2, length(vec),2)]
print(ChenxiZhangAssignment2$s3a)
###b.
ChenxiZhangAssignment2$s3b <-paste(vec[c(29,8,5)],collapse = "")
print(ChenxiZhangAssignment2$s3b)
print("Che")
###c.
arr<-array(c(letters,LETTERS), dim=c(3,3,3))
print(arr)
ChenxiZhangAssignment2$s3c<-paste(arr[1:3,1,2])
print(ChenxiZhangAssignment2$s3c)
print("j k l")
###d.
ChenxiZhangAssignment2$s3d<-paste(arr[2,2,1:3])
print(ChenxiZhangAssignment2$s3d)
print("e n w")
###e.
ChenxiZhangAssignment2$s3e<-paste(arr[3,1,1],arr[2,3,1],arr[2,2,1],sep="")
print(ChenxiZhangAssignment2$s3e)
print("che")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 4:
library(foreign)
org_example<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

sort(unique(org_example$year))
sort(unique(org_example$month))
sort(unique(org_example$educ))

ChenxiZhangAssignment2$s4<-aggregate(org_example$rw,list(Year=org_example$year,Month=org_example$month,Education=org_example$educ),mean,na.rm=TRUE)
print(ChenxiZhangAssignment2$s4)
dim(ChenxiZhangAssignment2$s4)


save(ChenxiZhangAssignment2,
     file = "ChenxiZhangAssignment2.RData")
######## End ########