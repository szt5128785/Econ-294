# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0

ChenxiZhangAssignment3 <- list(
  firstName = "Chenxi",
  lastName  = "Zhang",
  email     = "czhang62@ucsc.edu",
  studentID = 1505034
)
print(ChenxiZhangAssignment3)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
class(df.ex)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 2
install.packages("dplyr")
library(dplyr)
df.ex.2a <- df.ex %>%
  dplyr::filter(
    year == 2013 & month ==12
  )
print(nrow(df.ex.2a))

df.ex.2b <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2b))
print("a.the number of observations that remain is 13261.
       b.the number of observations in Summer 2013 is 39657.")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 3
df.ex.3 <- df.ex %>%
  arrange(year,month)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 4
df.ex.4a <- df.ex %>%
  select(year:age)

df.ex.4b <- df.ex %>%
  select(year,month,starts_with("i"))

print(distinct(select(df.ex,state)))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}

nrmlz <- function(x){(x-min(x, na.rm = T)) / (max(x, na.rm = T)-min(x, na.rm = T))}

df.ex.5a <- df.ex %>%
  mutate(rw.stndz=stndz(rw),rw_nrmlz=nrmlz(rw)) 

df.ex.5b <- df.ex %>%
  group_by(year,month) %>%
  mutate(rw.stndz=stndz(rw),rw_nrmlz=nrmlz(rw),count=n())

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 6
df.ex.6<-df.ex %>%
  group_by(year,month,state) %>%
  summarise(min_rw=min(rw,na.rm=T),
            max_rw=max(rw,na.rm=T),
            mean_rw= mean(rw,na.rm=T),
            median_rw=median(rw,na.rm=T),
            rw_1st=quantile(rw,0.25,na.rm=T),
            rw_3rd=quantile(rw,0.75,na.rm=T),
            count=n())
df.ex.6b<-filter(df.ex.6, mean_rw==max(df.ex.6$mean_rw))
print(df.ex.6b)
print("In December 2013, Washington DC has the highest mean real wage = 40.62582 ")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 7
df.ex$state.char<- as.character(df.ex$state)
df.ex.7a<-arrange(df.ex,-desc(year),-desc(month), desc(state.char))


