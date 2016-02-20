# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0

ChenxiZhangAssignment4 <- list(
  firstName = "Chenxi",
  lastName  = "Zhang",
  email     = "czhang62@ucsc.edu",
  studentID = 1505034
)
print(ChenxiZhangAssignment4)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1
install.packages("dplyr")
require(foreign)
require(dplyr)
library(foreign)
library(dplyr)

flights <- read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv",
                    stringsAsFactors = F)

airports <- read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv",
                    stringsAsFactors = F)

planes <- read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv",
                   stringsAsFactors = F)

weather <- read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv",
                    stringsAsFactors = F)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 2
flights$date <- as.Date(flights$date, "%Y-%m-%d %H:%M:%S")
class(flights$date) 

weather$date <- as.Date(weather$date, "%Y-%m-%d")
class(weather$date) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 3
#a
flights2a <- flights %>%
  dplyr::filter(
    dest == "SFO" | dest == "OAK"
  )

print(nrow(flights2a))
print("The number of observations is 3508.")

#b
flights2b <- flights %>%
  dplyr::filter(
    dep_delay >= 60 | arr_delay >= 60
  )

print(nrow(flights2b))
print("The number of observations is 11920.")

#c
flights2c <- flights %>%
  dplyr::filter(
    (arr_delay > 2*as.numeric(dep_delay)) & (dep_delay>0)
  )

print(nrow(flights2c))
print("The number of observations is 13424.")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 4
####1
flights3a <- flights %>%
  select(arr_delay,dep_delay)

####2
flights3b <- flights %>%
  select(contains("delay"))

####3
flights3c <- flights %>%
  select(ends_with("delay"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 5
###a
print(
  arrange(flights,-dep_delay)%>%
    head(5)
)

###b
print(flights5b<-flights%>%
        mutate(catup=dep_delay-arr_delay)%>%
        arrange(-catup)%>%
        select(time,plane,catup)%>%
        head(5)
)
print("  time  plane catup
       1   66 N12157    69
       2  201 N73406    57
       3  234 N37437    54
       4  200 N74856    53
       5  147 N814SK    53")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 6
flights <- mutate(flights,speed=dist/time)
View(flights$speed)

flights <- mutate(flights, delta=dep_delay-arr_delay)
View(flights$delta)

##a
print(
  flights%>%
    arrange(speed)%>%
    select(time,plane,speed)%>%
    head(5)
)
print("time  plane    speed
     1   85 N76288 1.647059
     2   79 N17229 1.772152
     3   77 N14639 1.818182
     4   77 N705SW 1.922078
     5   92 N16646 2.076087.")
##b
print(
  flights%>%
    arrange(delta)%>%
    select(time,plane,delta)%>%
    head(5)
)

print("  time  plane delta
       1  105 N14998  -153
       2  107 N417US  -144
       3  192 N510SW  -143
       4   54 N27722  -143
       5  109 N14558  -142 .")

##c
print(
  flights%>%
    arrange(arr_delay)%>%
    select(time,plane,arr_delay)%>%
    head(5)
)

print(" time  plane arr_delay
      1   66 N12157       -70
      2  147 N814SK       -57
      3  171 N767SK       -56
      4  172 N783SK       -56
      5  163 N713SK       -55 .")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 7
####a
flights7a <- flights %>%
  group_by(carrier) %>%               
  summarise(
    cancel = sum(cancelled),
    total = n(),
    ratio = cancel/total, 
    min_d = min(delta, na.rm = T),
    quarter1_d = quantile(delta, .25, na.rm = T),
    quarter2_d = quantile(delta, .75, na.rm = T),
    median_d = median(delta,na.rm = T),
    mean_d = mean(delta, na.rm = T),
    quantile3_d = quantile(delta, .90, na.rm = T),
    max_d = max(delta, na.rm = T)
  )

####b
day_delay1 <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(flights,!is.na(dep_delay)), date
    ),
    delay = mean(dep_delay),n = n()
  ),
  n > 10
)


day_delay <- flights %>%
  dplyr::filter(!is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(delay = mean(dep_delay),
            n = n()
  )%>%
  filter(n>10)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 8
print(
  mutate(
    day_delay, differ = delay-lag(delay)
  )%>%
    arrange(
      -differ
    )%>%
    head(5)
)

print("1 2011-10-09
       2 2011-06-22
       3 2011-12-31
       4 2011-05-12
       5 2011-03-03.")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 9
####
dest_delay<-flights %>% 
  group_by(dest) %>%
  summarise (
    mean_ad = mean(arr_delay, na.rm = T),
    number=n()
  )
airports<-select(airports,
                 dest = iata, 
                 name = airport , 
                 city,state, lat, long
)

####a
df.9a<- dest_delay%>% 
  left_join(airports,by="dest")

print(df.9a%>%
        select(city,state,mean_ad)%>%
        head(5)
)
print(nrow(df.9a))

####b
df.9b<- dest_delay%>% 
  inner_join(airports,by="dest")

print(df.9b%>%
        select(city,state,mean_ad)%>%
        head(5)
)
print(nrow(df.9b))
##NO,in #a,it is 116,at here,it is 114.

#####c
df.9c<- dest_delay%>% 
  right_join(airports,by="dest")

print(nrow(df.9c))
print("There are 3376 observations in this new table.")
print("There are some NAs appear in the arr_delay,because the datas can not match..")

####d
df.9d <- dest_delay%>% 
  full_join(airports,by="dest")

print(nrow(df.9d))
print("There are 3378 observations in this new table.")
print("There is no NA appear in the arr_delay.")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 10
hourly_delay<- filter(flights,!is.na(dep_delay))%>%
  group_by(date,hour)%>%
  summarize(
    delay = mean(dep_delay),
    n = n()
  )%>%
  filter(n>10)

df.10<- hourly_delay%>% 
  inner_join(weather)

print(df.10%>%
        group_by(conditions)%>%
        summarize(
          max_dl=max(delay,na.rm=T)
        )
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 11
install.packages("tidyr")
require(tidyr)
##a
df <- data.frame(
  treatment = c("a", "b"), 
  subject1 = c(3, 4), 
  subject2 = c(5, 6)
)

df.11a <- df %>% 
  gather(demo,value,
         ...= -treatment) %>%
  rename(subject=demo)%>%
  select(subject,treatment,value)

df.11a[1,"subject"]<- 1
df.11a[2,"subject"]<- 1
df.11a[3,"subject"]<- 2
df.11a[4,"subject"]<- 2
df.11a

##b
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)

df.11b <-df %>%
  arrange(treatment,subject)%>%
  spread(
    key = subject,
    value = value
  ) %>%
  rename(subject1=`1`,subject2=`2`)

##c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df.11c <- df %>%
  separate(
    demo, 
    c("sex", "age", "state"),
    "_"
  )

##d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)

df11.d <- df %>%
  unite(
    col = demo, 
    ... = sex, age, city,
    sep = "."
  )
df11.d[4,2] = "<NA>"