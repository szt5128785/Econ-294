# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0
ChenxiZhangAssignment5 <- list(
  firstName = "Chenxi",
  lastName  = "Zhang",
  email     = "czhang62@ucsc.edu",
  studentID = 1505034
)
print(ChenxiZhangAssignment5)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1
#a
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
qa1 <- ggplot(diamonds, aes(x=x*y*z,y=price,color=clarity))
qa1 + geom_point(aes(color+clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()

#b
qb1<-ggplot(diamonds,aes(carat,fill=clarity,..density..))
qb1+geom_histogram()+facet_grid(cut~.)

#c
qc1<-ggplot(diamonds,aes(x=cut,price))
qc1+geom_jitter(alpha=0.1)+geom_violin()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Question 3
library(foreign)
library(dplyr)
df <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
##a##
df1<- df%>%
  group_by(year,month)%>%
  summarise(
    q_1st = quantile(rw, .25, na.rm = T,type=7),
    q_3rd = quantile(rw, .75, na.rm = T,type=7),
    q_1 = quantile(rw, .1, na.rm = T,type=5),
    q_9 = quantile(rw, .9, na.rm = T,type=5),
    median_d = median(rw, na.rm = T)
  ) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )

qa2 <- ggplot(data=df1,aes(x=date))+
  geom_line(aes(y = median_d))+
  geom_ribbon(aes(ymin = q_1st, ymax = q_3rd), fill = "grey50",alpha=0.6)+
  geom_ribbon(aes(ymin = q_1, ymax = q_9), fill = "grey70",alpha=0.4)+ylim(0,50)
qa2

##b##
df2 <- df%>%
  group_by(year,month,educ)%>%
  summarise(median_d = median(rw, na.rm = T)) %>%
  mutate(date = paste(year, month, "01", sep = "-"),date = as.Date(date, format = "%Y-%m-%d"))

qb2 <- ggplot(data=df2,aes(x=date,color=educ))+geom_line(aes(y = median_d))+ylim(5,35)
qb2
