#Exercise1:
EPI_data <- read.csv("C:\\Users\\Xiaoyu Li\\Desktop\\DataAnalyticsLab\\2010EPI_data.csv",skip=1)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI); qqline(EPI)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(EPI_data$EPI,EPI_data$DALY)

multivariate<- read.csv("C:\\Users\\Xiaoyu Li\\Desktop\\DataAnalyticsLab\\multivariate.csv",header=T)
head(multivariate)

attach(multivariate)
mm <-lm(Homeowners~Immigrant)
summary(mm)
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
attributes(mm)
mm$coefficients
mm
help("attributes")

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")

qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

barplot(BOD$demand,names.arg=BOD$Time)
table(mtcars$cy1)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()
BOD$Time

hist(mtcars$mpg)
break1<-seq(1,40,5)
hist(mtcars$mpg,breaks=seq(1,40,5))
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4,breaks=seq(1,40,5))
qplot(mpg,data=mtcars,binwidth=6,breaks=seq(1,40,5))
qplot(mpg,data=mtcars,binwidth=6)
help("hist")
hist(mtcars$mpg,breaks=10,bins=5)
hist(mtcars$mpg,bins=6)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=8)
help(qplot)
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)

ToothGrowth$dose
ToothGrowth$len
ToothGrowth$supp
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+ geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+ geom_boxplot()

rm(list=ls())
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)

library(dplyr)
library(nycflights13)
head(flights)
summary(flights)
filter(flights,month == 10, day == 4, carrier =='AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA'))
slice(flights, 1:15)
head(arrange(flights,year,month,day, desc(arr_time)))
head(select(flights, carrier, arr_time))
distinct(select(flights, carrier))
distinct(flights,carrier,year)
head(mutate(flights, MyNewColumn = arr_delay - dep_delay))
summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) 
help("summarise")
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE))
summarise(flights, sum(air_time, na.rm = TRUE))
sample_n(flights, 15) 
sample_n(flights, 30)
sample_frac(flights, 0.5)
df_mtcars <- mtcars
head(df_mtcars)
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg
#Pipe operator:  %>%
results<-filter(df_mtcars,mpg>20)%>%sample_n(10)%>%arrange(desc(mpg))
results
