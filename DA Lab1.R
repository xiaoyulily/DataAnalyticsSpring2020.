days<-c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
temp<-c(28,30.5,32,31,29.3,27.9,26.4)
snowed<-c("T","T","F","F","T","T","F")
RPI_Weather_Week<-data.frame(days,temp,snowed)

str(RPI_Weather_Week)
summary(RPI_Weather_Week)
RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,"days"]
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$days
subset(RPI_Weather_Week,subset=snowed==T)
sorted.snowed<-order(RPI_Weather_Week["snowed"])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]
dec.snow<-order(-RPI_Weather_Week$temp)
dec.snow
empty.DataFrame<-data.frame()
v1<-1:10
v1
letters
v2<-letters[1:10]
v2
df<-data.frame(col.name.1=v1,col.name.2=v2)
df
help("data.frame")



EPI_data <- read.csv("C:\\Users\\Xiaoyu Li\\Desktop\\DataAnalyticsLab\\2010EPI_data.csv",skip=1)
View(EPI_data)
attach(EPI_data) 
fix(EPI_data) 
head(EPI_data)
EPI
tf <- is.na(EPI)
tf
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30, 95, 1), prob=TRUE,ylim=c(0,0.06),xlim=c(30,100))
lines(density(EPI,na.rm=TRUE,bw="SJ"))
help("density")
rug(EPI) 
help(rug)
help("density")
help("fix")
help(plot)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
plot(ecdf(EPI), do.points=TRUE, verticals=F) 
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t 
dsn")
qqline(x)

summary(DALY)
hist(DALY,prob=TRUE)
lines(density(DALY,na.rm = T,bw="SJ"))
rug(DALY)
plot(ecdf(DALY),do.points=F,verticals = T)
qqnorm(DALY)
qqline(DALY)

summary(WATER_H)
hist(WATER_H,prob=T)
lines(density(WATER_H,na.rm=T,bw="SJ"))
rug(WATER_H)
plot(ecdf(WATER_H),do.points=F, verticals=T)
qqnorm(WATER_H)
qqline(WATER_H)

x<-rt(250,df=5)
qqnorm(x)
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)

help("distributions")
EPILand<-EPI[!Landlock]
EPILand
Eland <- EPILand[!is.na(EPILand)]
Eland
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
summary(Eland)
hist(Eland,prob=TRUE)
lines(density(Eland,na.rm = T,bw="SJ"))
rug(Eland)
plot(ecdf(Eland),do.points=F,verticals = T)
qqnorm(Eland)
qqline(Eland)

EPI_surfacewater<-EPI[!No_surface_water]
EPI_surfacewater
Esurfacewater<-EPI_surfacewater[!is.na(EPI_surfacewater)]
hist(Esurfacewater)

EPI_Desert<-EPI[!Desert]
EPI_Desert
EDesert<-EPI_Desert[!is.na(EPI_Desert)]
hist(EDesert)

EPI_High_Population_Density<-EPI[!High_Population_Density]
EPI_High_Population_Density
EHigh_Population_Density<-EPI_High_Population_Density[!is.na(EPI_High_Population_Density)]
hist(EHigh_Population_Density)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia 

water_treatment<- read.csv("C:\\Users\\Xiaoyu Li\\Desktop\\DataAnalyticsLab\\water-treatment.csv",skip=1)
head(water_treatment)
summary(water_treatment)
fivenum(water_treatment$X1.5,na.rm=TRUE)
class(water_treatment$X1.5)
Water_treatment=as.numeric(as.character(water_treatment$X1.5))
is.na(Water_treatment)
Water_treatment[is.na(Water_treatment)] <- 0
Water_treatment
stem(Water_treatment)
hist(Water_treatment)
lines(density(Water_treatment,na.rm=TRUE,bw="SJ"))
rug(Water_treatment) 
plot(ecdf(Water_treatment), do.points=FALSE, verticals=TRUE) 
plot(ecdf(Water_treatment), do.points=TRUE, verticals=F) 
par(pty="s")
qqnorm(Water_treatment)
qqline(Water_treatment)
