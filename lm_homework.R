#soham_da
#series8
foo<-function(a,b){
  x<-runif(100,a,b)
  y <- rep(5,100)+ x*.5 + rnorm(100,0,5)
  m<-summary(lm(y~x))
  l<-m$coefficients
  print(l)
}
foo(0,100)
#series9
library(lattice)
library(ggplot2)
setwd("C:/Users/Ril/Desktop/Datas/Soham")
tesla<-read.csv("cigarette.csv", header = TRUE, sep =',')
#qns.2(a)
plot(tesla$age,tesla$pressure)
#plotting the data categorywise
df1<-tesla[which(tesla$smoker=="yes"),]
plot(df1$age,df1$pressure)
df2<-tesla[which(tesla$smoker=="no"),]
plot(df2$age,df2$pressure)
#or
xyplot(pressure~age|smoker,data=tesla)
#qns.2(b)
M<-lm(pressure~age,data=tesla)
x<-cbind(rep(1,215),tesla$age)
beta<-M$coefficients
yhat<-x%*%beta
yhat<-as.vector(yhat)
ggplot(data=tesla, map = aes(age, pressure))+ geom_smooth(method = lm, se = F) +  xlab("Age")+
  ylab("Pressure")+ ggtitle("Linear Models") + geom_point()

#qns.2(c)
M1<-lm(pressure~age+smoker, data=tesla)
beta<-unname(M1$coefficients)
X1<-cbind(x,tesla$smoker=="yes") 
y_hat<-X1%*%beta
g<-data.frame(tesla$age,tesla$pressure,y_hat,yhat)
ggplot(data=g)+geom_line(map=aes(g[,1],g[,4],col="previous fitted line"))+geom_point(map=aes(g[,1],g[,2],col="original"))+geom_line(map = aes(x =g[,1], y = g[,3], col = "Estimated"))+
ggtitle("True vs Fitted") + labs(col = "Legend")+xlab("Age")+ylab("Pressure")

#qns.2(d)
M2<-lm(pressure~age+smoker+age:smoker, data=tesla)
g1<-data.frame(g,M2$fitted.values)
ggplot(data=g1)+geom_point(map=aes(g1[,1],g1[,2],col="original"))+geom_point(map = aes(x =g1[,1], y = g1[,3], col = "Estimated"))+
 geom_point(map=aes(g1[,1],g1[,5],col="current fitting"))+ ggtitle("True vs Fitted") + labs(col = "Legend")+xlab("Age")+ylab("Pressure")

#series 11
#qns 3
setwd("C:/Users/Ril/Desktop/Datas/Soham")
data<-read.csv("airpassengers.csv", header = TRUE, sep =',')
attach(data)
#a)
Model <- lm(AirPassengers ~ year, data = data)
summary(Model)
mon <- as.factor(month)
model2 <- lm(AirPassengers ~ year + mon , data = data)
summary(model2)
plot(Model,1)
plot(model2,1)
plot(year,AirPassengers,data=data)
model3 <- lm(log(AirPassengers)~year+ mon, data = data)
summary(model3)
plot(model3,1)
#plotting lagged plot
x<-model3$residuals
y<-x[-1]
z<-x[-length(x)]
length(y)
plot(z,y)

#qns.4
load("C:/Users/Ril/Desktop/Datas/Soham/pima.RData")
data<- pima[-c(which(pima$bmi==0)),]
View(data)
attach(data)
summary(data)
preg <- as.factor(pregnant)
library(dplyr)
# set up cut-off values 
breaks <- c(0,1,2,3,5,7,18)
# specify interval/bin labels
tags <- c("0","1", "2", "[3-4]", "[5-6]", "[7 or More]")
# bucketing values into bins
preg <- cut(pregnant, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(preg)
data$pregnant<-preg
model<-lm(bmi ~ preg, data = data)
summary(model)
fullmodel <- lm(bmi ~ ., data = data)
summary(fullmodel)

data = read.csv("radiation.csv", header = TRUE, sep =',')
x_mean = sapply(data,mean)
s = cov(data)
n = length(data$V1)
p = 2
cutoff = (n - 1)*p/(n-p)* qf(0.95,p,n-p) 
su = solve(s)
#ellipsoid cf

#simulateneous cf
#x
unname(x_mean[1])-sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p)*s[1,1]/n)
unname(x_mean[1])+sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p)*s[1,1]/n)
#y
unname(x_mean[2])-sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p)*s[2,2]/n)
unname(x_mean[2])+sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p)*s[2,2]/n)

#bonferroni
#x
unname(x_mean[1])-qt(1-.05/(2*p),n-1)*sqrt(s[1,1]/n)
unname(x_mean[1])+qt(1-.05/(2*p),n-1)*sqrt(s[1,1]/n)
#y
unname(x_mean[2])-qt(1-.05/(2*p),n-1)*sqrt(s[2,2]/n)
unname(x_mean[2])+qt(1-.05/(2*p),n-1)*sqrt(s[2,2]/n)

#individual
#x
unname(x_mean[1])-sqrt(p*(n-1)/(n-p)*qt(1-0.05/2,n-1)*s[1,1]/n)
unname(x_mean[1])+sqrt(p*(n-1)/(n-p)*qt(1-0.05/2,n-1)*s[1,1]/n)
#y
unname(x_mean[2])-sqrt(p*(n-1)/(n-p)*qt(1-0.05/2,n-1)*s[2,2]/n)
unname(x_mean[2])+sqrt(p*(n-1)/(n-p)*qt(1-0.05/2,n-1)*s[2,2]/n)