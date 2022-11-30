library(tidyverse)
library(glmnet)
#setwd
setwd("C:/Users/woods/Desktop/Woods/고려대학교/4학년/1학기/통계학/통계적데이터 과학/final project/data")

#reading data
houseinfo <- read.csv("informations_households.csv")
weather <- read.csv("weather_daily_darksky.csv")

#reading daily energy
setwd("C:/Users/woods/Desktop/Woods/고려대학교/4학년/1학기/통계학/통계적데이터 과학/final project/data/daily_dataset")	#set directory#
energy.d<- list.files(pattern="*.csv") %>%
  map_df(~read_csv(.))
energy.d<-merge(energy.d[,c(1,2,8)], houseinfo, by="LCLid", all.x=T)
str(energy.d)
energy.d<-energy.d[,-7]

#reading hourly energy
energy.h<-read.csv("energy_h.csv")[,-1]
energy.h<-merge(energy.h, houseinfo, by="LCLid", all.x=T)[,-7]
energy.h$date<-as.Date(energy.h$date)

#treating daily weather
weather<-separate(weather, col=time, sep=" ", into=c("date", "Time"))
index<-grep('Time$', colnames(weather))
weather<-weather[,-index]
weather$date<-as.Date(weather$date)

#merging half
halfdata<-merge(energy.h, weather, by="date", all.x=T)
str(halfdata)
halfdata$LCLid<-as.factor(halfdata$LCLid)

#merging daily
dailydata<-merge(energy.d, weather, by.x="day", by.y="date", all.x=T)
str(dailydata)

#check for seasonality
halfdata$month<-as.numeric(substr(halfdata$date,6,7))
halfdata<-transform(halfdata, season=ifelse(month >=3 & month < 6, "spring",
                                            (ifelse(month >=6 & month <9, "summer",
                                                    (ifelse(month>=9 & month < 12, "fall", "winter"))))))


#check for seasonality_daily
dailydata$month<-as.numeric(substr(dailydata$day, 6,7))
dailydata<-transform(dailydata, season=ifelse(month >=3 & month < 6, "spring",
                                              (ifelse(month >=6 & month <9, "summer",
                                                      (ifelse(month>=9 & month < 12, "fall", "winter"))))))
#character to factor
for (i in 1:ncol(halfdata)){
  ifelse(class(halfdata[,i])=="character",halfdata[,i]<-as.factor(halfdata[,i]),halfdata[,i]<-halfdata[,i])
}
#character to factor
for (i in 1:ncol(dailydata)) {
  ifelse(class(dailydata[,i])=="character",dailydata[,i]<-as.factor(dailydata[,i]),dailydata[,i]<-dailydata[,i])
}

#drop variables-icon, summary, month
halfdata<-halfdata[,-c(6, 24, 27)]

#drop variables-icon, summary, month
str(dailydata)
dailydata<-dailydata[,-c(9,24,27)]

#making train data
index2.d<-sample(c(T,F), size=nrow(dailydata), prob=c(0.3, 0.7), replace=T)
train.d<-dailydata[index2.d,]
test.d<-dailydata[!index2.d,]

#making train data
nrow(halfdata)
index2<-sample(c(T,F), size=nrow(halfdata), prob=c(0.3,0.7),replace=T)
sum(index2)
train<-halfdata[index2,]
test<-halfdata[!index2,]
str(train)
#regression model
x<-model.matrix(energy~C(stdorToU)+C(Acorn)+temperatureMax+windBearing+dewPoint+cloudCover+windSpeed+pressure+apparentTemperatureHigh+
                  C(precipType)+visibility+humidity+apparentTemperatureLow+apparentTemperatureMax+uvIndex+temperatureMin+
                  temperatureHigh+apparentTemperatureMin+moonPhase+C(season),train)
index3<-rownames(x)
y<-train[index3,3]
levels(train$season)
#tuning-lasso
set.seed(1)
cv<-cv.glmnet(x,y, alpha=1)
opt.lambda<-cv$lambda.min
lasso<-glmnet(x,y, alpha=1, lambda=opt.lambda)
coef(lasso)

#lasso testing
test.x<-model.matrix(energy~C(stdorToU)+C(Acorn)+temperatureMax+windBearing+dewPoint+cloudCover+windSpeed+pressure+apparentTemperatureHigh+
                       C(precipType)+visibility+humidity+apparentTemperatureLow+apparentTemperatureMax+uvIndex+temperatureMin+
                       temperatureHigh+apparentTemperatureMin+moonPhase+C(season),test)
index4<-rownames(test.x)
test.y<-test[index4,3]
predicts<-lasso %>% predict(., test.x) %>% as.vector()

#regression model.d
x.d<-model.matrix(energy_sum~C(stdorToU)+C(Acorn)+temperatureMax+windBearing+dewPoint+cloudCover+windSpeed+pressure+apparentTemperatureHigh+
                  C(precipType)+visibility+humidity+apparentTemperatureLow+apparentTemperatureMax+uvIndex+temperatureMin+
                  temperatureHigh+apparentTemperatureMin+moonPhase+C(season),train.d)
index3.d<-rownames(x.d)
y.d<-train.d[index3.d, 3]

#lasso for daily data
lasso.d<-cv.glmnet(x.d,y.d, alpha=1)
opt.lambda.d<-lasso.d$lambda.min
lasso.dd<-glmnet(x,y,alpha=1,lambda=opt.lambda.d)
coef(lasso.dd)
