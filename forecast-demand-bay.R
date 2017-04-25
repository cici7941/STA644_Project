##Data Pre-process
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(magrittr)
library(ROCR)
status <-fread("201508_status_data.csv")
station = fread("201508_station_data.csv")
weather = fread("201508_weather_data.csv")

#fix wrong dockcount for station_id 22
station[station$station_id=="22"] %<>%mutate(dockcount=27)

station_partial <- station %>% select(station_id, dockcount)
agg1 <- merge(status, station_partial, by.x = "station_id", by.y = "station_id")

#create response variable over_demand_hour
agg1 <- agg1 %>% 
  mutate(date = floor_date(ymd_hms(time),"day")) %>%
  mutate(hour_of_day = hour(ymd_hms(time))) %>%
  mutate(over_demand_minute = ifelse(bikes_available<=round(dockcount/10) | docks_available<=round(dockcount/10), 1, 0)) %>% 
  group_by(station_id, date, hour_of_day) %>%
  summarise(over_demand = sum(over_demand_minute)) %>%
  mutate(over_demand_hour = ifelse(over_demand >= 15, 1, 0))

#add season and weekday_or_weekend
agg1 %<>% mutate(day_of_week = wday(date)) %>%
  mutate(weekday_or_weekend = ifelse(day_of_week >1 & day_of_week < 7, "weekday", "weekend")) %>% mutate(month = month(date)) %>% 
  mutate(season = ifelse(month >=9 & month <= 11, "Fall",
                         ifelse(month >= 12 | month <= 2, "Winter",
                                ifelse(month >= 3 & month <= 5, "Spring",
                                       "Summer"))))


names(weather)
weather_s = select(weather, PDT, which(str_detect(names(weather),"Mean")),19:24)
weather_s$city[weather_s$Zip==94107] = "San Francisco"
weather_s$city[weather_s$Zip==94063] = "Redwood City"
weather_s$city[weather_s$Zip==94301] = "Palo Alto"
weather_s$city[weather_s$Zip==94041] = "Mountain View"
weather_s$city[weather_s$Zip==95113] = "San Jose"
#capital letter of Y!
weather_s$date = as.Date(weather_s$PDT, "%m/%d/%Y")
# n = 24
weather = as.data.frame(weather_s) %>%
  arrange(PDT)
pa.na.hu=which(is.na(weather$`Mean Humidity`)&weather$city=="Palo Alto")
weather$`Mean Humidity`[pa.na.hu]=weather$`Mean Humidity`[pa.na.hu-1]
pa.na.dew=which(is.na(weather$`MeanDew PointF`)&weather$city=="Palo Alto")
weather$`MeanDew PointF`[pa.na.dew]=weather$`MeanDew PointF`[pa.na.dew-1]

#recategorize precipitation
weather$Precipitation<-NA
weather[weather$PrecipitationIn==0,]%<>% mutate(Precipitation = "no rain")
weather[weather$PrecipitationIn=="T" | weather$PrecipitationIn<0.4 & weather$PrecipitationIn>0,]%<>% mutate(Precipitation = "light rain")
weather[weather$PrecipitationIn=="T" | weather$PrecipitationIn<0.4 & weather$PrecipitationIn>0,]%<>% mutate(Precipitation = "light rain")
weather[weather$PrecipitationIn>=0.4 & weather$PrecipitationIn<1,]%<>% mutate(Precipitation = "moderate rain")
weather[weather$PrecipitationIn>=1,]%<>% mutate(Precipitation = "heavy rain")
weather$`MeanDew PointF`[is.na(weather$`MeanDew PointF`)] <- weather$`MeanDew PointF`[is.na(weather$`MeanDew PointF`)]
#recategorize fog event
weather$fog<-NA
weather[str_detect(weather$Events, "Fog"),]%<>% mutate(fog = 1)
weather[!str_detect(weather$Events, "Fog"),]%<>% mutate(fog = 0)
weather$`Max Gust SpeedMPH`<-NULL
weather$Events<-NULL
weather$PrecipitationIn<-NULL

#subset station information
station_s = select(station, station_id, name, dockcount, landmark)
station_s$station_type <- NA
station_s %<>% mutate(station_type = ifelse(str_detect(name, "Caltrain") |
                                              str_detect(station_s$name, "Bart") | 
                                              str_detect(station_s$name, "Terminal"), "Transportation", "Other"))
weather_station = merge(weather, station_s, by.x = "city", by.y = "landmark", all.x = TRUE) %>%
  arrange(station_id)

agg_table <- merge(agg1, weather_station, by=c("station_id","date"), all=TRUE)
agg_table %<>% mutate(day_of_week=wday(date))

agg_table$PDT<-NULL
agg_table$Zip<-NULL

agg_table <- agg_table[complete.cases(agg_table),]

#recategorize time_period
aggg<-agg_table %>% mutate(time_period = ifelse(weekday_or_weekend=="weekday",
                                                ifelse(hour_of_day>=21|hour_of_day<=6,"weekday_night",ifelse(hour_of_day>=7 & hour_of_day<=10 | hour_of_day>=16 & hour_of_day <= 20, "weekday_rush", "weekday_day")),ifelse(hour_of_day>=10& hour_of_day<=17,"weekend_day", "weekend_night")))

##Save data
write.table(aggg,"bikeshare.txt")

##Read data
bikeshare_n <- read.table("bikeshare.txt")

##Modeling

library(lme4)
library(glmnet)

#define function to run glm and adjust for over-dispersion
model <- function(formula,dat){
  fit = glm(formula, data = dat,family = binomial(link = logit))
  dis = sum(residuals(fit,"pearson")^2)/fit$df.residual
  return(list(fit,dis,summary(fit),summary(fit,dispersion = dis)))
}

#slipt data into training and testing dataset
n <- nrow(bikeshare_n)
index <- sample(1:n,round(n*1/3))
train <- bikeshare_n[-index, ]
test <- bikeshare_n[index, ]

#no interation between station_type and city
fit1 <- model(over_demand_hour~time_period +season + `MeanDew.PointF`+factor(CloudCover)+Precipitation+`Mean.TemperatureF`+`Mean.VisibilityMiles`+`Mean.Wind.SpeedMPH`+WindDirDegrees+`Mean.Humidity`+fog+`Mean.Sea.Level.PressureIn`+station_type+city, train)

#interation between station_type and city
fit2 <- model(over_demand_hour~time_period +season + `MeanDew.PointF`+factor(CloudCover)+Precipitation+`Mean.TemperatureF`+`Mean.VisibilityMiles`+`Mean.Wind.SpeedMPH`+WindDirDegrees+`Mean.Humidity`+fog+`Mean.Sea.Level.PressureIn`+station_type*city, train)

#station_id as random effects
fit3 <- glmer(over_demand_hour~time_period +season + `MeanDew.PointF`+factor(CloudCover)+Precipitation+`Mean.TemperatureF`+`Mean.VisibilityMiles`+`Mean.Wind.SpeedMPH`+WindDirDegrees+`Mean.Humidity`+fog+`Mean.Sea.Level.PressureIn`+(1|station_id),data=train, family=binomial(link=logit), control = glmerControl(optimizer = "bobyqa"))

#city as random effects
fit4 <- glmer(over_demand_hour~time_period +season + `MeanDew.PointF`+factor(CloudCover)+Precipitation+`Mean.TemperatureF`+`Mean.VisibilityMiles`+`Mean.Wind.SpeedMPH`+WindDirDegrees+`Mean.Humidity`+fog+`Mean.Sea.Level.PressureIn`+station_type+(1|city),data=train,family=binomial(link=logit), control = glmerControl(optimizer = "bobyqa"))

library(glmnet)
f = as.formula(over_demand_hour~time_period +season+city*station_type + `MeanDew.PointF`+CloudCover+Precipitation+`Mean.TemperatureF`+`Mean.VisibilityMiles`+`Mean.Wind.SpeedMPH`+WindDirDegrees+`Mean.Humidity`+fog+`Mean.Sea.Level.PressureIn`+0)
x = model.matrix(f,train)
fit5 <- cv.glmnet(x, train$over_demand_hour,family = "binomial",alpha = 1, type.measure = "auc")
x_test = model.matrix(f,test)

perf = function(cut, mod, y,x)
{
  # yhat=(predict(mod, x,type = "response")>cut)
  if(class(mod)=="list"){
    yhat = (mod[[1]]$fit>cut)
  } else {yhat=(fitted(mod)>cut)}
  # yhat = (predict(mod[[1]], test, type="response")>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( 0.3*2*d[1]^2 + 0.7*2*d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
  return(out)
}

s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000) OUT[i,]=perf(s[i],fit1,train$over_demand_hour,x)
plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
lines(s, OUT[, 4], col = "darkred", lwd = 2)
box()
legend(
  0.25,
  .5,
  col = c(2, "darkgreen", 4, "darkred"),
  lwd = c(2, 2, 2, 2),
  c(
    "Sensitivity",
    "Specificity",
    "Classification Rate",
    "Distance"
  )
)

p <- predict(fit1[[1]],test,type="response")
pr <- prediction(p, test$over_demand_hour)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Predict

yfit=predict(fit1,x_test,type="response")
ypred=rep(NA,10000)
ypred[yfit<=0.2]<-0
ypred[yfit>0.2]<-1
apply(cbind(test$over_demand_hour,ypred),2,sum)

(table(test$over_demand_hour,ypred)[1,1]+table(test$over_demand_hour,ypred)[2,2])/sum(table(test$over_demand_hour,ypred))
