getwd()
install.packages("corrgram")
install.packages("corrplot")
library(corrgram)
library(corrplot)
install.packages("caTools")
library(caTools)
install.packages("ggthemes")


#Bike Sharing demand

bikes <- read.csv("original.csv")
head(bikes)
str(bikes)


#Drop variables which are not needed
bikes <- subset(bikes,select = -c(1,2,15,16))
head(bikes)

#check for missing values
sum(is.null(bikes))

#Distribution of Continuous variables
library(ggplot2)
library(ggthemes)
plot1 <- ggplot(bikes,aes(x = temp)) +geom_histogram(fill='skyblue')
print(plot1)

plot2 <- ggplot(bikes,aes(x = atemp)) +geom_histogram(fill='green')
print(plot2)

plot3 <- ggplot(bikes,aes(x = humidity)) +geom_histogram(fill='orange')
print(plot3)

plot4 <- ggplot(bikes,aes(x = windspeed)) +geom_histogram(fill='yellow')
print(plot4)

plot5 <- ggplot(bikes,aes(x = demand)) +geom_histogram(fill='red')
print(plot5)


#Correlation with demand

plot_1 <- ggplot(bikes,aes(x=temp,y=demand)) + geom_point(color='blue') + theme_clean()
print(plot_1)

plot_2 <- ggplot(bikes,aes(x=atemp,y=demand)) + geom_point(color='grey') + theme_clean()
print(plot_2)

plot_3 <- ggplot(bikes,aes(x=humidity,y=demand)) + geom_point(color='black') + theme_clean()
print(plot_3)

plot_4 <- ggplot(bikes,aes(x=windspeed,y=demand)) + geom_point(color='green') + theme_clean()
print(plot_4)

#Correlation

vars <- c('temp', 'atemp', 'humidity', 'windspeed', 'demand')
corrgram(bikes[vars], lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)
bikes <- subset(bikes,select = c(-10))
head(bikes)

#Average demand per discrete variable

library(dplyr)
cat_avg <- bikes %>% group_by(season) %>% summarise(mean_demand=mean(demand))

plt1 <- ggplot(cat_avg,aes(x=season,y=mean_demand)) + geom_point()
plt1


cat_avg <- bikes %>% group_by(year) %>% summarise(mean_demand=mean(demand))
plt2 <- ggplot(cat_avg,aes(x=year,y=mean_demand)) + geom_point()
plt2




cat_avg <- bikes %>% group_by(month) %>% summarise(mean_demand=mean(demand))
plt3 <- ggplot(cat_avg,aes(x=month,y=mean_demand)) + geom_point()
plt3


cat_avg <- bikes %>% group_by(hour) %>% summarise(mean_demand=mean(demand))
plt4 <- ggplot(cat_avg,aes(x=hour,y=mean_demand)) + geom_point()
plt4


cat_avg <- bikes %>% group_by(holiday) %>% summarise(mean_demand=mean(demand))
plt5 <- ggplot(cat_avg,aes(x=holiday,y=mean_demand)) + geom_point()
plt5

cat_avg <- bikes %>% group_by(weekday) %>% summarise(mean_demand=mean(demand))
plt6 <- ggplot(cat_avg,aes(x=weekday,y=mean_demand)) + geom_point()
plt6

cat_avg <- bikes %>% group_by(workingday) %>% summarise(mean_demand=mean(demand))
plt7 <- ggplot(cat_avg,aes(x=workingday,y=mean_demand)) + geom_point()
plt7


cat_avg <- bikes %>% group_by(weather) %>% summarise(mean_demand=mean(demand))
plt8 <- ggplot(cat_avg,aes(x=weather,y=mean_demand)) + geom_point()
plt8


head(bikes_dummies)
bikes <- subset(bikes,select = -c(2,6,7))

#Lagged variables ??

#Dummy variables

install.packages("dummies")
library(dummies)

bikes_1 <- dummy(bikes$season)
head(bikes_1)
bikes_2 <- dummy(bikes$month)
bikes_3 <- dummy(bikes$hour)
bikes_4 <- dummy(bikes$holiday)
bikes_5 <- dummy(bikes$weather)

bikes_dummies <- cbind(bikes,bikes_1,bikes_2,bikes_3,bikes_4,bikes_5)
bikes_dummies <- subset(bikes_dummies,select =-c(1,2,3,4,5) )

bikes_dummies <- subset(bikes_dummies,select =-c(8,20,44,46,50) )

bikes_dummies <- subset(bikes_dummies,select =-c(3) )


#Train test split

## 75% of the sample size
smp_size <- floor(0.75 * nrow(bikes_dummies))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(bikes_dummies)), size = smp_size)

train <- bikes_dummies[train_ind, ]
test <- bikes_dummies[-train_ind, ]

# Model

model <- lm(formula = demand ~.,data=train)
print(summary(model))


#Predictions

prediction <- predict(model,test)
results <- cbind(prediction,test$demand)
head(results)

#Remove -ve values

to_zero <- function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}

results <- as.data.frame(results)
results$prediction <- sapply(results$prediction,to_zero)


#Calculate rmse

mse <- mean((results$prediction-results$V2)^2)

rmse <- (mse^0.5)

print(mse)
print(rmse)

#calculate R squared for test data

SSE <- sum((results$prediction-results$V2)^2)
SST <- sum((results$V2-mean(bikes_dummies$demand))^2)

R2 <- (1-SSE/SST)



