loans <- read.csv('Desktop/original (1)/datasets/Give me some Credit/cs-training.csv')
head(loans)
dim(loans)
str(loans)
names(loans)

table(loans$SeriousDlqin2yrs)

table(loans$NumberOfTime30.59DaysPastDueNotWorse)
table(loans$NumberOfTime60.89DaysPastDueNotWorse)


summary(loans$RevolvingUtilizationOfUnsecuredLines)
percentile <- (quantile(loans$RevolvingUtilizationOfUnsecuredLines,
                            c(0.05,0.15,0.3,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,1.00)))
percentile
library(ggplot2)
plt <- ggplot(loans,aes(y=RevolvingUtilizationOfUnsecuredLines))+geom_boxplot()
plt

loans$RevolvingUtilizationOfUnsecuredLines  <- ifelse((loans$RevolvingUtilizationOfUnsecuredLines)>1,median(loans$RevolvingUtilizationOfUnsecuredLines),
       loans$RevolvingUtilizationOfUnsecuredLines)




summary(loans$MonthlyIncome)
library(ggplot2)
plt <- ggplot(loans,aes(y=MonthlyIncome))+geom_boxplot()
plt

loans$NumberOfTime30.59DaysPastDueNotWorse <- ifelse(loans$NumberOfTime30.59DaysPastDueNotWorse>12,6,
                                                     loans$NumberOfTime30.59DaysPastDueNotWorse)

cross_tab <- table(loans$NumberOfTime30.59DaysPastDueNotWorse,loans$SeriousDlqin2yrs)
cross_tab

#####Kirill's course on data preprocessing
df <- read.csv("Data.csv")
head(df)
X <- df[,-1]
Y <- df[,4]

df$Age <- (ifelse(is.na(df$Age),mean(df$Age,na.rm = T),df$Age))

df$Salary<- ifelse(is.na(df$Salary),mean(df$Salary,na.rm=T),df$Salary)

View(df)

df$Country <- factor(df$Country,levels = c('France','Spain','Germany'),labels = c(1,2,3))
df

df$Purchased <- factor(df$Purchased,levels=c('Yes','No'),labels = c(1,0))
df

install.packages("caTools")
library(caTools)

set.seed(999)

spl <- sample.split(df$Purchased,SplitRatio = 0.8)
spl

training_set <- subset(df,spl=='TRUE')
training_set
test_set <- subset(df,spl=='FALSE')

training_set_1 <- scale(training_set[,2:3])
training_set_1
















