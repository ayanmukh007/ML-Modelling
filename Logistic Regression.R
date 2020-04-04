#Fiberbits model

df <- read.csv("Fiberbits.csv")
dim(df)
head(df)
summary(df)

library(ggplot2)

plt1 <- ggplot(df,aes(x=Num_complaints))+geom_histogram(fill='skyblue',bins=5)
plt1


log_model <- glm(formula = active_cust ~ ., family = binomial(),data = df)
summary(log_model)

###########Confusion matrix###########

predicted_values<-predict(log_model,type="response")
cat("Predcited Values")
predicted_values[1:10]

cat("Lets convert them to classes using a threshold")
threshold=0.95
threshold

predicted_class<-ifelse(predict(log_model,type="response")>=threshold,1,0)
cat("Predcited Classes")
predicted_class[1:10]


actual_values<-df$active_cust
conf_matrix<-table(predicted_class,actual_values)
cat("Confusion Matrix")
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
cat("Accuracy")
accuracy


#Multicollinearity

install.packages("car")
library(car)

vif(log_model)
# if VIF >5, then multicollinearity exists
#Also if p-value >0.05, drop the variable

#Variable Importance---- mod z-value the more...more imp the variable
install.packages("caret")
library(caret)
varImp(log_model)

#Best Model out of M1, M2, M3,M4---check AIC/BIC (min is best...least info lost)
#M1
install.packages("stats19")
library(stats)

AIC(log_model)
BIC(log_model)

#Lets build M2

log_model_1 <- glm(formula = active_cust ~ months_on_network+Num_complaints+number_plan_changes
                 +relocated+technical_issues_per_month+Speed_test_result, family = binomial(),data = df)
summary(log_model_1)

predicted_values<-predict(log_model_1,type="response")
cat("Predcited Values")
predicted_values[1:10]

cat("Lets convert them to classes using a threshold")
threshold=0.95
threshold

predicted_class<-ifelse(predict(log_model_1,type="response")>=threshold,1,0)
cat("Predcited Classes")
predicted_class[1:10]


actual_values<-df$active_cust
conf_matrix<-table(predicted_class,actual_values)
cat("Confusion Matrix")
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
cat("Accuracy")
accuracy

AIC(log_model_1)
BIC(log_model_1)






#Logistic Regression model Using Titanic dataset

df <- read.csv('titanic_train.csv')
head(df)
str(df)

install.packages("Amelia")
library(Amelia)
missmap(df,col = c('green','yellow'))

#Survival rate

plt <- ggplot(df,aes(x=Survived))+geom_bar()
plt

plt1 <- ggplot(df,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
plt1

plt2 <- ggplot(df,aes(x=Age)) +geom_histogram()
plt2



#Missing value Imputation- replace missing 'age' with avg age of the Pclass

plt3 <- ggplot(df,aes(x=Pclass,y=Age)) +geom_boxplot(aes(group=Pclass,fill=factor(Pclass))) +theme_bw()
plt3

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if(is.na(age[i])){
      
      if(class[i]==1){
        out[i] <- 36
        
      }else if(class[i]==2){
        out[i] <- 28
        
      }else{
        out[i] <- 23
      }
    }else{
      out[i] <- age[i]
    }
  }
  return(out)
}


fixed.age <- impute_age(df$Age,df$Pclass)

df$Age <- fixed.age


#Remove unwanted feautures
library(dplyr)
df <- select(df,-c(1,4,9,11))
head(df)


str(df)

df$Survived<- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$SibSp <- factor(df$SibSp)
df$Parch <- factor(df$Parch)

#Split dataset
library(caTools)
set.seed(100)

split <- sample.split(df$Survived,SplitRatio = 0.8)
df.train <- subset(df,split==TRUE)
df.test <- subset(df,split==FALSE)


logit_model <- glm(formula = Survived ~ .,family = binomial(link = "logit"),data=df.train)
summary(logit_model)

varImp(logit_model)

predict(logit_model,type = 'response')[1:10]

threshold <- 0.8

predict_val <- ifelse(predict(logit_model,type = 'response')>threshold,1,0)

conf_matrix <- table(predict_val,df.train$Survived)
conf_matrix
acc <- sum(conf_matrix[1,1],conf_matrix[2,2])/sum(conf_matrix)
acc
#Test data check

predict(logit_model,df.test,type = 'response')[1:5]
predict_val_test <- ifelse(predict(logit_model,df.test,type = 'response')>threshold,1,0)

conf_matrix_test <- table(predict_val_test,df.test$Survived)
conf_matrix_test

acc_test <- sum(conf_matrix_test[1,1],conf_matrix_test[2,2])/sum(conf_matrix_test)
acc_test

#Project

df <- read.csv('adult_sal.csv')
head(df)
library(dplyr)
summary(select(df,-X))

sum(is.na(df))

summary(df$type_employer)

#Feature Engineering  
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked'| job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

df$type_employer <- sapply(df$type_employer,unemp)
table(df$type_employer)

job_2 <-function(job){
  if(job=='Local-gov'| job=='State-gov'){
    return('SL-job')
  }else if(job=='Self-emp-inc'| job=='Self-emp-not-inc'){
    return('Self-emp')
  }else {
    return(job)
  }
  
}

df$type_employer <- sapply(df$type_employer,job_2)
table(df$type_employer)


group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

df$marital <- sapply(df$marital,group_marital)
table(df$marital)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

df$country <- sapply(df$country,group_country)
table(df$country)

df <- select(df,-X)
str(df)

df[df=='?'] <- NA

sum(is.na(df))
library(Amelia)

missmap(df,col = c('black','yellow'))
df <- na.omit(df)

#Visualization
library(ggplot2)
plt <- ggplot(df,aes(x=age)) +geom_histogram(aes(fill=income),color='black') +theme_bw()
plt


plt2 <- ggplot(df,aes(x=country))+geom_bar(aes(fill=income))+theme_bw()
plt2

str(df) 
df$type_employer<- as.factor(df$type_employer)
df$marital<- as.factor(df$marital)
df$country<- as.factor(df$country)


library(caTools)
set.seed(100)


sample <- sample.split(df,SplitRatio = 0.75)

df.train <- subset(df,sample==TRUE)
df.test <- subset(df,sample==FALSE)

model <- glm(formula = income ~ .,family = binomial(link = "logit"),data=df.train)
summary(model)

predict(model,type = 'response')[1:5]
thres <- 0.5

predictions <- ifelse(predict(model,type = 'response')>=thres,'>50K','<=50K')
conf_mat <- table(df.train$income,predictions)
acc <- sum(conf_mat[1,1],conf_mat[2,2])/sum(conf_mat)
acc

predictions <- ifelse(predict(model,df.test,type = 'response')>=thres,'>50K','<=50K')
conf_mat_test <- table(df.test$income,predictions)

acc_test <- sum(conf_mat_test[1,1],conf_mat_test[2,2])/sum(conf_mat_test)
acc_test


conf_mat
conf_mat_test



























