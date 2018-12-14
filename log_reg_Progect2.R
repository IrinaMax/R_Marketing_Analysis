# Logistic regression\
library(ggplot2)
library(dplyr)
library(Amelia)
df.train <- read.csv('titanic_train.csv')
df.train %>% head %>% str

missmap(df.train, main='Missing Map', col= c('yellow', 'black'), legend = TRUE)
ggplot(df.train, aes(Survived)) +geom_bar()
ggplot(df.train, aes(Pclass)) +geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill=factor(Sex)))
ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, alpha=0.5,fill = 'green')
ggplot(df.train, aes(SibSp)) + geom_bar(aes(fill=SibSp))
ggplot(df.train, aes(Fare)) + geom_histogram(bin=50, fill= 'green', color='black', alpha=0.5)
#ggplot(df.train, aes(Survived,Pclass)) + geom_bar( position = "dodge")

pl_t <- ggplot(df.train, aes(Pclass, Age))
pl_t <- pl_t + geom_boxplot(aes(group =Pclass, fill= factor(Pclass), alpha=0.4))
pl_t + scale_y_continuous(breaks = seq(min(0), max(80), by=2)) 
pl_t


# imputation of Age base on the 
impute_age <- function(age, class) {
  out <- age
  for (i in 1:length(age)) {
    if (is.na(age[i])) {
      if (class[i]==1) {
        out [i] <- 37
      }else if (class[i] ==2){
        out[i] <- 29
      }else {
        out[i] <- 24
      } 
      }else{
        out[i] <- age[i]
      }  
      }
      return(out)
}


fixed.age <- impute_age(df.train$Age, df.train$Pclass) 
 fixed.age
df.train$Age <- fixed.age
# imputation check : must be missing value now
missmap(df.train, main = 'Imputation Check', col = c('yellow', 'black'), legend = F)

# Let's buld the model
# First of all we need to remove some feature we don need to use and make sure that the features are of the correct data type.
df.train <- select(df.train, -PassengerId, -Ticket, -Cabin, - Name)
df.train %>% head
# We also need to set factor columns
str(df.train)
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
# we need add level for Parch
df.train$Parch <- factor(df.train$Parch = c("0", "1","2","3", "4", "5", "6"))
df.train$Parch <- factor(df.train$Parch)
df.train$Parch <- factor(df.train$Parch, levels = c(levels(df.train$Parch), "9"))
df.train$SibSp <- factor(df.train$SibSp)

df.train$Parch %>% levels()

df.train %>% head
str(df.train)

# Now we can train the model
log.model <- glm(Survived ~., family= binomial(link='logit'), data=df.train)
summary(log.model)

# We can see in summary clearly that Sex, Age, and Class are the most significant features. Which makes sense given the women and children first policy.

# We need to prepare the Test set
df.test <- read.csv('titanic_test.csv') 
df.test %>% head
df.test %>% str
fixed.age.test <- impute_age(df.test$Age, df.test$Pclass)
fixed.age.test
df.test$Age <- fixed.age.test
missmap(df.test, main = 'test', col = c('blue','yellow'))
df.test <- select(df.test, -PassengerId, -Ticket, - Cabin, -Name)
df.test %>% head


df.test$Pclass <- factor(df.test$Pclass)
df.test$Parch <- factor(df.test$Parch)
df.test$SibSp <- factor(df.test$SibSp)
df.test %>% str

# Replace missing value with mean (age variable)
library(gam)
#na.fail(dataset)         # Fails if NAs are present
dataset<-na.gam.replace (dataset)        # Replace missing in age with the mean of the non-missings;
# Check
na.fail(dataset)         # Fails if NAs are present
library(Amelia)
missmap(df.test, main= "Missing values vs observed")
sapply(df.test,function(x)sum(is.na(x)))
# or we can just delete it with omit 
df.test <-  na.omit(df.test)
df.test$Fare

fit.prob <- predict(log.model, df.test.clean, type ='response')
fit.prob


# using test set as a split of the train
library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = .7)
final.train <- subset(df.train, split ==TRUE)
final.test <- subset(df.train, split ==FALSE)
final.log <- glm(Survived ~., family = binomial(link='logit'), final.train)
summary(final.log)

fitted.probabilities <- predict(final.log, final.test, type ='response')
fitted.result <- ifelse(fitted.probabilities>0.5,1 , 0)
  fitted.result
missClassError <- mean(fitter.result != final.test$Survived)  
missClassError
accurasy <- (1 - missClassError)
accurasy

# confusion metrix
table(final.test$Survived, fitted.probabilities> 0.5)
-------------------------------------------------------
# working with real Kaggle test set
  
  df.test <- read.csv('titanic_test.csv') 
df.test %>% head
df.test %>% str
fixed.age.test <- impute_age(df.test$Age, df.test$Pclass)
fixed.age.test
df.test$Age <- fixed.age.test

df.test <- select(df.test, -PassengerId, -Ticket, - Cabin, -Name)
df.test %>% head
missmap(df.test, main = 'test', col = c('blue','yellow'))

df.test$Pclass <- factor(df.test$Pclass)
df.test$Parch <- factor(df.test$Parch)
df.test$SibSp <- factor(df.test$SibSp)
df.test %>% str

# Replace missing value with mean (age variable)
library(gam)
#na.fail(dataset)         # Fails if NAs are present
df.test$Fare<-na.gam.replace(df.test$Fare)        # Replace missing in age with the mean of the non-missings;
# Check
na.fail(df.test)         # Fails if NAs are present

#cheking missing value
library(Amelia)
missmap(df.test, main= "Missing values vs observed")
sapply(df.test,function(x)sum(is.na(x)))

# or we can just delete it with omit 
df.test <-  na.omit(df.test)
df.test$Fare
# try to collapsee levels in Parch

library(rockchalk)
levels(df.test$Parch )<-combineLevels(df.test$Parch,levs = c("6", "9"), newLabel = c("6") )
df.test$Parch

fit.prob <- predict(log.model, df.test, type ='response')
fit.prob
 fit.result <- ifelse(fit.prob>0.5, 1, 0)
fit.result
summary(fit.result)
is.na(fit.result)
fit.result <- na.omit(fit.result)
missClassError.test <- mean(fit.result)
accurasy.test <- (1- missClassError.test)
accurasy.test

# Confusion matrix
table(fit.prob<0.5, fit.prob>0.5 )  # we can see 158 people from 418 will be safe, and 259 will die.
# try **

df.train$Parch <- factor(df.train$Parch, levels = c(levels(df.train$Parch), "9"))
  df.train %>% head
str(df.train)
log.model <- glm(Survived ~., family= binomial(link='logit'), data=df.train)
summary(log.model)

summary(df.train$Parch)
fit.prob <- predict(log.model, df.test, type ='response', xlev = df.test$Parch)
fit.prob
