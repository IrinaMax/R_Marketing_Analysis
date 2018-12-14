# Databcamp Marketing Analytics in R: Statistical Modeling
getwd()
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyselect)
# https://campus.datacamp.com/courses/marketing-analytics-in-r-statistical-modeling/modeling-customer-lifetime-value-with-linear-regression?ex=1
# data https://assets.datacamp.com/production/course_6027/datasets/churn_data.csv
# https://assets.datacamp.com/production/course_6027/datasets/salesData.csv
#https://assets.datacamp.com/production/course_6027/datasets/salesDataMon2To4.csv
# https://assets.datacamp.com/production/course_6027/datasets/survivalDataExercise.csv
# https://assets.datacamp.com/production/repositories/1861/datasets/0b0772985a8676c3613e8ac2c6053f5e60a3aebd/defaultData.csv
# https://assets.datacamp.com/production/repositories/1861/datasets/4c612c0c990fccbd16c695043096aafbb66bd266/clvData1.csv
#https://assets.datacamp.com/production/repositories/1861/datasets/29ce8bb869c5af2ec627c2df583781025ab0b4db/clvData2.csv


# Structure of dataset
#getwd()
setwd("~/Documents/STUDY/DATA_SCIENCE/DataCamp_data/")
salesData <- read.csv("https://assets.datacamp.com/production/course_6027/datasets/salesData.csv",  header = T, stringsAsFactors = T)

str(salesData, give.attr = FALSE)

# Classes 'tbl_df', 'tbl' and 'data.frame':	5122 obs. of  14 variables:
#   $ id                   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ nItems               : int  1469 1463 262 293 108 216 174 122 204 308 ...
# $ mostFreqStore        : chr  "Stockton" "Stockton" "Colorado Springs" "Colorado Springs" ...
# $ mostFreqCat          : chr  "Alcohol" "Alcohol" "Shoes" "Bakery" ...
# $ nCats                : int  72 73 55 50 32 41 36 31 41 52 ...
# $ preferredBrand       : chr  "Veina" "Veina" "Bo" "Veina" ...
# $ nBrands              : int  517 482 126 108 79 98 78 62 99 103 ...
# $ nPurch               : int  82 88 56 43 18 35 34 12 26 33 ...
# $ salesLast3Mon        : num  2742 2791 1530 1766 1180 ...
# $ salesThisMon         : num  1284 1243 683 730 553 ...
# $ daysSinceLastPurch   : int  1 1 1 1 12 2 2 4 14 1 ...
# $ meanItemPrice        : num  1.87 1.91 5.84 6.03 10.93 ...
# $ meanShoppingCartValue: num  33.4 31.7 27.3 41.1 65.6 ...
# $ customerDuration     : int  821 657 548 596 603 673 612 517 709 480 ...
 salesData %>% head
# # A tibble: 5,122 x 14
# id nItems mostFreqStore   mostFreqCat nCats preferredBrand nBrands nPurch
# <int>  <int> <chr>           <chr>       <int> <chr>            <int>  <int>
#   1     1   1469 Stockton        Alcohol        72 Veina              517     82
# 2     2   1463 Stockton        Alcohol        73 Veina              482     88
# 3     3    262 Colorado Sprin~ Shoes          55 Bo                 126     56
# 4     4    293 Colorado Sprin~ Bakery         50 Veina              108     43
# 5     5    108 Colorado Sprin~ Beverages      32 Bo                  79     18
# 6     6    216 Boston          Alcohol        41 Bo                  98     35
# 7     7    174 Columbus        Packaged f~    36 Bo                  78     34
# 8     8    122 Seattle         Shoes          31 Bo                  62     12
# 9     9    204 Jersey          Bakery         41 Bo                  99     26
# 10    10    308 Seattle         Alcohol        52 Bo                 103     33
# ... with 5,112 more rows, and 6 more variables: salesLast3Mon <dbl>,
#   salesThisMon <dbl>, daysSinceLastPurch <int>, meanItemPrice <dbl>,
#   meanShoppingCartValue <dbl>, customerDuration <int>
# Visualization of correlations
salesData %>% names
salesData %>% dim
salesData %>% select_if(is.numeric) %>% select(-id) %>%cor()%>% corrplot()


# Frequent stores
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

# Model specification using lm
salesData
salesSimpleModel <- lm(salesThisMon ~salesLast3Mon, 
                       data = salesData)
summery(salesSimpleModel)
# Looking at model summary
# summary(salesSimpleModel)
# Call:
#   lm(formula = salesThisMon ~ salesLast3Mon, data = salesData)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -570.18  -68.26    3.21   72.98  605.58 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   99.690501   6.083886   16.39   <2e-16 ***
#   salesLast3Mon  0.382696   0.004429   86.40   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 117.5 on 5120 degrees of freedom
# Multiple R-squared:  0.5932,	Adjusted R-squared:  0.5931 
# F-statistic:  7465 on 1 and 5120 DF,  p-value: < 2.2e-16
#Since the regression coefficient is greater than 0, there exists a positive relationship between the explanatory variable salesLast3Mon and the dependent variable salesThisMon. It explains almost 60 percent of the variation in the sales of this month.

install.packages('rms')
library (rms)
# Estimating the full  Multilinear model
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel1)

# Estimating new model by removing information on brand
salesModel2 <- lm(salesThisMon ~ . -id - preferredBrand -nBrands,
                  data = salesData)

# Checking variance inflation factors
vif(salesModel2)
summary(salesModel2)

# The effect of the mean item price on the sales this month is statistically significant. A one-unit increase in the mean item price leads to a 0.23 Euro increase in the sales of this month.
library(stats)
AIC(salesModel2)
AIC(salesModel1)

# Summary of data
defaultData <- read.csv("https://assets.datacamp.com/production/repositories/1861/datasets/0b0772985a8676c3613e8ac2c6053f5e60a3aebd/defaultData.csv", sep = ";", header = T, stringsAsFactors = T)

summary(defaultData)

# Look at data structure
str(defaultData) 
defaultData%>% names

# Analyze the balancedness of dependent variable
ggplot(defaultData,aes(x = PaymentDefault)) +
  geom_histogram(stat = "count") 

# Build logistic regression model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, data = defaultData)

# Take a look at the model
summary(logitModelFull)

# Take a look at the odds
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp

library(MASS)
# The old (full) model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, defaultData)
logitModelFull
#Build the new model
logitModelNew <- stepAIC(logitModelFull,trace = 0) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit

#lets look at the goodned of fit :its accurary on the predict nodel and thresholding
#install.packages("SDMTools")
library(SDMTools)
# Make predictions using the full Model
defaultData$predFull <- predict(logitModelFull, type = "response", na.action =na.exclude )
head(defaultData)
defaultData %>% select(PaymentDefault, predFull) %>% tail()

# Construct the in-sample confusion matrix
# Confusion Matrix
# Prediction \ Truth	     negative	            positive
# negative	             true-negative	    false-negative
# positive	             false-positive	    true-positive
confMatrixModelFull <- confusion.matrix(defaultData$PaymentDefault, defaultData$predFull, threshold = 0.5)
confMatrixModelFull

# Calculate the accuracy for the full Model
accuracyFull <- sum(diag(confMatrixModelFull)) / sum(confMatrixModelFull)
accuracyFull

#we can also use Pseudo R2: McFadden, Cox&Shell and Nagalkerke if we used Logistic regression
install.packages("descr")
library(descr)

LogRegR2(logitModelFull)
#Interpretation:
# Reasonable if > 0.2
# Good if  > 0.4
# Very Good if > 0.5

# Calculate the accuracy for 'logitModelNew'
# Make prediction
defaultData$predNew <- predict(logitModelNew, type = "response", na.action = na.exclude)
defaultData %>% head()
# Construct the in-sample confusion matrix
confMatrixModelNew <- confusion.matrix(defaultData$PaymentDefault,defaultData$predNew, threshold = 0.5)
confMatrixModelNew

# Calculate the accuracy...
accuracyNew <- sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew)
accuracyNew

# and compare it to the full model's accuracy
accuracyFull

library(SDMTools)
# Prepare data frame with threshold values and empty payoff column
payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1),
                           payoff = NA) 
payoffMatrix 

# The payoff is calculated according to the specific costs given in the following scenario:  
#  payoff = 250 * true negative ,     - 1000 * false negative


for(i in 1:length(payoffMatrix$threshold)) {
  # Calculate confusion matrix with varying threshold
  confMatrix <- confusion.matrix(defaultData$PaymentDefault,
                                 defaultData$predNew, 
                                 threshold = payoffMatrix$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffMatrix$payoff[i] <- confMatrix[1,1]*250 + confMatrix[1,2]*(-1000)
}
payoffMatrix
#You could see that the optimal threshold is 0.3.--------------  important--------------------

# Out-of-Sample Fit: Training and Test Data
#------------- 1) Divide the dataset in training and test data

# Generating random index for training and test set
# set.seed ensures reproducibility of random components
set.seed(534381)

churnData <- read.csv("https://assets.datacamp.com/production/course_6027/datasets/churn_data.csv")
churnData %>% head
churnData$isTrain <- rbinom(nrow(churnData), 1, 0.66)
train <- subset(churnData, churnData$isTrain == 1)
test <- subset(churnData, churnData$isTrain == 0)
churnData %>% head
churnData %>% str
#   ------------2) Build a model based on training data

# Modeling logitTrainNew
logitTrainNew <- glm( returnCustomer ~ title + newsletter + websiteDesign +
                        paymentMethod + couponDiscount + purchaseValue + throughAffiliate +
                        shippingFees + dvd + blueray + vinyl + videogameDownload +
                        prodOthers + prodRemitted, family = binomial, data = train)

# Out-of-sample prediction for logitTrainNew
test$predNew <- predict(logitTrainNew, type = "response", newdata = test)

##-----------------------Out-of-Sample Accuracy
#calculating the confusion matrix
confMatrixNew <- confusion.matrix(test$returnCustomer, test$predNew, 
                                  threshold = 0.3)
confMatrixNew

#calculating the accuracy 
accuracyNew <- sum(diag(confMatrixNew)) / sum(confMatrixNew)
accuracyNew


# _____________________Cross-Validation: Accuracy
# Calculation of cross-validated accuracy

library(boot)
# Accuracy function with threshold = 0.3
Acc03 <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Accuracy
set.seed(534381)
cv.glm(churnData, logitModelNew, cost = Acc03, K = 6)$delta

##   working with defaultData again---------------------------------------
# Split data in train and test set
set.seed(534381) 
defaultData$isTrain <- rbinom(nrow(defaultData), 1, 0.66)
train <- subset(defaultData, isTrain == 1)
test <- subset(defaultData, isTrain  == 0)

logitTrainNew <- glm(formulaLogit, family = binomial, data = train) # Modeling
test$predNew <- predict(logitTrainNew, type = "response", newdata = test) # Predictions
#test%>%head
# Out-of-sample confusion matrix and accuracy
confMatrixModelNew <- confusion.matrix(test$PaymentDefault, test$predNew, threshold = 0.3) 
sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) # Compare this value to the in-sample accuracy
library(boot)
# Accuracy function
costAcc <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross validated accuracy for logitModelNew
set.seed(534381)
cv.glm(defaultData, logitModelNew, cost = costAcc, K = 6)$delta[1]



# Survival curve analysis by Kaplan Meier------------------------------- lesson

library(survival)
dataSurv <- read.csv("survivalDataExercise.csv")
dataSurv %>% head
# Survival Object I for the head(10)
 cbind(dataSurv %>% select(tenure, churn),
      surv = Surv(dataSurv$tenure, dataSurv$churn)) %>% head(10)
surv
# tenure churn surv
# 1       1     0   1+
# 2      34     0  34+
# 3       2     1   2 
# 4      45     0  45+
# 5       2     1   2 
# 6       8     1   8 
# 7      22     0  22+

fitKM <- survfit(Surv(dataSurv$tenure, dataSurv$churn) ~ 1, 
                 type = "kaplan-meier")
fitKM$surv

#Printing the Survfit Object 
print(fitKM)

# Call: survfit(formula = Surv(dataSurv$tenure, dataSurv$churn) ~ 1, 
#               type = "kaplan-meier")
# n  events  median 0.95LCL 0.95UCL 
# 5311    1869      70      68      72

plot(fitKM)  # plot give us the confidence interval

#Kaplan-Meier with Categorial Covariate
fitKMstr <- survfit(Surv(tenure, churn) ~ Partner, 
                    data = dataSurv) print(fitKMstr)
# Call: survfit(formula = Surv(tenure, churn) ~ Partner, data = dataSurv)
# 
# n events median 0.95LCL 0.95UCL
# Partner=No  2828   1200     45      41      50
# Partner=Yes 2483    669     NA      NA      NA

# Estimation of the survival function can be intependent of any covariates. But 'survfit()" function allow us to easily model it depending on different covariates.We just put Partner there.
# Then, survival curves are estimated according to where or not our customers have a partner.
# Median survival time of customers with NO partner 45, but without partner  NA, it mean it is higher then 72 and lies outside the ofservation period. This happens when only few customers churned withing the time under observation.
# 
# The survival function describes the proportion of observations who are still alive (or, for example, in a customer relationship, the proportion of customers who haven't churned yet), depending on their time under observation.
                                                                              
#The hazard rate describes the risk that an event occurs within a very small period of time (provided that it hasn't occured yet).

# The hazard rate can go up and down. For example, customers could be very likely to churn at the beginning of their customer relationship, then become less likely for some months, and then become more likely again due to a saturation effect.
# The cumulative hazard function describes the cumulative risk until time t.

# Cox PH model with constant covariates_____ Fitting a Survival Model________________________
library(rms)
dataNextOrder <-dataNextOrder
dataNextOrder %>% head
units(dataSurv$tenure) <- "Month"
dd <- datadist(dataSurv)
options(datadist = "dd")

#Fitting a Survival Model
library(rms)
units(dataSurv$tenure) <- "Month"
dd <- datadist(dataSurv)
options(datadist = "dd")
fitCPH1 <- cph(Surv(tenure, churn) ~ gender  +
                 SeniorCitizen + Partner + Dependents + 
                 StreamMov + PaperlessBilling + PayMeth + 
                 MonthlyCharges,
               data = dataSurv, 
               x = TRUE, y = TRUE, surv = TRUE, 
               time.inc = 1)
# Determine distributions of predictor variables
dd <- datadist(dataNextOrder)
options(datadist = "dd")
head(dd)
# Compute Cox PH Model and print results
fitCPH <- cph(Surv(daysSinceFirstPurch, boughtAgain) ~ shoppingCartValue + voucher + returned + gender,
              data = dataNextOrder,
              x = TRUE, y = TRUE, surv = TRUE)
print(fitCPH)

# Interpret coefficients
exp(fitCPH$coefficient)

# Plot result summary
plot(summary(fitCPH), log = TRUE)
#Survival Probabilities by MonthlyCharges
survplot
#-------------------------------------------------------------------
# Checking model assumptions and making predictions

#Test of PH Assumption
testCPH1 <- cox.zph(fitCPH1)
print(testCPH1)
# rho   chisq        p
# gender=Male               0.0317   1.884 1.70e-01
# SeniorCitizen=Yes         0.0587   6.507 1.07e-02
# Partner=Yes               0.0752  10.116 1.47e-03
# Dependents=Yes            0.0131   0.314 5.75e-01
# StreamMov=NoIntServ      -0.0448   3.588 5.82e-02
# StreamMov=Yes             0.0827  12.174 4.85e-04
# PaperlessBilling=Yes      0.0180   0.611 4.34e-01

#  if variable voilate Propotional Hazard assumption, then their affect changes over time.
#Proportional Hazards for Partner
plot(testCPH1, var = "Partner=Yes")
# the increse in the coefficient is marginal
# if the proportional hazards assumption holds, beta(t) ia a horizontal line.

#Proportional Hazards for MonthlyCharges
plot(testCPH1, var = "MonthlyCharges")

#What if PH Assumption is Violated?  stratified analysis
fitCPH2 <- cph(Surv(tenure, churn) ~ MonthlyCharges +
                 SeniorCitizen + Partner + Dependents + 
                 StreamMov + Contract,
               stratum = "gender = Male",
               data = dataSurv, x = TRUE, y = TRUE, surv = TRUE)

# Categorical variables are added to the argumrnt "stratum",  continuous variables are classed fist
# The other method: time-dependent coefficients deviding by the time of the obcervation into period for which we assume the coefficient to be constant.

#Validating the Model  10 fold
validate(fitCPH1, 
         method = "crossvalidation", 
         B = 10, pr = FALSE)
# index.orig training   test optimism index.corrected  n
# R2        0.2277   0.2279 0.2277   0.0002          0.2276 10

#Probability not to Churn at Certain Timepoint
oneNewData <- data.frame(gender = "Female",
                         SeniorCitizen = "Yes",
                         Partner = "No",
                         Dependents = "Yes",
                         StreamMov = "Yes",
                         PaperlessBilling = "Yes",
                         PayMeth = "BankTrans(auto)",
                         MonthlyCharges = 37.12)
str(survest(fitCPH1, newdata = oneNewData, times = 3))
# List of 5
# $ time   : num 3
# $ surv   : num 0.905   # Probobility this customer will not churn withing 3 month for 90%
# $ std.err: num 0.0136
# $ lower  : num 0.881
# $ upper  : num 0.93

#Survival Curve for new Customer
plot(survfit(fitCPH1,
             newdata = oneNewData))

#Predicting Expected Time until Churn
 print(survfit(fitCPH1,
                +         newdata = oneNewData))
# Call: survfit(formula = fitCPH1, newdata = oneNewData)
# 
# n  events  median 0.95LCL 0.95UCL 
# 5311    1869      65      53      72
 
 #  if the proportional hazard assumption is violated for a predictor we can can divide the data into strata and estimate the model separately within each stratum.

dd %>% head#--------------------------------work with dataNextOrder--------------
# Survival Analysis
#Survival analysis is suited for situations where for some observations an event has not yet happened, but may happen at some point in time.

# work with data about customers of an online shop in order to practice survival analysis. But now it's not about the time until churn, but about the time until the second order.
# Look at the head of the data
library(survival)
dataNextOrder <- read.csv("https://assets.datacamp.com/production/course_6027/datasets/survivalDataExercise.csv", header = T, stringsAsFactors = T)
head(dataNextOrder)

# Plot a histogram
ggplot(dataNextOrder) +
  geom_histogram(aes(x = daysSinceFirstPurch,
                     fill = factor(boughtAgain))) +
  facet_grid( ~ boughtAgain) + # Separate plots for boughtAgain = 1 vs. 0
  theme(legend.position = "none") # Don't show legend
# We can see that there are more customers in the data who bought a second time. Apart from that, the differences between the distributions are not very large.

# Survival Ananlysis
# Create survival object
dataNextOrder %>% head
survObj <- Surv(dataNextOrder$daysSinceFirstPurch, dataNextOrder$boughtAgain)
survObj
# Look at structure
str(survObj)
#typical structure of the survival object: For each observation there is the time under observation, marked with a + if the second order has not been placed yet.


# Kaplan-Meier Analysis (without covariates) using survfit()
head(dataNextOrder)
fitKMSimple <- survfit(survObj ~ 1)
print(fitKMSimple)
plot(fitKMSimple)
# Plot fit
plot(fitKMSimple,
     conf.int = FALSE, xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function")

# Compute fit with categorical covariate
fitKMCov <- survfit(survObj ~ voucher, data = dataNextOrder)
print(fitKMSimple)
# Plot fit with covariate and add labels
# Plot fit with covariate and add labels
plot(fitKMCov, lty = 2:3,
     xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function")
legend(90, .9, c("No", "Yes"), lty = 2:3)
# voucher usage is related to the survival curve! Customers using a voucher seem to take longer to place their second order. They are maybe waiting for another voucher?

#install.packages("survminer")

library(survminer)
fit.coxph <- coxph(survObj ~ voucher + gender + shoppingCartValue + returned , 
                   data = dataNextOrder)
ggforest(fit.coxph, data = dataNextOrder)

#
#Cox Proportional Hazard model on the online shop data
# Determine distributions of predictor variables
dd <- datadist(dataNextOrder)
options(datadist = "dd")

head(dd)
# Compute Cox PH Model and print results
fitCPH <- cph(Surv(daysSinceFirstPurch, boughtAgain) ~ shoppingCartValue + voucher + returned + gender,
              data = dataNextOrder,
              x = TRUE, y = TRUE, surv = TRUE)
print(fitCPH)

# Interpret coefficients
exp(fitCPH$coefficient)

# Plot result summary
survplot(fitCPH,  gender, lable.curves = list(keys= 1: 2))
         
plot(summary(fitCPH), log = TRUE)
#a shopping cart value increase of 1 dollar decreases the hazard to buy again by a factor of only slightly below 1 - but the coefficient is significant, as are all coefficients. For customers who used a voucher, the hazard is 0.74 times lower, and for customers who returned any of the items, the hazard is 0.73 times lower. Being a man compared to a women increases the hazard of buying again by the factor 1.11.
#The influence of the predictors does not change over time.it would not be allowed that the gender "male" has as positive effect on the survival time after a short time under observation, but a large negative effect after a longer time under observation.

# Validation of the  PH assumptio
#Stratify the sample according to this predictor and analyse the strata separately.
#You can divide the data into strata and estimate the model separately within each stratum.

# Checking proportional hazard assumption and print result
testCPH <- cox.zph(fitCPH)
print(testCPH)

# Plot time-dependent beta
plot(testCPH, var = "gender=male")

# Load rms package
library(rms)

# Validate model
validate(fitCPH, method = "crossvalidation",
         B = 10, dxy = TRUE, pr = FALSE)
#Unfortunately, the explanatory power of your model is rather low. You could try to collect more explanatory variables.

# PREDICTION
# The new customer is female and used a voucher in her first order (voucher = 1). The order was placed 21 days ago and had a shopping cart value of 99.90 dollars. She didn't return the order (returned = 0).
# Create data with new customer
newCustomer <- data.frame(daysSinceFirstPurch =21 , shoppingCartValue =99.90, gender = "female", voucher = 1, returned = 0, stringsAsFactors = FALSE)

# Make predictions
pred <- survfit(fitCPH, newdata = newCustomer)
print(pred)
plot(pred)

# Correct the customer's gender
newCustomer2 <- newCustomer
newCustomer2$gender <- "male"

# Redo prediction
pred2 <- survfit(fitCPH, newdata = newCustomer2)
print(pred2)
#The correction of the gender decreased the predicted median time until the second order from 47 to 44 days.
#_______________________________PCA___________________________________

newsData <- read.csv("newsData.RData")#, header=T, stringsAsFactors = T)
# Overview of data structure:
str(newsData, give.attr = FALSE)

# Correlation structure:
newsData %>% cor() %>% corrplot()
#Did you notice the group of intercorrelated variables in the bottom right area of the plot? Let's see if this group reflects in the principal  PCA defined base on the correlation.
#Status Quo
# Variances of all variables before any data preparation
# header = T, stringsAsFactors = T)
#lapply(dataCustomers, var)

#Standardization
#dataCustomers <- dataCustomers %>% scale() %>% as.data.frame()

# Check variances of all variables
#lapply(dataCustomers, var)
# $nOrders                        $salesOrdered
# [1] 1                           [1] 1

#PCA Computation-----------------------------------------------------------------------
pcaCust <- prcomp(dataCustomers)
pcaCust
str(pcaCust, give.attr = FALSE)

# List of 5
# $ sdev    : num [1:16] 2.1 1.84 1.3 1.2 1.12 ...
# $ rotation: num [1:16, 1:16] -0.439 -0.44 -0.33 -0.384 -0.352 ...
# $ center  : Named num [1:16] -4.66e-17 1.90e-17 -1.24e-18 6.69e-18 ...
# $ scale   : logi FALSE

#Standard Deviations of the Components
# Standard deviations going smaller as a importence of variables
pcaCust$sdev %>% round(2)

[1] 2.10 1.84 1.30 1.20 1.12 1.07 0.80 0.78 0.72 0.61 0.48 0.37 0.26
[14] 0.21 0.17 0.13

#Standard Deviations of the Components
 # WHY???  Principal components are extracted such that they cover as much of the original variance as possible. If some variables had larger variance than others, they would be overrepresented in the components.
# Standard deviations
pcaCust$sdev %>% round(2)

# Variances (Eigenvalues) Squaers of SD: then highte variance then more importent component it is
pcaCust$sdev ^ 2 %>% round(2)
[1] 4.39 3.38 1.68 1.45 1.26 1.15 0.65 0.61 0.52 0.38 0.23 0.14 0.07
[14] 0.04 0.03 0.02

# # Proportion of explained variance : eigenvalues /number od components 
# it proportion of this components explaned
(pcaCust$sdev ^ 2/length(pcaCust$sdev)) %>% round(2)
[1] 0.27 0.21 0.10 0.09 0.08 0.07 0.04 0.04 0.03 0.02 0.01 0.01 0.00
[14] 0.00 0.00 0.00

#Loadings and Interpretation
# Loadings (correlations between original variables and components)
round(pcaCust$rotation[, 1:6], 2)

#Values of the Observations
# Value on 1st component for 1st customer
sum(dataCustomers[1,] * pcaCust$rotation[,1])

pcaCust$x[1:5, 1:6]

## --------------------------  working with newsData

# Standardize data
newsData <- newsData %>%scale() %>% as.data.frame()

# Compute PCA
pcaNews<- newsData %>% prcomp()

# Eigenvalues
pcaNews$sdev^2
# The eigenvalues are the variances of the components, i.e., the squared standard deviations.
#The squared standard deviations of the components are called eigenvalues.

#If you divide the variances of the components by the number of components, you get a measure of component importance.

#The loadings are the correlations of the components and the original variables.

#No1. Relevant Components: Explained variance  / choosing component
# Proportion of variance explained:
summary(pcaCust)

#No2. Relevant Components: Kaiser-Guttman Criterion
# Kaiser-Guttman criterion: we keep only when  Eigenvalue > 1 , by contribytion of components
# Kaiser-Guttmann (number of components with eigenvalue larger than 1):
sum(pcaNews$sdev^2 > 1)
pcaCust$sdev ^2 
#3 No. Relevant Components: Screeplot
#The screeplot or: "Find the elbow"

screeplot(pcaCust, type = "lines")
box()
abline(h = 1, lty = 2)

#The Biplot
biplot(pcaCust, choices = 1:2, cex = 0.7)
pcaNews %>% biplot(cex = 0.5)  #Scale the text with the factor 0.5.
 #can see a separated small group of articles with low values on PC1 and low variance in their PC2 values. These articles have a low subjectivity and are neither positive nor negative (which makes sense, because a very positive or negative article would probably also be higher in subjectivity).


# Print loadings of the first six components
pcaNews$rotation[,1: 6] %>% round(2)
# some ideas for the interpretation: PC1 reflects “Subjectivity” (high global_subjectivity and avg_positive_polarity, negative loading on avg_negative_polarity). PC2 contains “Positivity” (high global_sentiment_polarity, low global_rate_negative_words; even negative words are not very negative as you can see from the positive loading on avg_negative_polarity). Here you meet again the group of intercorrelated variables from the corrplot, but they split into two components.

# We can see a separated small group of articles with low values on PC1 and low variance in their PC2 values. These articles have a low subjectivity and are neither positive nor negative (which makes sense, because a very positive or negative article would probably also be higher in subjectivity).

# PC in Regression Analysis I-----------------------------------------------------------
mod1 <- lm(customerSatis ~ ., dataCustomers)

library(car)
vif(mod1)
# variance inflation factor shows many values above 5 or even 10 indicate strong multicollinearity and renders the regression estimats unstable.
# To solve this problem, we are going to use selected PC as regressors insteed.
#PC in Regression Analysis II
# Create dataframe with customer satisfaction and first 6 components
dataCustComponents <- cbind(dataCustomers[, "customerSatis"],
                            pcaCust$x[, 1:6]) %>%
  as.data.frame
mod2 <- lm(customerSatis ~ ., dataCustComponents)

vif(mod2)
#PC1 PC2 PC3 PC4 PC5 PC6 
#1   1   1   1   1   1
#because PC complitely  uncorrelated all variance inflation factors equal one now.

summary(mod1)$adj.r.squared
#[1] 0.8678583

summary(mod2)$adj.r.squared
#[1] 0.7123822
# if we compare the model1 and 2 second model is more stable even R2 is lower, bicause less variables.

# Interpritation 
summary(mod2)
# Remember the PC1 reflecter low customer activity, so negativ value means the customers with low activity are less satisfited.
# In contract P2 - customers with fe returnspca are more satisfited
# PC3 Quality or brand awareness is not significant

# Learnings and Relevance
# Learnings about PCA
# You have learned...	to reduce the number of variables without losing too much information
# that variables should be standardized before a PCA
# how to decide on the number of relevant components
# to interpret the selected components
# Learnings from the model
# You have learned...	that the original variables can be reduced to 6 components, i.a., customer activity, return behavior and brand awareness
# that using the first six components to explain customer satisfaction causes a decrease in explained variance, but solves the multicollinearity problem

# Predict log shares with all original variables
mod1 <- lm(logShares ~ ., data = newsData)

# Create dataframe with log shares and first 6 components
dataNewsComponents <- cbind(logShares = newsData[, "logShares"],
                            pcaNews$x[, 1:6]) %>%
  as.data.frame()

# Predict log shares with first six components
mod2 <- lm(logShares ~ ., data = newsData)

# Print adjusted R squared for both models
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared