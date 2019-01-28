#DESCRIPTIVE AND PREDICTIVE ANALYTICS PROJECT - GROUP 8

#DSC is an organization that helps non-profit organizations finding donors for their good causes.
#They have a huge database with candidate donors. DSC wants to launch a new re-activation campaign: 
#They want to send letters to donors that have been inactive for a long time, hoping that they will donate again. 
#They only want to send letters to candidate donors that are likely to donate more than EUR 35. 
#Therefore, they need your help: can you construct a model that predicts which donors are most likely to donate 
#more than EUR 35 for a re-activation campaign?

#Installing Libraries
install.packages('readr')
install.packages('dplyr')
install.packages('forecast')
install.packages('lubridate')
install.packages('fastDummies')
install.packages('knitr')
install.packages('pROC')
install.packages('corrr')
install.packages('sqldf')
install.packages('car')
install.packages('rfm')
install.packages('caret')
install.packages('ROCR')
install.packages('DMwR')
install.packages('plotly')

#Libraries Used.
library(readr)
library(dplyr)
library(forecast)
library(lubridate)
library(fastDummies)
library(knitr)
library(pROC)
library(corrr)
library(sqldf)
library(car)
library(rfm)
library(caret)
library(ROCR)
library(DMwR)
library(plotly)



#Defined Functions.
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)
  return(auc)
}
setwd("C:/Users/pvalenciajimenez/Documents/Descriptive and Predictive Analytics/Project")
#Importing Datasets
gifts <- data.frame(read_delim("gifts.csv", ";", escape_double = FALSE, 
                               col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                               trim_ws = TRUE))

donors <- data.frame(read_delim("donors.csv", ";", escape_double = FALSE, 
                                col_types = cols(zipcode = col_character()), 
                                trim_ws = TRUE))

c2014 <- data.frame(read_delim("campaign20140115.csv", ";", escape_double = FALSE, trim_ws = TRUE))

c2013 <- data.frame(read_delim("campaign20130411.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE))

#Creation of Copies
c_gifts <- gifts
c_donors <- donors
c_c2013 <- c2013
c_c2014 <- c2014

#Understanding The Data

glimpse(c_gifts)
glimpse(c_donors)
glimpse(c_c2013)
glimpse(c_c2014)

#Data Cleaning

#Creating Target Variable and selection of unique values.
c_c2013$Target <- ifelse(c_c2013$amount>35,1,0)
c_c2014$Target <- ifelse(c_c2014$amount>35,1,0)

c_c2013$amount <- NULL
c_c2014$amount <- NULL

table(duplicated(c_c2013)) 
table(duplicated(c_c2014))
c_c2013 <- unique(c_c2013)
c_c2014 <- unique(c_c2014)

#Creation of New Regions Column.
c_donors$region <- NULL
c_donors$region=ifelse(c_donors$zipcode<1500,"WalloonBrabant",ifelse(c_donors$zipcode<2000,"FlemishBrabant",
                ifelse(c_donors$zipcode<3000,"Antwerp",ifelse(c_donors$zipcode<3500,"FlemishBrabant",
                ifelse(c_donors$zipcode<4000,"Limburg",ifelse(c_donors$zipcode<500,"Liege",
                ifelse(c_donors$zipcode<6000,"Namur",ifelse(c_donors$zipcode<6600,"Hainut",
                ifelse(c_donors$zipcode<7000,"Luxembourg",ifelse(c_donors$zipcode<8000,"Hainut",
                ifelse(c_donors$zipcode<9000,"WestFlanders","EastFlanders")))))))))))

#Conversion of NAs to 0.
c_donors$zipcode <- as.numeric(c_donors$zipcode)   
c_donors$zipcode[is.na(c_donors$zipcode)] <- 0
c_donors$zipcode <- NULL


#Working with c_gifts and creation of required variables for final training and testing data.
c_gifts1 <- filter(c_gifts,date<='2013-04-11')
c_gifts1$TargetDate <- as.Date('2013/04/11','%Y/%m/%d')
c_gifts2 <- filter(c_gifts,date<='2014-01-15')
c_gifts2$TargetDate <- as.Date('2014/01/15','%Y/%m/%d')

#Creation of Recency, Monetary and Frequency Variables (RMF Variables) including a variable which allows us to understand who gifted more than 35 Euros.
c_gifts1 <- sqldf('SELECT donorID, ((MAX(TargetDate) - MAX(Date))) as Days_Recency, SUM(amount) as MVal, COUNT(campID) as Frequency   
                FROM c_gifts1 
                GROUP BY donorID') 

c_gifts2 <- sqldf('SELECT donorID, ((MAX(TargetDate) - MAX(Date))) as Days_Recency, SUM(amount) as MVal, COUNT(campID) as Frequency 
               FROM c_gifts2 
               GROUP BY donorID')

#Creation of Amount35 (target wether gift amount is > than 35)
c_gifts1$Amount35 <- ifelse(c_gifts1$MVal > 35, 1, 0)
c_gifts2$Amount35 <- ifelse(c_gifts2$MVal > 35, 1, 0)


#Creation of Training and Testing Datasets by creating Preliminary Tables which are finally added to the required Tables.
prelim1 <- left_join(c_c2013,c_donors)
prelim2 <- left_join(c_c2014,c_donors)

c_train <- left_join(prelim1, c_gifts1 ,by = "donorID")
c_test <- left_join(prelim2, c_gifts2,by = "donorID")

#Data Check.
summary(c_train)
summary(c_test)

#Removal of NAs.
c_train$Frequency[is.na(c_train$Frequency)] = 0 
c_train$Days_Recency[is.na(c_train$Days_Recency)] = 0
c_train$MVal[is.na(c_train$MVal)] = 0
c_train$Amount35[is.na(c_train$Amount35)] = 0

c_test$Frequency[is.na(c_test$Frequency)] = 0
c_test$Days_Recency[is.na(c_test$Days_Recency)] = 0
c_test$MVal[is.na(c_test$MVal)] = 0
c_test$Amount35[is.na(c_test$Amount35)] = 0

#Creation of Dummy Variables for Proper Calculations.
c_train <- dummy_cols(c_train)
c_test <- dummy_cols(c_test)

#Selection of Required Variables and Removal of Unwanted Variables.
c_train <- c_train[,c(1, 2, 6:9, 10:25)]
c_train$language_F <- NULL
c_train$region_Limburg <- NULL
c_train$gender_S <- NULL
c_train$gender_U <- NULL

c_test <- c_test[,c(1, 2, 6:9, 10:25)]
c_test$language_F <- NULL
c_test$region_Limburg <- NULL
c_test$gender_S <- NULL
c_test$gender_U <- NULL

# saving clean datasets
write.csv(c_train, file = "C:/Users/pvalenciajimenez/Documents/Descriptive and Predictive Analytics/Project/Train.csv")
write.csv(c_test, file = "C:/Users/pvalenciajimenez/Documents/Descriptive and Predictive Analytics/Project/Test.csv")

#Correlation Check using a cutoff value of 0.8.
CorrDF <- cor(c_train)
Reqd <- findCorrelation(CorrDF, cutoff=0.8)
Reqd <- sort(Reqd)
Needed_Data <- CorrDF[,-c(Reqd)]

#Converting the Target Variables to Factors for Model Building.
c_train$Target <- factor(c_train$Target)
c_test$Target <- factor(c_test$Target)

#Data Modelling.
train <- c_train
test <- c_test

#Running the SMOTE function to balance the Data. 
smoted.data <- SMOTE(Target ~., train, perc.over = 100, perc.under = 200)

#Manual Elimination Procedure.
Fit <- glm(Target~., data=smoted.data, family = binomial)
summary(Fit)
Fit_Updated <- glm (Target~. -region_WalloonBrabant-region_EastFlanders-region_Luxembourg-region_Hainut-region_Antwerp
                    -region_Liege-region_Namur-region_Hainut-region_FlemishBrabant-region_WestFlanders, data=smoted.data, family = 'binomial')
summary(Fit_Updated)
predict <- predict(Fit_Updated, test, type = 'response')

#AUC Calculation.
predicttrain_BE <- predict(Fit_Updated,newdata=train,type="response")
predicttest_BE <- predict(Fit_Updated,newdata=test,type="response")
auctrain_BE <- auc(train$Target,predicttrain_BE)
auctest_BE <- auc(test$Target,predicttest_BE)

#Creation of Predictors and Performance.
Trainpred_BE <- prediction(predicttrain_BE, train$Target)
Testpred_BE <- prediction(predicttest_BE, test$Target)
Trainperf_BECG <- performance(Trainpred_BE, 'tpr','fpr')
Testperf_BECG <- performance(Testpred_BE, 'tpr','fpr')
Trainperf_BELC <- performance(Trainpred_BE, 'lift','rpp')
Testperf_BELC <- performance(Testpred_BE, 'lift','rpp')

#Plotting the Cumulative Gains Curve.
plot(Trainperf_BECG, main = 'Cumulative Gains Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_BECG, col = 'red', text.adj = c(-0.2,1.7))

#Plotting the Lift Curve.
plot(Trainperf_BELC, main = 'Lift Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_BELC, col = 'red', text.adj = c(-0.2,1.7))

#Forward Stepwise Regression.
Forward_Step <- step(glm(Target ~ 1, data = train, family = 'binomial'), direction = 'forward', scope = formula(Fit_Updated))
summary(Forward_Step)

#AUC Calculation.
predicttrain_FS <- predict(Forward_Step,newdata=train,type="response")
predicttest_FS <- predict(Forward_Step,newdata=test,type="response")
auctrain_FS <- auc(train$Target,predicttrain_FS)
auctest_FS <- auc(test$Target,predicttest_FS)

#Creation of Predictors and Performance.
Trainpred_FS <- prediction(predicttrain_FS, train$Target)
Testpred_FS <- prediction(predicttest_FS, test$Target)
Trainperf_FSCG <- performance(Trainpred_FS, 'tpr','fpr')
Testperf_FSCG <- performance(Testpred_FS, 'tpr','fpr')
Trainperf_FSLC <- performance(Trainpred_FS, 'lift','rpp')
Testperf_FSLC <- performance(Testpred_FS, 'lift','rpp')

#Plotting the Cumulative Gains Curve.
plot(Trainperf_FSCG, main = 'Cumulative Gains Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_FSCG, col = 'red', text.adj = c(-0.2,1.7))

#Plotting the Lift Curve.
plot(Trainperf_FSLC, main = 'Lift Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_FSLC, col = 'red', text.adj = c(-0.2,1.7))

#Both Direction Stepwise Regression.
BothDirection_Step <- step(glm(Target ~ 1, data = train, family = 'binomial'), direction = 'both', scope = formula(Fit_Updated))
summary(BothDirection_Step)

#AUC Calculation.
predicttrain_B <- predict(BothDirection_Step,newdata=train,type="response")
predicttest_B <- predict(BothDirection_Step,newdata=test,type="response")
auctrain_B <- auc(train$Target,predicttrain_B)
auctest_B <- auc(test$Target,predicttest_B)

#Creation of Predictors and Performance.
Trainpred_B <- prediction(predicttrain_B, train$Target)
Testpred_B <- prediction(predicttest_B, test$Target)
Trainperf_BCG <- performance(Trainpred_B, 'tpr','fpr')
Testperf_BCG <- performance(Testpred_B, 'tpr','fpr')
Trainperf_BLC <- performance(Trainpred_B, 'lift','rpp')
Testperf_BLC <- performance(Testpred_B, 'lift','rpp')

#Plotting the Cumulative Gains Curve.
plot(Trainperf_BCG, main = 'Cumulative Gains Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_BCG, col = 'red', text.adj = c(-0.2,1.7))

#Plotting the Lift Curve.
plot(Trainperf_BLC, main = 'Lift Curve', col = 'blue', text.adj = c(-0.2,1.7))
par(new = TRUE)
plot(Testperf_BLC, col = 'red', text.adj = c(-0.2,1.7))

#Calculation of profit
population_size<-nrow(c_donors) #number of elements in donors data
target_incidence<-((sum(c_train$Target==1)/nrow(train))+(sum(c_test$Target==1)/nrow(test)))/2 #percentage of target = 1
reward_target<-35
cost_campaign<-0.5
perc_selected<- 0.3
lift<-2
perc_target<- lift * target_incidence

profit=function(perc_target, perc_selected, population_size, reward_target, cost_campaign) {
  cost<-cost_campaign * perc_selected * population_size
  reward<- reward_target * perc_target * perc_selected * population_size
  return(reward - cost) 
}  

profit_value<-profit(perc_target, perc_selected, population_size, reward_target, cost_campaign)

number_targets_toreach<-population_size*target_incidence*0.80
perc_targets<-number_targets_toreach/(target_incidence*population_size)
cumulative_gains<-0.60
number_donors_toreach<-cumulative_gains*population_size

#Profit value
profit_value
number_targets_toreach
perc_targets
number_donors_toreach



