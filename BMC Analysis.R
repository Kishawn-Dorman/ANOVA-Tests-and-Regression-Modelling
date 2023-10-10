#Load Dataset.
df <- read.csv("BMC Dataset full.csv", header = T)

#Create bar charts to display statistical information for outcome variables.

#Fit a table for clients age.
Age <- table(df$age)
#plot chart to show the ages of each client.
barplot(Age , space =0.10, main="Ages of clients",
        xlab="Age",
        ylab="Clients",
        border="Black",
        col="Gold", density =25)

#Fit a table for housing loan.
Housing <- table(df$housing)
#plot chart to show clients with or without a housing loan.
barplot(Housing , space =0.5, main="Client's with or without a housing loan",
        xlab="Response",
        ylab="Clients",
        border="Black",
        col="Blue", density =25)



#LINEAR MODEL
#1. Remove any incomplete cases/data.
df <- na.omit(df)

#2. Variable 12(duration) can skew the performance of the models as clients 
#information and responses are captured during contact.
#For this reason variable 12 will be removed from the dataset.
df <- subset (df, select = -duration)

#3. Set a seed to make these same sample groups reproducible.
set.seed(999)

#4. The data will be split to limit the occurrence of overfitting, even though its
#unlikely.
#Split the data into a training and test set
library(caret)
inTrain <- createDataPartition(df$age, p =0.8, list = F)
dftrain <- df[inTrain,]
dftest <- df[-inTrain,]
#dfTrain contain 80% of the data and dfTest contain 20% of the data.

#5. Create a full model on the training set with the "age" variable as the outcome
#and all other variables as predictors.
linmodel <- lm(age ~ ., data = dftrain)
summary(linmodel)
#p-value < 0.001 and adjusted R-squared value = 0.42, suggest that linmodel is significant
# & the model's predictors cover significant variance of the outcome variable. 

#6. Use step wise method to reduce the model by keeping the most significant predictors only.
linstep <- step(linmodel, k = log(nrow(dftrain)))
summary(linstep)
#R-squared value = 0.42, the predictors of this model (linstep) have the same amount 
#of variance on the response variable(age) as the full model.

#7. Determine if the step wise model is better than the full model at fitting the data
#using BIC values and BF.
#Calculate BIC on full model & step wise model
BIC(linmodel)
BIC(linstep)

#8. Run a Base Factor Analysis
BF <- exp((254563.5- 254514.2)/2)
BF
#The stepwise model(linstep) is 50740994964 times more likely to fit the data better 
#than the full model(linmodel) and will be used for the pred testing. 

#9. Prediction of the age of clients in dftest.
#The stepwise model(linstep) was built around dftrain, which means the data in 
#linstep is unseen to dfTest. 
#Use predict function and view predicted age values with linstep model.
pred <- predict.lm(linstep,dftest)
pred

#10. View actual age values of dftest to compare pred results.
View(dfTest)
#Just by looking at the output we can see that the model is doing fairly okay,
#in comparison to dftest.

#11. Check R-squared value to see how well the model did on the test set.
cor(pred,dftest$age)^2

#12. Check linstep model R-squared.
summary(linstep)
#The model performed as expected on the test set as the same r-squared value was
#registered for both. 

#13. Add the pred results to the test dataset (dfTest)
dftest$predlin <- pred

#14. Create a scatterplot to show the variance between the pred & actual age values.
library(ggplot2)
dftest$predlin <- as.integer(dftest$predlin)
ggplot(dftest, aes(x = age, y = predlin)) + geom_point() + geom_smooth(method=lm, se=FALSE)
#the diagram shows their is a positive linear relationship between age and pred 
#as when age increases pred increases as well.

#15. Plot a graph depicting the zone of the predicted ages against the actual ages.
fitResults <- lm(age ~ predlin, data = dftest)
library(sjPlot)
plot_model(fitResults, type = "pred")
#The plot shows that the area of variance is very small which reflects a high r squared. 
#This suggest that the model built from dftrain was good at predicting clients age
#in dftest.

#16. Check diagnostics plot
plot(linstep)
#The scatterplot shows a typical fitted value vs. residual plot in which 
#homoscedasticity is present. The Q-Q plot shows a normal distribution followed.





#LOGISTIC MODEL
#1. Load Dataset.
df <- read.csv("BMC Dataset full.csv", header = T)

#2. Remove any incomplete cases/data.
df <- na.omit(df)

#3. Variable 12(duration) can skew the performance of the models as clients 
#information and responses are captured during contact.
#For this reason variable 12 will be removed from the dataset.
df <- subset (df, select = -duration)

#4. Convert housing variable to binary for logistic regression & confusion matrix.
df$housing = ifelse(df$housing == "yes",1,0)
#The categories of the housing variable was changed to the following numeric values
#yes=1, no=0 

#5. Set a seed to make these same sample groups reproducible.
set.seed(999)

#6. Split the data into a training and test set
library(caret)
inTrain <- createDataPartition(df$housing, p = 0.8, list = F)
dftrain <- df[inTrain,]
dftest <- df[-inTrain,]
#dftrain contain 80% of the data and dftest contain 20% of the data.

#7. Create a full model on the training set with the "housing" variable as the outcome
#and all other variables as predictors.
logmodel <- glm(housing ~ ., data = dftrain, family = "binomial")
summary(logmodel)

#8. Build a second model without y variable because it can act as a direct indication 
#of the outcome variable 'housing'.
log2model <- glm(housing ~ age + job + marital + education + default + balance + 
                   loan + contact + day + month + campaign + pdays + previous + 
                   poutcome, data = dftrain, family = "binomial")
summary(log2model)

#9. Use step wise method to reduce the model by keeping the most significant predictors only.
logstep <- step(log2model, k = log(nrow(dftrain)))
summary(logstep)

#10 Calculate BIC on full model & stepwise model
BIC(log2model)
BIC(logstep)

#11. Use BIC values to do Bayes Factor Analysis
BF <- exp((37189.11 - 37172.65)/2)
BF
#logstep model is 3752 times more likely to fit the data than the logmodel.

#12. Measure how well the outcome variable(housing) is being predicted by the stepwise model.
1- logstep$deviance/logstep$null.deviance
#Deviance value is 0.26. 

#13. Find log odds of stepwise model.
#view model 
summary(logstep)
#jobblue-collar appears first from the job predictor variable, so that means that
#R has chosen jobadmin as the intercept.

#14. Find exponent of coefficients for significant predictors(p-value<0.05).
exp(logstep$coefficients[2]) 
1-0.97
exp(logstep$coefficients[3])
1.37-1
exp(logstep$coefficients[5])
1-0.56
exp(logstep$coefficients[7])
1-0.38
exp(logstep$coefficients[8])
1-0.71
exp(logstep$coefficients[10])
1-0.20
exp(logstep$coefficients[12])
1-0.60
exp(logstep$coefficients[13])
1-0.11
exp(logstep$coefficients[15])
1-0.62
exp(logstep$coefficients[18])
1-0.65
exp(logstep$coefficients[19])
1-1
exp(logstep$coefficients[20])
1-0.74
exp(logstep$coefficients[21])
1.65-1
exp(logstep$coefficients[22])
1-0.99
exp(logstep$coefficients[23])
1-0.09
exp(logstep$coefficients[24])
1-0.11
exp(logstep$coefficients[25])
1-0.29
exp(logstep$coefficients[26])
1-0.29
exp(logstep$coefficients[27])
1-0.42
exp(logstep$coefficients[28])
1-0.21
exp(logstep$coefficients[29])
1-0.17
exp(logstep$coefficients[30])
2.13-1
exp(logstep$coefficients[31])
1-0.61
exp(logstep$coefficients[32])
1-0.17
exp(logstep$coefficients[33])
1-0.13
exp(logstep$coefficients[34])
1.04-1
exp(logstep$coefficients[35])
1-1
exp(logstep$coefficients[36])
1-0.85
exp(logstep$coefficients[37])
1-0.36

#15. Test prediction of housing loan status in dftest.
pred <- predict(logstep, dftest, type = "response")
head(pred)
#These predictions show the probability of the outcome variable being classified
#into 1.

#16. Add the pred results to the test dataset (dfTest)
dftest$predlog <- pred

#17. Generate a confusion matrix with a default cut-off point 0.5.
pred <- ifelse(pred > 0.5, 1, 0)
print(confusionMatrix(as.factor(pred), as.factor(dftest$housing), positive = "1"))
#In this case the 0.5 cut off gave good balance between sensitivity & specificity 
#however, the objective of the model is to predict clients with housing loan with
#high accuracy. 

#18. plot a roc-curve to pinpoint a suitable cut-off point for the purpose above.
library(verification)
pred <- predict(logstep, dftest, type = "response")
roc.plot(dftest$housing == 1, pred)
#this shows that cut off point of 0.4 maybe the best point for a more sensitive matrix.

#19. Generate confusion matrix with 0.4 cut-off point.
pred <- ifelse(pred > 0.4, 1, 0)
print(confusionMatrix(as.factor(pred),as.factor(dftest$housing), positive = "1"))
#sensitivity increased and specificity decreased.
#2296 true negatives
#674 false negatives
#1669 false positives
#4403 true positives

#20. MODEL PERFORMANCE
  #1. ACCURACY: 0.74 = 74%

  #2. MISCLASSIFICATION: 1-Accuracy = 0.26 = 26%

  #3. SENSITIVITY (true positive rate): 0.87 = 87%
  #The model performed well with classifying clients with a housing loan correctly
  #as only 13% of these clients were missed.

  #4. SPECIFICITY (true negative rate): 0.58 = 58%
  #The model classified 58% of clients without a housing loan successfully and 42%
  #of these clients were missed.

  #5. PRECISION: 4403/(4403+1669) = 0.73
  #73% of positive classifications were correct.

  #6. RECALL: 4403/(4403+674) = 0.87
  #87% chance of correctly classifying as positive.

  #F1-score: (2(4403))/(2(4403)+1669+674) = 0.79 = 79%
  #The model seems to be guided in its intended purpose as the F1-score indicate 
  #that there is a good balance between precision and recall.

#21. PARAMETRIC ASSUMPTIONS
  #Check Multicollinearity.
library(car)
vif(logstep)
#final model does not suffer from multicollinearity vif<5 (logstep). 
#Meaning that there is no significant relationship between the predictors.

# Check qqplot for extreme outliners.
plot(logstep)
#There are outliners however, they are not extreme.



