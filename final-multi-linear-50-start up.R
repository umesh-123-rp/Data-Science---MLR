# Prepare a prediction model for profit of 50_startups data.
# Do transformations for getting better predictions of profit and 
# make a table containing R^2 value for each prepared model.

# Loading the data
opt_profit<-read.csv("D:\\Data\\IN102385\\Downloads\\50_Startups.csv")
View(opt_profit)
summary(opt_profit)
str(opt_profit)
# Defining State as a factor
opt_profit$State<-factor(opt_profit$State,levels=c('New York','California','Florida'), labels=c(1,2,3))
str(opt_profit)
install.packages("moments")
library(moments)
install.packages("lattice")
library(lattice)
attach(opt_profit)
# Understanding data of R.D. Spend
skewness(R.D.Spend)
kurtosis(R.D.Spend)
dotplot(R.D.Spend)
hist(R.D.Spend)
qqnorm(R.D.Spend)
qqline(R.D.Spend)
boxplot(R.D.Spend)
# R.D. Spend does not exactly follow the normal distribution.
# Mean is greater than median and skewness is +ve. It indicates positively skewed distributed
# However, there is no outliers as evident from the boxplot.

# Understanding data of Administration
skewness(Administration)
kurtosis(Administration)
dotplot(Administration)
hist(Administration)
qqnorm(Administration)
qqline(Administration)
boxplot(Administration)
# This is a negative skewed distributed with -ve skewness. Mean is lower than median value.
# However,there is no outliers

# Understanding Marketing Spend
skewness(Marketing.Spend)
kurtosis(Marketing.Spend)
dotplot(Marketing.Spend)
hist(Marketing.Spend)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
boxplot(Marketing.Spend)
# This is negative skewed distributed with negative skewness.
# Median is more than mean which also indicates that it is -ve skewed distributed.
# There is no outlier exists
# Understanding the output -Profit
skewness(Profit)
kurtosis(Profit)
dotplot(Profit)
hist(Profit)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
boxplot(Profit)
# Profit is positive skewed distributed with mean greater than median.
# There is no outlier in it

# Relationship of output Profit with other variables & relation among all input variables
pairs(opt_profit)
cor(opt_profit[,-4])
# It indicates that there is a strong correlation i.e. 0.97 between R.D.Spend and Profit
# There is a moderate correlation i.e. 0.75 between Marketing Spend and Profit
# There is a moderate correlation i.e. 0.72 between R.D.Spend and Marketing Spend
# State and Administration do not have any effect on profit or any other variables

# Building Model of Profit with input variables
library(caTools)
model1<-lm(Profit~.,data=opt_profit)
summary(model1)
plot(model1)
# R^2 is 0.95 which is Excellent
# R.D.Spend is found to be significant and others are found to be insignificant
# Check colinearity among input variables
install.packages("car")
library(car)
install.packages(carData)
library(carData)
car::vif(model1)
# VIF values for all variables are found to be less than 10- No colinearity
library(MASS)
stepAIC(model)
# AIC values were found to be reducing in absence of State and Administration.
# Therefore, these two variables are not significant
residualPlots(model)
avPlots(model)
qqPlot(model)
# There is no trend found in residual plots
# R.D.Spend & Marketing Spend found to have contributions to prediction of Profit
# First Iteration (Removal of State)
model2<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=opt_profit)
summary(model2)
# Second Iteration (Removal of Administration)
model3<-lm(Profit~R.D.Spend+Marketing.Spend,data=opt_profit)
summary(model3)
# Third Iteration (Removal of Marketing Spend)
model4<-lm(Profit~R.D.Spend,data=opt_profit)
summary(model4)
# Since there is a decrease in R^2 value by not considering the Marketing Spend.
# Moreover this is also significant at 90% significance level
# Therefore, let's consider both the variables
model5<-lm(Profit~R.D.Spend+Marketing.Spend, data=opt_profit)
summary(model5)
plot(model5)
pred<-predict(model5,interval="predict")
pred1<-data.frame(pred)
cor(pred1$fit,opt_profit$Profit)
plot(pred1$fit,opt_profit$Profit)
# Correlation between predicted and actual found to be strong i.e. 0.97
# For further improvement in the model, we can check data points influencing the model
influenceIndexPlot(model5)
# We have observed the data point 50 is above limits in Diagnosis plots.
# We can make the model by eliminating the influencing data point 50
model6<-lm(Profit~R.D.Spend+Marketing.Spend, data=opt_profit[-50,])
summary(model6)
# R^2 value has improved to 0.96
# Calculation of RMSE
sqrt(sum(model5$residuals^2)/nrow(opt_profit))

model_R_Squared_values <- list(model=NULL,R_squared=NULL,RMSE=NULL)
model_R_Squared_values[["model"]] <- c("model1","model2","model3","model4","model5","model6")
model_R_Squared_values[["R_squared"]] <- c(0.95,0.95,0.95,0.94,0.95,0.96)
model_R_Squared_values[["RMSE"]]<-c(9439,9232,9161,9416,9161,7192)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]],model_R_Squared_values[["RMSE"]])
View(model_R_Squared_values)
View(Final)
# Final model is as given below :
final_model<-lm(Profit~R.D.Spend+Marketing.Spend, data=opt_profit[-50,])
summary(final_model)
pred<-predict(final_model,interval="predict")
pred1<-data.frame(pred)
pred1
cor(pred1$fit,opt_profit[-50,]$Profit)
plot(pred1$fit,opt_profit[-50,]$Profit)
# Final model gives R^2 value 0.96 and correlation with fitting value as 0.98
