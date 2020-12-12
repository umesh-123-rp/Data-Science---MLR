# Consider only the below columns and prepare a prediction model for predicting Price.
# Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
# Model -- model of the car
# Price  -- Offer Price in EUROs	
# Age_08_04 -- Age in months as in August 2004	
# Mfg_Month -- Manufacturing month (1-12)	
# Mfg_Year	-- Manufacturing Year
# KM -- Accumulated Kilometers on odometer
# Fuel_Type	 -- Fuel Type (Petrol, Diesel, CNG)
# HP -- Horse Power
# Met_Color	 -- Metallic Color?  (Yes=1, No=0)
# Color -- Color (Blue, Red, Grey, Silver, Black, etc.)
# Automatic	-- Automatic ( (Yes=1, No=0)
# cc -- Cylinder Volume in cubic centimeters
# Doors -- Number of doors
# Cylinders	-- Number of cylinders
# Gears -- Number of gear positions
# Quarterly_Tax -- Quarterly road tax in EUROs
# Weight -- Weight in Kilograms

# Loading the data
Toyota_Corolla<-read.csv("D:\\Data\\IN102385\\Downloads\\Toyoto_Corrola (1).csv")
View(Toyota_Corolla)
Toyota_Corolla1<-Toyota_Corolla[,-c(1,2)]
View(Toyota_Corolla1)
str(Toyota_Corolla1)
summary(Toyota_Corolla1)
# It shows that cylinder is constant, which does not give any variance.
# The variable cylinder can be eliminated from the data set for analysis
Toyota_Corolla2<-Toyota_Corolla1[,-6]
View(Toyota_Corolla2)
# Check for correlation between output and input variables among all input variables
pairs(Toyota_Corolla2)
cor(Toyota_Corolla2)
# There is a negative strong correlation found between price and age(-0.876)
# There is a negative moderate correlation found between price and km(-0.57)
# There is a positive correlation found between price and weight(0.58)
# There is a positive correlation between age and km (0.50)
attach(Toyota_Corolla2)
model1<-lm(Price ~. , data=Toyota_Corolla2)
summary(model1)
# R^2 value is observed 0.86 and Door variable was found insignificant
plot(model1)
install.packages("car")
library(car)
car::vif(model1)
# VIF values are found to be less than 10. There is no Collinearity observed.
library(MASS)
stepAIC(model1)
# AIC value decreases by removing the insignificant variable i.e. Door
residualPlots(model1)
avPlots(model1)
qqPlot(model1)
sqrt(sum(model1$residuals^2)/nrow(Toyota_Corolla2))

# QQ Plot looks to be normal.Residual plot of Age is showing a trend
model2<-lm(Price ~ Age_08_04+I(Age_08_04^2)+KM+HP+Gears+Weight, data=Toyota_Corolla2)
summary(model2)
# R^2 value improved to 0.88
# All the variables are found to be significant
plot(model2)
residualPlots(model2)
avPlots(model2)
qqPlot(model2)
sqrt(sum(model2$residuals^2)/nrow(Toyota_Corolla2))

# Trend was found in HP variable in the residual plot
# model was further improved by adding HP^2
model3<-lm(Price ~ Age_08_04+I(Age_08_04^2)+KM+HP+I(HP^2)+Gears+Weight, data=Toyota_Corolla2)
summary(model3)
plot(model3)
residualPlots(model3)
avPlots(model3)
qqPlot(model3)
# No trend is observed in residual plot
# But Data points 222 & 602 are found out of the normal plot.
# These data points can be verified in Diagnosis plots
influenceIndexPlot(model3)
sqrt(sum(model3$residuals^2)/nrow(Toyota_Corolla2))

# 222 & 602 data points also observed in cooks' distance plot
# These two influencing points can be removed from the model
model4<-lm(Price ~ Age_08_04+I(Age_08_04^2)+KM+HP+I(HP^2)+Gears+Weight,
                                       data=Toyota_Corolla2[-c(222,602),])
summary(model4)
# R^2 value improved to 0.89
plot(model4)
residualPlots(model4)
avPlots(model4)
qqPlot(model4)
influenceIndexPlot(model4)
sqrt(sum(model4$residuals^2)/nrow(Toyota_Corolla2[-c(222,602),]))

# Trend was observed in KM residual plot and
# some influencing data points were identified
model5<-lm(Price ~ Age_08_04+I(Age_08_04^2)+KM+HP+I(HP^2)+Gears+Weight+I(Weight^2),
           data=Toyota_Corolla2[-c(148,192,193,222,602,961,524),])
summary(model5)
plot(model5)
residualPlots(model5)
avPlots(model5)
qqPlot(model5)
influenceIndexPlot(model5)
sqrt(sum(model5$residuals^2)/nrow(Toyota_Corolla2[-c(148,192,193,222,602,961,524),]))
# No residual trend observed.
# It is found to be normal in Q-Q Plot
# R^2 is found to be improved to 0.9 and all the input variables are found to be significant.
model_R_Squared_RMSE_values <- list(model=NULL,R_squared=NULL,RMSE=NULL)
model_R_Squared_RMSE_values[["model"]] <- c("model1","model2","model3","model4","model5")
model_R_Squared_RMSE_values[["R_squared"]] <- c(0.86,0.88,0.87,0.89,0.90)
model_R_Squared_RMSE_values[["RMSE"]]<-c(1342,1240,1258,1195,1112)
final_model <- cbind(model_R_Squared_RMSE_values[["model"]],model_R_Squared_RMSE_values[["R_squared"]],model_R_Squared_RMSE_values[["RMSE"]])
View(model_R_Squared_RMSE_values)
View(final_model)
# Final model is as given below :
final_model<-lm(Price ~ Age_08_04+I(Age_08_04^2)+KM+HP+I(HP^2)+Gears+Weight+I(Weight^2),
           data=Toyota_Corolla2[-c(148,192,193,222,602,961,524),])
pred<-predict(final_model,interval="predict")
pred1<-data.frame(pred)
pred1
Toyota_Corolla3<-Toyota_Corolla2[-c(148,192,193,222,602,961,524),]
cor(pred1$fit,Toyota_Corolla3$Price)
# Since there is a consistent value in R^2 and all the variables are significant, We can take it as final model
# R-Square value is 0.90 and Correlation between fitting value with price is 0.95


