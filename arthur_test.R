library(ggplot2)
library(dplyr)
library(tidyverse)
library(fields)
library(maps)
library(sp)
library(mgcv) #need for GAM

#get filepath
cwd <- getwd()
load("datasets_project.RData")

TBdata$real_rate <- (TBdata$TB / TBdata$Population)*100000

#-----Using the Model with Training and Testing Data to Validate Predictions-----

# splitting the data into training and testing and then comparing 2014 risk vs actual data
train_df <- subset(TBdata, Year %in% c(2012, 2013))
test_df <- subset(TBdata, Year == 2014)

# fitting the model on training data (2012 and 2013)
model6 <- gam(TB ~ s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=65, bs='cs') + 
                s(Poverty, k=65, bs='cs') + s(Density, k=65, bs='cs') +
                s(lat, lon, k=65, bs='tp') + s(Unemployment, k=65, bs='cs') +
                s(Poor_Sanitation, k=65, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=train_df)

# summary, residual and check plots
summary(model6)
par(mfrow=c(2,2))
gam.check(model6) # residual plots and additional summary info
par(mfrow=c(3,3))
plot(model6)

# predicting risk and plotting against known data
risk_preds = predict(model6, newdata=test_df, type='link') #this returns log of predicted cases
test_df$pred_risk = (exp(risk_preds) / test_df$Population) * 100000   #turn predicted cases into predicted risk (TB cases per 100000)

plot(test_df$real_rate,test_df$pred_risk)