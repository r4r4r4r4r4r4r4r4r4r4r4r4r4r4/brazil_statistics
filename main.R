#MAIN FILE FOR WORKING

#import dependencies
install.packages("mgcv") #for GAM

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

df <- TBdata

df
head(df)
summary(df)

#-----Visualising Data------
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")


#-----Making first GAM-----

model1 <- gam(TB~s(Density, by=Region, k=4, bs="cs")+
                 s(Population, by=Region, k=4, bs="cs"),
              data=TBdata)
summary(model1)

par(mfrow=(c(1,2)))   # add number of subplots for number of smooth functions
plot.gam(model1)

?gam.models
?gam


#-----Understanding Some Relationships in the Data-----
par(mfrow=c(1,1))
plot(df$Timeliness, df$TB)   # simple 2d scatter plot to see a variable against TB
# scatter plot with TB as colour scale to see about potential interaction terms
ggplot(df, aes(x=lon, y=lat, color=TB)) +
  geom_point() +
  #xlim(c(21000, 21025)) +
  scale_color_gradient(low = "blue", high = "red", limits=c(0, 1000)) +   # set limits to stop outliers skewing the scale
  theme_bw() + 
  labs(color = 'TB Cases', title = 'Potential Interaction Terms')

#-----New Models-----
TBdf <- df
head(TBdf, 5)
TBdf$Region <- factor(TBdf$Region)  # make region a factor

model2 <- gam(TB ~ Population + s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=50, bs='cs') + 
                s(Poverty, k=50, bs='cs'), #+ s(Indigenous, k=5, bs='cs'), #+
                #s(Density, k=5, bs='cs') + s(Poor_Sanitation, k=5, bs='cs'), 
                family=poisson(link='log'), data=TBdata)
              #+
                #s(Density, by=Poverty, k=20, bs='cs') + s(Poor_Sanitation, by=Poverty, k=20, bs='cs')
summary(model2)
par(mfrow=c(2,2))
gam.check(model2) # residual plots and additional summary info
par(mfrow=c(3,2))
plot(model2)

model2 <- gam(TB ~ Population + s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=50, bs='cs') + 
                s(Poverty, k=50, bs='cs'), #+ s(Indigenous, k=5, bs='cs'), #+
              #s(Density, k=5, bs='cs') + s(Poor_Sanitation, k=5, bs='cs'), 
              family=poisson(link='log'), data=TBdata)

summary(model2)
par(mfrow=c(2,2))
gam.check(model2) # residual plots and additional summary info
par(mfrow=c(3,2))
plot(model2)

# population acts as offset I think as we are looking for rate of TB
model3 <- gam(TB ~ s(Timeliness, k=100, bs='cs') + s(Urbanisation, k=100, bs='cs') + 
                s(Poverty, k=100, bs='cs') + offset(log(Population)), family=poisson(link='log'), data=TBdata)
summary(model3)
par(mfrow=c(2,2))
gam.check(model3) # residual plots and additional summary info
par(mfrow=c(2,2))
plot(model3)

# population acts as offset I think as we are looking for rate of TB
model4 <- gam(TB ~ s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=50, bs='cs') + 
                s(Poverty, k=50, bs='cs') + s(Density, k=50, bs='cs') +
                s(lat, lon, k=50, bs='tp') + s(Unemployment, k=50, bs='cs') +
                s(Poor_Sanitation, k=50, bs='cs') + s(Indigenous, k=50, bs='cs') +
                s(Illiteracy, k=50, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)
summary(model4)
par(mfrow=c(2,2))
gam.check(model4) # residual plots and additional summary info
par(mfrow=c(3,3))
plot(model4)

# best model so far without training and testing data split
model5 <- gam(TB ~ s(Timeliness, k=60, bs='cs') + s(Urbanisation, k=66, bs='cs') + 
                s(Poverty, k=70, bs='cs') + s(Density, k=62, bs='cs') +
                s(lat, lon, k=65, bs='tp') + s(Unemployment, k=67, bs='cs') +
                s(Poor_Sanitation, k=68, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)
summary(model5)
par(mfrow=c(2,2))
gam.check(model5) # residual plots and additional summary info
par(mfrow=c(3,3))
plot(model5)

# plotting TB risk and comparing to TB cases
risk_preds = predict(model5, type='response') # changing this to 'link' is possibly right? think its response tho
TBdf$TBrisk = risk_preds

par(mfrow=c(1,2))
plot.map(TBdf$TBrisk[TBdf$Year==2014],n.levels=7,main="TB Risk for 2014")
plot.map(TBdf$TB[TBdf$Year==2014],n.levels=7,main="TB Counts for 2014")


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
risk_preds = predict(model6, newdata=test_df, type='response')  # think this returns rate and that TB in data is rate but need to confirm
test_df$TBrisk = risk_preds

par(mfrow=c(1,2))
plot.map(test_df$TBrisk[test_df$Year==2014],n.levels=7,main="Predicted TB Risk for 2014")
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB Counts for 2014")

