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
plot(df$Urbanisation, df$TB)   # simple 2d scatter plot to see a variable against TB
# scatter plot with TB as colour scale to see about potential interaction terms
ggplot(df, aes(x=Poverty, y=Density, color=TB)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red", limits=c(0, 1000)) +   # set limits to stop outliers skewing the scale
  theme_bw() + 
  labs(color = 'TB Cases', title = 'Potential Interaction Terms')
  