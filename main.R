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
head(df)

#-----Visualising Data------
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")


#-----Making first GAM-----

model1 <- gam(TB~s(Density, by=Region, k=4, bs="cs")+
                 s(Population, by=Region, k=4, bs="cs"),
              data=TBdata)
summary(model1)

plot.gam(model1)

