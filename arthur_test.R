library(ggplot2)
library(dplyr)
library(tidyverse)
library(fields)
library(maps)
library(sp)
library(mgcv) #need for GAM
library(sf) #need for geojson
library(geojsonio)
library(gratia)
library(hrbrthemes)
library(geobr)
library(patchwork)

#get filepath
cwd <- getwd()
load("datasets_project.RData")

#TBdata$Region <- factor(TBdata$Region)

TBdata$real_rate <- (TBdata$TB / TBdata$Population)*100000

brasil_outline <- st_read("gadm41_BRA_0.shx")

csv_output <- read.csv("world-administrative-boundaries.csv", sep=";")
brasil_sf <- st_read(csv_output$Geo.Shape)
coordinates <- st_coordinates(brasil_sf)
coordinates_df <- as.data.frame(coordinates)
colnames(coordinates_df) <- c("lon", "lat")
brasil_outline <- subset(coordinates_df,select = c("lon","lat"))


#-----Using the Model with Training and Testing Data to Validate Predictions-----

# splitting the data into training and testing and then comparing 2014 risk vs actual data
train_df <- subset(TBdata, Year %in% c(2012, 2013))
test_df <- subset(TBdata, Year == 2014)

# fitting the model on training data (2012 and 2013)
model6 <- gam(TB ~ s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=65, bs='cs') + 
                s(Poverty, k=65, bs='cs') + s(Density, k=65, bs='cs') +
                s(lon,lat, k=65, bs='tp') + s(Unemployment, k=65, bs='cs') +
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
test_df$pred_error = test_df$real_rate - test_df$pred_risk    #find error - is this a useful metric? 

par(mfrow=c(1,1))
plot(test_df$real_rate,test_df$pred_risk)
plot(model6,select=5,asp=1)
lines(brasil_micro$bbox)



par(mfrow=c(1,2))
plot.map(test_df$pred_risk[test_df$Year==2014],n.levels=1,main="Predicted TB Risk for 2014")
plot.map(test_df$real_rate[test_df$Year==2014],n.levels=4,main="Actual TB Risk for 2014")

#-----MODEL 7 same as model6 but without train test split --------------------------

model7 <- gam(TB ~ s(Timeliness, k=58, bs='cs') + s(Urbanisation, k=68, bs='cs') +
                s(Poverty, k=70, bs='cs') + s(Density, k=63, bs='cs') +
                s(lon,lat, k=68, bs='tp') + s(Unemployment, k=68, bs='cs') +
                s(Poor_Sanitation, k=68, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=TBdata)

par(mfrow=c(2,2))
gam.check(model7) # residual plots and additional summary info

par(mfrow=c(1,1))
lon_lat_smooth <- model7$smooth[[5]]
vis.gam(model7, view = c("lon", "lat"), plot.type = "contour", color = "topo", contour.col = "tan",asp=1, type="link")
plot.gam(model7,select=5,asp=1,scheme=3,xlab="Longitude",ylab="Latitude",main="Spatial Smooth",n=100)

par(mfrow=c(3,3))
plot(model7)

draw(model7, select=5) +
  labs(x="Longitude",y="Latitude")+
  ggtitle("Spatial Smooth Function")+
  theme(plot.title = element_text(hjust = 0.5),axis.line = element_blank())

# predicting risk and plotting against known data
risk_preds = predict(model7, newdata=TBdata, type='link') #this returns log of predicted cases
TBdata$pred_risk = (exp(risk_preds) / TBdata$Population) * 100000   #turn predicted cases into predicted risk (TB cases per 100000)
TBdata$pred_error = TBdata$real_rate - TBdata$pred_risk    #find error - is this a useful metric? 


ggplot()+
  geom_tile(data=latlong,aes(x=lon,y=lat,fill=est))+
  xlim(-75,-35)+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Spatial Smooth Function")+
  scale_fill_gradient2(low = "blue",mid="white", high = "red", name=" ")+
  geom_sf(data = brasil_outline,fill=NA,size=50)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

latlong <- smooth_estimates(model7)
latlong <- subset(latlong, select= c("est","se","lon","lat"))
latlong <- na.omit(latlong)

#plotting on meso region scale
meso_region <- read_meso_region(year=2014)#from geobr
micro_region <- read_micro_region(year=2014)

df <- dplyr::left_join(micro_region,TBdata,by=c("code_micro"="Region"))



smooth_plot <- ggplot()+
  geom_tile(data=latlong,aes(x=lon,y=lat,fill=est))+
  xlim(-75,-35)+
  xlab("")+
  ylab("")+
  scale_fill_gradient2(low = "blue",mid="white", high = "red", name=" ")+
  geom_sf(data = brasil_outline,fill=NA,size=50)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

pred_plot <- ggplot()+
  geom_sf(data=df,aes(fill=pred_risk),size=0.25)+
  xlim(-75,-35)+
  labs(fill=" ")+
  scale_fill_viridis_c()+
  theme_minimal()

(smooth_plot+pred_plot)+plot_layout(ncol=2)


