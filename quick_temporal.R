# Loading libraries and dataframe
library(ggplot2)
library(dplyr)
library(tidyverse)
library(fields)
library(maps)
library(sp)
library(mgcv)
library(patchwork)

# Loading DIR and DF
setwd('/Users/matthewfound/Documents/MSc Data Science with AI/Statistical Data Modelling/Group Project')
load('/Users/matthewfound/Documents/MSc Data Science with AI/Statistical Data Modelling/Group Project/datasets_project.RData')

cwd <- getwd()
load("datasets_project.RData")

# Defining TB Risk
df <- TBdata
df$TBR <- (df$TB / df$Population) * 100000
df$Region <- factor(df$Region)  

# FINAL MODEL
model6 <- gam(TB ~ s(Timeliness, k=50, bs='cs') + s(Urbanisation, k=65, bs='cs') + 
                s(Poverty, k=65, bs='cs') + s(Density, k=65, bs='cs') +
                s(lat, lon, k=65, bs='tp') + s(Unemployment, k=65, bs='cs') +
                s(Poor_Sanitation, k=65, bs='cs') +
                offset(log(Population)), family=poisson(link='log'), data=df)

TBR_preds = predict(model6, new_data=df, type='link') #this returns log of predicted cases
df$TBR_preds = (exp(TBR_preds) / df$Population) * 100000

# TEMPORAL ANALYSIS ----

# PLOTTING

# Plotting map of Brazil coloured by TBR in each region for each year
plot.map(df$TBR[df$Year==2012], n.levels=7, main="TB Risk 2012")
plot.map(df$TBR[df$Year==2013], n.levels=7, main="TB Risk 2013")
plot.map(df$TBR[df$Year==2014], n.levels=7, main="TB Risk 2014")

df$Year <- as.factor(df$Year)
# Plot density of TB risk over the years
density_plot <- ggplot(df, aes(x = TBR, fill = Year)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of TB Risk Over the Years",
       x = "TB Risk",
       y = "Density") +
  theme_bw()
print(density_plot)

# Plot TBR by region colored by year as scatter plot
scatter_plot <- ggplot(df, aes(x = Region, y = TBR, color = as.factor(Year))) +
  geom_point() +
  labs(title = "TB Risk by Region Colored by Year",
       x = "Year",
       y = "TB Risk") +
  theme_minimal()
print(scatter_plot)

# Box plot TB Risk by Year 
box_plot <- boxplot(TBR ~ Year, data = df, main = "TB Risk Distribution by Year", xlab = "Year", ylab = "TB Risk")
print(box_plot)

# STATISTICS

# Summary statistics to gain overview of TBR by Year
summary_stats <- df %>% 
  group_by(Year) %>% 
  summarize(
    Mean_TBR = mean(TBR),
    Median_TBR = median(TBR),
    SD_TBR = sd(TBR),
    Min_TBR = min(TBR),
    Max_TBR = max(TBR))
print(summary_stats) # RESULTS ALDO SUGGEST NO SIGNIFICANT CHANGES - FAIRLY CONSTANT MU, STD, MEDIAN ETC...

# ANOVA to test for significant differences in TBR year by year
anova_result <- aov(TBR ~ Year, data = df)
summary(anova_result) # P >> 0.05 - NO SIGNIFICANT DIFFERENCES IN TBR YEAR ON YEAR...

box_plot <- ggplot(df, aes(x = as.factor(Year), y = TBR)) +
  geom_boxplot() +
  labs(title = "TB Risk Distribution by Year",
       x = "Year",
       y = "TB Risk") +
  theme_bw()
box_plot
temporal_plot <- (box_plot + density_plot) + plot_layout(ncol = 2)
temporal_plot
