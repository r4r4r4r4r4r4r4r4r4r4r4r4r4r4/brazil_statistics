#MAIN FILE FOR WORKING

#import dependencies
library(ggplot2)
library(tidyverse)

#get filepath
cwd <- getwd()

load("datasets_project.RData")

df <- TBdata
head(df)
