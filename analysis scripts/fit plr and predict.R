# Title: Fit & predict pairwise logistic regression model
# 
# Description:
#
# Requirement:
#   Packages: lr2cluster, dplyr
#


library(lr2cluster)
library(dplyr)


## ***** Load TB Valencia data *****
## *********************************
source("./analysis scripts/tb_valencia.R")
clus_dt <- dt %>% filter(Cluster != "unique")
#



## ***** Split the data into training vs testing sets *****
## ********************************************************
set.seed(123)
id <- caret::createDataPartition(clus_dt$Cluster, 1, .6, F)

traindt <- clus_dt[id,]
testdt <- clus_dt[-id,]

mod1 <- Cluster ~ Latitude+Longitude+Gender+Diabetes+HIV+Foreign
mod2 <- Cluster ~ Latitude+Longitude+Foreign
fit1 <- plr(mod1, clus_dt) 
fit2 <- plr(mod2, clus_dt)
summary(fit1); summary(fit2) #this is the results in Table 3
#




