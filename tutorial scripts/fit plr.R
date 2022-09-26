# Title: Fit & predict pairwise logistic regression
# 
# Description:
#
# Requirement:
#   Packages: lr2cluster, dplyr, caret
#


library(lr2cluster)
library(dplyr)


## ****** Configurations *********
## *******************************
p <- .6 #approx. proportion of splitting data to train vs test set
C <- 12 #cost ratio
dt <- tb_valencia %>% filter(tr_cl != "unique") #change this with your data
mod <- tr_cl ~ latitude+longitude+foreign #formula for your model
  #NOTE: if spatial distance is one of the predictor, you have to name the geo
  #locatin to latitude (or lat) and longitude (or long).

cl_nm <- "tr_cl" #cluster's column name
cs_nm <- "id_server" #case's column name
## *************




## ***** Split the data into training vs testing sets *****
## ********************************************************
set.seed(123)
id <- caret::createDataPartition(dt[,cl_nm], p = p, list = F)
traindt <- dt[id,]
testdt <- dt[-id,]
## ***********



## ****** Fit & predict ******
## ***************************
fit <- plr(mod, traindt) 
pred <- predict(fit, testdt, case.id = testdt[,cs_nm])
summary(fit)
head(pred)
#



## **** Get optimum threshold *******
## **********************************
true_y <- zCovariate(testdt[,cl_nm], id = testdt[,cs_nm])
pi0 <- optThreshold(true_y$y, pred$y, cost.ratio = C)
plot(pi0)
print(pi0)
#

