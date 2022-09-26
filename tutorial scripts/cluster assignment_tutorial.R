# Compute the accuracy of the best K clusters
#
# Description:
#   This script is to predict cluster assignment of cases using pairwise LR,
#   multinomial LR, and random assignment; and then compute their accuracy.
#   We first split the data to training and testing sets for N times.
#   Then, we compute the average accuracy and its standard deviation.
#
# Requirement:
#   Packages: lr2cluster, dplyr, ggplot2, caret
#
# Note: result shows in Table 4



library(lr2cluster)
library(dplyr)
library(ggplot2)




## ****** Configurations *********
## *******************************
K <- c(1,3,5) #no. of best cluster to choose
method <- c("plr", "mlr") #add "random" if you want to use random assignment
N <- 10 #how many times to split the data
p <- .6 #approx. proportion of splitting data to train vs test set

dt <- tb_valencia %>% filter(tr_cl != "unique") #change this with your data
cl_nm <- "tr_cl" #cluster's column name

mod <- tr_cl ~ latitude+longitude+foreign #formula for your model
  #NOTE: if spatial distance is one of the predictor, you have to name the geo
  #locatin to latitude (or lat) and longitude (or long).
## *************




## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallacc <- function(model, nbest, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_nm)
  test_cl <- newdata %>% pull(cl_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- lapply(nbest, function(x) clusterPLR(model, data, newdata, nbest = x))
    else if(m == "mlr") assgn. <- lapply(nbest, function(x) clusterMLR(model, data, newdata, nbest = x))
    else if(m == "random") assgn. <- lapply(nbest, function(x) clusterRandom(cl_col, nrow(newdata), nbest = x))
    
    accu. <- sapply(assgn., function(x) acc(x, test_cl))
    tmp <- data.frame(K = paste("best", nbest), method = m, acc = accu., msg = NA)
    res <- bind_rows(res, tmp)
  }
  
  res
}
#




## ****** Parallel configuration ******
## ************************************
pb <- tkProgressBar(paste("Cluster assgn. accuracy"), "Progress...", 0, length(id), 0)
progress <- function(n){
  info <- sprintf("%1.0f%% done", n/length(id)*100)
  setTkProgressBar(pb, n, paste("Cluster assgn. accuracy"), info)
} 
#

# Perform parallel computation
id <- caret::createDataPartition(dt[,cl_nm], N, p)
cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

myacc <- foreach(i = 1:N, .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress), .errorhandling = "pass") %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  
  tryCatch(getallacc(mod, K, method, traindt, testdt), 
           error = function(e){
             data.frame(K = NA, method = NA, acc = NA, msg = conditionMessage(e))
           })
  
}

close(pb)
stopCluster(cl)


head(myacc)

# save the output
#write.csv(myacc, file = "cluster_assgn_acc.csv", row.names = F)
#



## ******* Create boxplot *******
## ******************************
myacc %>% ggplot() +
  geom_boxplot(aes(x = K, y = acc, fill = method)) +
  labs(x = "", y = "accuracy") +
  theme_light()

