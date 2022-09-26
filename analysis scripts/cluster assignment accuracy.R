# Title: Find best clusters for new cases (task in Subsubsection 2.2.2)
# 
# Description:
#   We predict the best-1/3/5 clusters a new case can be assigned to based
#   on some covariates.
#
# Requirement:
#   Packages: lr2cluster, dplyr, doSNOW, foreach, tcltk
#



library(lr2cluster)
library(dplyr)
library(doSNOW)
library(foreach)
library(tcltk)




## ****** Configurations *********
## *******************************
K <- c(1,3,5) #no. of best clusters to predict
method <- c("plr", "mlr", "random")

dt <- tb_valencia %>% filter(tr_cl != "unique")
load("./analysis scripts/id.RData")

mod1 <- tr_cl ~ latitude+longitude+sex+diabetes+hiv+foreign
mod2 <- tr_cl ~ latitude+longitude+foreign
cl_col_nm <- "tr_cl"




## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallacc <- function(model, nbest, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_col_nm)
  test_cl <- newdata %>% pull(cl_col_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- lapply(nbest, function(x) clusterPLR(model, data, newdata, nbest = x))
    else if(m == "mlr") assgn. <- lapply(nbest, function(x) clusterMLR(model, data, newdata, nbest = x))
    else if(m == "random") assgn. <- lapply(nbest, function(x) clusterRandom(cl_col, nrow(newdata), nbest = x))
    
    accu. <- sapply(assgn., function(x) acc(x, test_cl))
    tmp <- data.frame(K = paste("best", nbest), method = m, acc = accu.)
    res <- bind_rows(res, tmp)
  }
  
  res
}
#



## ****** Parallel configuration ******
## ************************************
pb <- tkProgressBar(paste("Cluster assign. accuracy"), "Progress...", 0, length(id), 0)
progress <- function(n){
  info <- sprintf("%1.0f%% done", n/length(id)*100)
  setTkProgressBar(pb, n, paste("Cluster assign. accuracy"), info)
} 
#

# Perform parallel computation
cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

myacc <- foreach(i = 1:length(id), .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress)) %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  
  tmp1 <- getallacc(mod1, K, method, traindt, testdt) %>% mutate(model = ifelse(method == "random", "random", "model 1"))
  tmp2 <- getallacc(mod2, K, method, traindt, testdt) %>% mutate(model = ifelse(method == "random", "random", "model 2"))
  bind_rows(tmp1, tmp2)
}

close(pb)
stopCluster(cl)
#  

# save the output
#write.csv(myacc, file = "cluster_assgn_acc.csv", row.names = F)
#  
  
  
  
  