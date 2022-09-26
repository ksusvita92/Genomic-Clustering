# Title: Choose cases to be sequenced next (task in Subsection 2.2.3)
# 
# Description:
#   Here, we choose 5 cases for each cluster which give the highest score of getting
#   assigned into a particular cluster.
#
# Requirement:
#   Packages: caret (optional)
#



library(lr2cluster)
library(dplyr)
library(doSNOW)
library(foreach)
library(tcltk)




## ****** Configurations *********
## *******************************
K <- 5 
method <- c("plr", "mlr") 

dt <- tb_valencia %>% filter(tr_cl != "unique") 
load("./analysis scripts/id.RData") 

mod <- tr_cl ~ latitude+longitude+foreign
cl_col_nm <- "tr_cl"
cs_col_nm <- "id_server"




## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallseq <- function(model, nbest, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_col_nm)
  test_cl <- newdata %>% pull(cl_col_nm)
  test_cs <- newdata %>% pull(cs_col_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- clusterPLR(model, data, newdata, nbest = 1)
    else if(m == "mlr") assgn. <- clusterMLR(model, data, newdata, nbest = 1)
    else if(m == "random") assgn. <- clusterRandom(cl_col, nrow(newdata), nbest = 1)
    
    cseq <- case2sequence(assgn., test_cs, nbest = nbest)
    accu. <- acc(cseq, test_cl) %>% mutate(method = m)
    res <- bind_rows(res, accu.)
  }
  
  res
}
#



## ****** Parallel configuration ******
## ************************************
pb <- tkProgressBar(paste("Case to seq. accuracy"), "Progress...", 0, length(id), 0)
progress <- function(n){
  info <- sprintf("%1.0f%% done", n/length(id)*100)
  setTkProgressBar(pb, n, paste("Case to seq. accuracy"), info)
} 
#

# Perform parallel computation
cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

myacc <- foreach(i = 1:length(id), .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress)) %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  
  getallseq(mod1, K, method, traindt, testdt)
}

close(pb)
stopCluster(cl)
#  

# save the output
write.csv(myacc, file = "case_seq_acc.csv", row.names = F)




