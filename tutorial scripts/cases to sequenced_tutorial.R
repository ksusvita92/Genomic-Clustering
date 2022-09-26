# Title: Choose cases to be sequenced next (task in Subsection 2.2.3)
# 
# Description:
#   Here, we choose 5 cases for each cluster which give the highest score of getting
#   assigned into a particular cluster.
#
# Requirement:
#   Packages: lr2cluster, dplyr, doSNOW, foreach, tcltk, ggplot2, caret



library(lr2cluster)
library(dplyr)
library(doSNOW)
library(foreach)
library(tcltk)
library(ggplot2)




## ****** Configurations *********
## *******************************
K <- 5 #no. of best cases to choose
method <- c("plr", "mlr") #add "random" if you want to use random assignment
N <- 10 #how many times to split the data
p <- .6 #approx. proportion of splitting data to train vs test set

dt <- tb_valencia %>% filter(tr_cl != "unique") #change this with your data
cl_nm <- "tr_cl" #cluster's column name
cs_nm <- "id_server" #case's column name

mod <- tr_cl ~ latitude+longitude+foreign #formula for your model
  #NOTE: if spatial distance is one of the predictor, you have to name the geo
  #locatin to latitude (or lat) and longitude (or long).
## *************




## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallseq <- function(model, nbest, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_nm)
  test_cl <- newdata %>% pull(cl_nm)
  test_cs <- newdata %>% pull(cs_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- clusterPLR(model, data, newdata, nbest = 1)
    else if(m == "mlr") assgn. <- clusterMLR(model, data, newdata, nbest = 1)
    else if(m == "random") assgn. <- clusterRandom(cl_col, nrow(newdata), nbest = 1)
    
    cseq <- case2sequence(assgn., test_cs, nbest = nbest)
    accu. <- acc(cseq, test_cl) %>% mutate(method = m, msg = NA)
    res <- bind_rows(res, accu.)
  }
  
  res
}
## *******************



## ****** Parallel configuration ******
## ************************************
pb <- tkProgressBar(paste("Case to seq. accuracy"), "Progress...", 0, length(id), 0)
progress <- function(n){
  info <- sprintf("%1.0f%% done", n/length(id)*100)
  setTkProgressBar(pb, n, paste("Case to seq. accuracy"), info)
} 
#

# Perform parallel computation
id <- caret::createDataPartition(dt[,cl_nm], N, p)
cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

myacc <- foreach(i = 1:N, .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress), .errorhandling = "pass") %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  
  tryCatch(getallseq(mod, K, method, traindt, testdt), 
           error = function(e){
             data.frame(cluster = NA, accuracy = NA, method = NA, msg = conditionMessage(e))
           })
  
}

close(pb)
stopCluster(cl)

head(myacc)

# save the output
#write.csv(myacc, file = "case_seq_acc.csv", row.names = F)
#



## ******* Plot the accuracy *******
## *********************************
myacc %>% 
  group_by(cluster, method) %>% 
  summarise(mean = mean(accuracy), sd = sd(accuracy)) %>%
  ungroup() %>%
  ggplot() +
  geom_errorbar(aes(x = cluster, ymin = ifelse(mean-sd<0,0,mean-sd), ymax = ifelse(mean+sd>1, 1, mean+sd), col = method), width = .2, position = position_dodge(.9)) +
  geom_point(aes(x = cluster, y = mean, col = method), position = position_dodge(.9)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = )) +
  labs(x = "", y = "accuracy")
#



