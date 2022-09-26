# Title: Plot Figure 6
# 
# Description:
#
# Requirement:
#   Packages: ggplot, dplyr
#


library(ggplot2)
library(dplyr)

dt <- read.csv("./results/case_seq_acc.csv")



## ******* Create errorbar *******
## *******************************
dt %>% 
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
