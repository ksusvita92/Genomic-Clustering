# Title: Plot Figure 6
# 
# Description: Reproduce Fig 6
#
# Requirement:
#   Packages: ggplot, dplyr, patchwork
#


library(ggplot2)
library(dplyr)
library(patchwork)

dt <- read.csv("./results/case_seq_acc.csv")
dt$method <- factor(dt$method, levels = c("plr","mlr"))
cl <- unique(dt$cluster)
dt1 <- dt %>% filter(cluster %in% cl[1:19])
dt2 <- dt %>% filter(cluster %in% cl[20:38])



## ******* Create errorbar *******
## *******************************
pl1 <- dt1 %>% 
  group_by(cluster, method) %>% 
  summarise(mean = mean(accuracy), sd = sd(accuracy)) %>%
  ungroup() %>%
  ggplot() +
  geom_errorbar(aes(x = cluster, ymin = ifelse(mean-sd<0,0,mean-sd), ymax = ifelse(mean+sd>1, 1, mean+sd), col = method), width = .2, position = position_dodge(.9)) +
  geom_point(aes(x = cluster, y = mean, col = method), position = position_dodge(.9)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = ), legend.position = "top") +
  labs(x = "", y = "accuracy")

pl2 <- dt2 %>% 
  group_by(cluster, method) %>% 
  summarise(mean = mean(accuracy), sd = sd(accuracy)) %>%
  ungroup() %>%
  ggplot() +
  geom_errorbar(aes(x = cluster, ymin = ifelse(mean-sd<0,0,mean-sd), ymax = ifelse(mean+sd>1, 1, mean+sd), col = method), width = .2, position = position_dodge(.9)) +
  geom_point(aes(x = cluster, y = mean, col = method), position = position_dodge(.9)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = ), legend.position = "none") +
  labs(x = "", y = "accuracy")

#

pl1/pl2
