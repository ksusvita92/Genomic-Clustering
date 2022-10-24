# Title: Figure S1
# 
# Description:
#   Reproduce Figure S1
#
# Requirement:
#   Packages: dplyr, ggplot, patchwork
#

library(ggplot2)
library(dplyr)
library(patchwork)


dt <- read.csv("./results/exp_cluster_size.csv") %>% filter(rho == 0)


plcl021 <- dt %>% filter(cluster == "CL021", method == "plr") %>%
  ggplot() + geom_histogram(aes(x = size, y = ..density..), col = "black", fill = "white") +
  geom_vline(aes(xintercept = mean(size)), col = "red", lty = "dashed") +
  labs(title = "CL021", x = "", y = "density") +
  theme_light()

plcl063 <- dt %>% filter(cluster == "CL063", method == "plr") %>%
  ggplot() + geom_histogram(aes(x = size, y = ..density..), col = "black", fill = "white") +
  geom_vline(aes(xintercept = mean(size)), col = "red", lty = "dashed") +
  labs(title = "CL063", x = "", y = "") +
  theme_light()

plcl021 + plcl063
