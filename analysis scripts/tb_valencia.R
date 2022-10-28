# Title: Prepare the TB data
# 
# Description:
#   This script is to download the raw data from Munoz et. al's paper and subset some 
#   necessary columns.
#
# Note:
#   We alter and jitter the location data, but maintain the spatial distance
#   to protect patients' privacy. The resulting 
#   analysis may be slightly different compared to the paper.
#
# Requirement:
#   Packages: readxl, dplyr
#


library(readxl)
library(dplyr)


url <- "https://elifesciences.org/download/aHR0cHM6Ly9jZG4uZWxpZmVzY2llbmNlcy5vcmcvYXJ0aWNsZXMvNzY2MDUvZWxpZmUtNzY2MDUtc3VwcDEtdjEueGxzeA--/elife-76605-supp1-v1.xlsx?_hash=pzQwKD1DzDLre7kKrWI%2Fhd%2BjY2FGgpekrPI4vXrlWNo%3D"
destfile <- "rawdt.xlsx"
curl::curl_download(url, destfile)
rawdt <- read_excel(destfile, range = "A3:AB778", na = "NA")


loc <- read.csv("./analysis scripts/location_data.csv")

# subset the necessary column and cases
dt <- rawdt %>% transmute(ID = ...1, Cluster = `Genomic
Cluster ID`, Gender = Gender, Foreign = ifelse(`Country of birth`=="SPAIN", "No", "YES"), Diabetes = Diabetes, HIV = `HIV infected`) %>%
  filter(!is.na(Gender), !is.na(Foreign), !is.na(Diabetes), !is.na(HIV)) %>%
  inner_join(loc, by = c("ID"))

nm <- as.data.frame(table(dt$Cluster)) %>% filter(Freq > 2) %>% pull(Var1)
dt <- dt %>% filter(Cluster %in% nm)

rm(loc, destfile, url, rawdt)
