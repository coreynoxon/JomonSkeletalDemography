#001-libraries, functions, and data import

library("tidyverse")
library("ggthemes")
library("ggpubr")
library("here")

#read in data
skeletalDF <- read_csv("data/raw-data/MA Skeletal Data - Region 4 trim.csv", na = c("#VALUE!", "#N/A", "uk"))