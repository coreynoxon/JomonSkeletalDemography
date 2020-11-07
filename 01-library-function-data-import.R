#001-libraries, functions, and data import

library("tidyverse")
library("ggthemes")
library("ggpubr")

#read in data

#import posthole and pithouse data
posts<- read_csv("data/raw-data/Tokyo PithouseDB V3.2 - PostHole.csv", na = c("#VALUE!", "#N/A"))
pits<- read_csv("data/raw-data/Tokyo PithouseDB V3.2 - Pithouse.csv", na = c("#VALUE!", "#N/A"))

#import skeletal data
skeletalDF <- read_csv("data/raw-data/MA Skeletal Data - Region 4 trim.csv", na = c("#VALUE!", "#N/A", "uk"))