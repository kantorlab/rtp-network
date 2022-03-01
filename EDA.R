rm(list=ls())


# Load libraries ---------------------------

library(dplyr)


# Set wd for generated output ---------------------------

setwd("./out")
#load("EDA.RData") #When mounting using SMB, uncomment

# Read data ---------------------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
list.files(path=data_dir)
net_dt <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))

## when mounting using SMB, comment above three lines
          
dim(net_dt)
str(net_dt)
View(net_dt)

table(net_dt$InterviewRank, exclude = NULL)


# Filter by rank 1 ---------------------------

net_dt_r1 <- net_dt %>% filter(InterviewRank == 1)

dim(net_dt_r1)
str(net_dt_r1)
View(net_dt_r1)

table(net_dt_r1$InterviewRank, exclude = NULL)
table(net_dt_r1$InterviewRank, exclude = NULL)/sum(table(net_dt_r1$InterviewRank, 
                                                         exclude = NULL))

table(net_dt_r1$ClientNotReached, exclude = NULL)
table(net_dt_r1$ClientNotReached, exclude = NULL)/sum(
  table(net_dt_r1$ClientNotReached, exclude = NULL)
)

table(net_dt_r1$ClientReached, exclude = NULL)
table(net_dt_r1$ClientReached, exclude = NULL)/sum(table(net_dt_r1$ClientReached, 
                                                         exclude = NULL))


table(net_dt_r1$AlreadyIndexCase, exclude = NULL)
table(net_dt_r1$AlreadyIndexCase, exclude = NULL)/sum(table(net_dt_r1$AlreadyIndexCase, exclude = NULL))


#Save Object ---------------------------

save.image(file="EDA.RData")