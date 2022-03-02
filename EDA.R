rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)


# Set wd for generated output ---------------------------

setwd("./out")

#-------------------
load("EDA.RData") #When mounting using SMB, uncomment
#-------------------


# Read data ---------------------------


#data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
#list.files(path=data_dir)
#net_dt <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))

#-------------------
## when mounting using SMB, comment above three lines
#-------------------

dim(net_dt)
str(net_dt)
sort(colnames(net_dt))
View(net_dt)

table(net_dt$InterviewRank, exclude = NULL)
table(net_dt$InterviewRank, exclude = NULL)/
  sum(table(net_dt$InterviewRank, exclude = NULL))


# Filter by rank 1 ---------------------------

net_dt_r1 <- net_dt %>% filter(InterviewRank == 1)

dim(net_dt_r1)
str(net_dt_r1)
View(net_dt_r1)

table(net_dt_r1$InterviewRank, exclude = NULL)
table(net_dt_r1$InterviewRank, exclude = NULL)/sum(table(net_dt_r1$InterviewRank, 
                                                         exclude = NULL))

## Client no reach (includes missing)
table(net_dt$ClientNotReached, exclude = NULL)
table(net_dt$ClientNotReached, exclude = NULL)/sum(
  table(net_dt$ClientNotReached, exclude = NULL)
)

table(net_dt_r1$ClientNotReached, exclude = NULL)
table(net_dt_r1$ClientNotReached, exclude = NULL)/sum(
  table(net_dt_r1$ClientNotReached, exclude = NULL)
)


## Client Reached
table(net_dt$ClientReached, exclude = NULL)
table(net_dt$ClientReached, exclude = NULL)/sum(table(net_dt$ClientReached, 
                                                         exclude = NULL))
table(net_dt_r1$ClientReached, exclude = NULL)
table(net_dt_r1$ClientReached, exclude = NULL)/sum(table(net_dt_r1$ClientReached, 
                                                         exclude = NULL))

## Already Index Case
table(net_dt$AlreadyIndexCase, exclude = NULL)
table(net_dt$AlreadyIndexCase, exclude = NULL)/sum(table(net_dt$AlreadyIndexCase, exclude = NULL))

table(net_dt_r1$AlreadyIndexCase, exclude = NULL)
table(net_dt_r1$AlreadyIndexCase, exclude = NULL)/sum(table(net_dt_r1$AlreadyIndexCase, exclude = NULL))


## Interview Type
table(net_dt$InterviewType, exclude = NULL)
table(net_dt$InterviewType, exclude = NULL)/sum(table(net_dt$InterviewType, exclude = NULL))

table(net_dt_r1$InterviewType, exclude = NULL)
table(net_dt_r1$InterviewType, exclude = NULL)/sum(table(net_dt_r1$InterviewType, exclude = NULL))

#Create network object ---------------------------
net_dt_mat <- as.matrix.data.frame(cbind(net_dt[,1], net_dt[,2]))
net <- as.network(net_dt_mat, bipartite = F, directed = T)
net
length(net %v% "vertex.names")

net_dt_r1_mat <- as.matrix.data.frame(cbind(net_dt_r1[,1], net_dt_r1[,2]))
net_r1 <- as.network(net_dt_r1_mat, bipartite = F, directed = T)
net_r1
length(net_r1 %v% "vertex.names")

plot(net)
plot(net_r1)


length(which(net_dt_mat[,1] %in% net_dt_mat[,2]))
length(which(net_dt_mat[,2] %in% net_dt_mat[,1]))
length(intersect(net_dt_mat[,1], net_dt_mat[,2]))

length(which(net_dt_r1_mat[,1] %in% net_dt_r1_mat[,2]))
length(which(net_dt_r1_mat[,2] %in% net_dt_r1_mat[,1]))
length(intersect(net_dt_r1_mat[,1], net_dt_r1_mat[,2]))

length(which(unique(net_dt_mat[,1]) %in% (unique(net %v% "vertex.names"))))
length(which(unique(net_dt_r2_mat[,1]) %in% (unique(net_r2 %v% "vertex.names"))))

length(which(unique(net_dt_r1_mat[,2]) %in% (unique(net_r2 %v% "vertex.names"))))
length(which(unique(net_dt_r1_mat[,2]) %in% (unique(net_r1 %v% "vertex.names"))))

#Save Object ---------------------------

save.image(file="EDA.RData")

