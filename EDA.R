rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)


# Setup ---------------------------

setwd("/Volumes/home/akhann16/code/rtp-network/out") #on smb
#setwd("/gpfs/home/akhann16/code/rtp-network/out") #on remote desktop 
load("EDA.RData") #When mounting using SMB, uncomment
par(mar=c(1,1,1,1)) #for figures
ls()

### 

# Read data ---------------------------

## Comment below lines on SMB
## Uncomment if needed on remote desktop

# data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
# list.files(path=data_dir)
# net_dt <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))
# individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))

#-------------------
## when mounting using SMB, comment the above lines
#-------------------

dim(net_dt)
str(net_dt)
sort(colnames(net_dt))
View(net_dt)

dim(individuals_dt)
colnames(individuals_dt)

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
head(net_r1 %v% "vertex.names")

plot(net)
plot(net_r1)


#Examine network datasets ---------------------------

length(which(net_dt_mat[,1] %in% net_dt_mat[,2]))
length(which(net_dt_mat[,2] %in% net_dt_mat[,1]))
length(intersect(net_dt_mat[,1], net_dt_mat[,2]))

length(which(net_dt_r1_mat[,1] %in% net_dt_r1_mat[,2]))
length(which(net_dt_r1_mat[,2] %in% net_dt_r1_mat[,1]))
length(intersect(net_dt_r1_mat[,1], net_dt_r1_mat[,2]))

length(which(unique(net_dt_mat[,1]) %in% (unique(net %v% "vertex.names"))))
#length(which(unique(net_dt_r2_mat[,1]) %in% (unique(net_r2 %v% "vertex.names"))))


# Measure network characteristics ---------------------------

options(max.print = 5000)

## size of the contact tracing network?
network.size(net) #nodes
network.edgecount(net) #edgecount

vnames <- (net %v% "vertex.names")
vnames 
table(substr(vnames, 1, 5)); table(substr(vnames, 1, 5))/sum(table(substr(vnames, 1, 5)))
table(substr(vnames, 1, 8))

## unique sources and receivers in the contact tracing network?
length(net_dt$StudyIDFrom); length(unique(net_dt$StudyIDFrom))
length(net_dt$StudyIDTo); length(unique(net_dt$StudyIDTo))


## msm
table(individuals_dt$RiskMSM, exclude = NULL); nrow(individuals_dt)
msm_individuals <- which(individuals_dt$RiskMSM == "True")
msm_ids <- individuals_dt$StudyID[msm_individuals]
length(msm_individuals); length(msm_individuals)/nrow(individuals_dt)

## IDU
table(individuals_dt$RiskIDU, exclude = NULL); nrow(individuals_dt)
IDU_individuals <- which(individuals_dt$RiskIDU == "True")
idu_ids <- individuals_dt$StudyID[IDU_individuals]
length(IDU_individuals); length(IDU_individuals)/nrow(individuals_dt)

## HRH
table(individuals_dt$RiskHRH, exclude = NULL); nrow(individuals_dt)
HRH_individuals <- which(individuals_dt$RiskHRH == "True")
HRH_ids <- individuals_dt$StudyID[HRH_individuals]
length(HRH_individuals); length(HRH_individuals)/nrow(individuals_dt)

msm_idu <- (intersect(msm_ids, idu_ids))
idu_hrh <- (intersect(idu_ids, HRH_ids))
msm_hrh <- (intersect(msm_ids, HRH_ids))

intersect3 <- function(a, b, c){
  a_b <- intersect(a, b)
  a_b_c <- intersect(a_b, c)
  return(a_b_c)
}

intersect3(msm_ids, idu_ids, HRH_ids)

## how many nodes from individual dataset appear in the network?
length(which(vnames %in% individuals_dt$StudyID))

## msm network
msm_ids_in_net <- which(vnames %in% msm_ids)
idu_ids_in_net <- which(vnames %in% idu_ids)
hrh_ids_in_net <- which(vnames %in% HRH_ids)

length(msm_ids_in_net)
length(idu_ids_in_net)
length(hrh_ids_in_net)

## add behaviors as attributes to networks
set.vertex.attribute(x=net, attrname = "behavior", value = "msm", v=msm_ids_in_net)
set.vertex.attribute(x=net, attrname = "behavior", value = "idu", v=idu_ids_in_net)
set.vertex.attribute(x=net, attrname = "behavior", value = "hrh", v=hrh_ids_in_net)

table(net %v% "behavior", exclude = NULL)
mixingmatrix(net, "behavior")

table(net %v% "behavior", exclude = NULL)/sum(table(net %v% "behavior", exclude = NULL))

png("net_by_behavior.png")
plot(net, vertex.col="behavior")
dev.off()

#Save Object ---------------------------

save.image(file="EDA.RData")

