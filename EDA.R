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

# How many nodes from individual dataset appear in the network? ---------------------------

length(which(vnames %in% individuals_dt$StudyID)) ## IMPORTANT


# For how many of individuals in contact network do we have sequence information ---------------------------

table(individuals_dt$Sequence) # Sequences available for 2308 individuals

StudyIDs_of_Seq_Individuals <- individuals_dt$StudyID[(which(individuals_dt$Sequence == "True"))]
StudyIDs_of_Seq_Individuals_in_ContactNetwork <- which(StudyIDs_of_Seq_Individuals %in% vnames)

length(StudyIDs_of_Seq_Individuals_in_ContactNetwork)


# How many unique clusters are the individuals in? ---------------------------

unique(individuals_dt$ClusteredHIVTrace005) #161 unique clusters
unique(individuals_dt$ClusteredHIVTrace015) #256 unique clusters


# How many unique clusters have only one individual? ---------------------------

length(which(table(individuals_dt$ClusteredHIVTrace005) == 1)) #34 clusters only have 1 member
length(which(table(individuals_dt$ClusteredHIVTrace015) == 1)) #35 clusters only have 1 member


# Which specific clusters have at least 2 individuals?
HIVTRACE005_ge2_members <- which(table(individuals_dt$ClusteredHIVTrace005) >= 2) 
HIVTRACE015_ge2_members <- which(table(individuals_dt$ClusteredHIVTrace005) >= 2)

# For individuals in each of these defined clusters, how many are in the named list?

named_pt_dt <- cbind(net_dt$StudyIDFrom, net_dt$StudyIDTo)

## Take specific examples

### HIV TRACE 100
HIVTRACE_100_005 <- which(individuals_dt$ClusteredHIVTrace005 == "HIVTRACE_100") # 2 indidviduals in cluster HIVTRACE_100
studyids_HIVTRACE_100_005 <- individuals_dt$StudyID[HIVTRACE_100_005] #Study IDs of these 2 individuals

studyids_HIVTRACE_100_005  %in% net_dt$StudyIDFrom 
  #Both appear in the STUDYIDFROM col in named partner data set 
studyids_HIVTRACE_100_005  %in% net_dt$StudyIDTo 
  #Only the second appears in the STUDYIDTO column

identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_HIVTRACE_100_005)
identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_HIVTRACE_100_005)

intersect(identify_p1_rows, identify_p2_rows) #1 such occurence out of max of 2

named_pt_dt[identify_p1_rows,]
named_pt_dt[identify_p2_rows,]

### CONCLUSION: "S15005676050" names "S20000532945" but not vice versa. 

### HIV TRACE 59
HIVTRACE_59_005 <- which(individuals_dt$ClusteredHIVTrace005 == "HIVTRACE_59") 
  # 9 indidviduals in cluster HIVTRACE_59
studyids_HIVTRACE_59_005 <- individuals_dt$StudyID[HIVTRACE_59_005] 
  #Study IDs of these 9 individuals

studyids_HIVTRACE_59_005  %in% net_dt$StudyIDFrom 
  #All 9 IDs appear in the STUDYIDFROM col in named partner data set 
studyids_HIVTRACE_59_005  %in% net_dt$StudyIDTo 
  #Five STUDY IDs appear in the STUDYIDTO column

identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_HIVTRACE_59_005)
identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_HIVTRACE_59_005)

intersect(identify_p1_rows, identify_p2_rows) #2 such occurences out of max of 9 Choose 2 = 36??

named_pt_dt[identify_p1_rows,]
named_pt_dt[identify_p2_rows,]


### CONCLUSION: Two seem to name each other


# Automate process for assessing if persons in transmission network are named partners?

## Step 1: List all clusters with >2 persons
## Step 2: Identify persons in each such cluster
## Step 3: Note STUDYIDFROM and STUDYIDTO where such persons appear
## Step 4: Identify pairs where STUDYIDFROM and STUDYIDTO are both in this list

class(HIVTRACE005_ge2_members)
names(HIVTRACE005_ge2_members)


calc_num_in_named_partners <- 
  function(distance="005"){
    
    named_pt_num_list <- NULL
    
    #for (i in 2:length(names(HIVTRACE005_ge2_members))){
       (i=names(HIVTRACE005_ge2_members)[4]){  
        label_col <- paste0("ClusteredHIVTrace", distance)
        
        #cluster <- names(HIVTRACE005_ge2_members)[i]
        cluster_distance <- which(individuals_dt[[label_col]] == i) 
        studyids_cluster_distance <- individuals_dt$StudyID[cluster_distance] 
        
        identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_cluster_distance)
        identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_cluster_distance)
        
        named_pt_num <- intersect(identify_p1_rows, identify_p2_rows)       
        named_pt_num_list <- c(named_pt_num_list, length(named_pt_num))
        
    }
    
    return(named_pt_num_list)
  }
calc_num_in_named_partners()

#Save Object ---------------------------

save.image(file="EDA.RData")

