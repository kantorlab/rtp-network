rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(utils)
#library(network)

# Setup ---------------------------

setwd("/gpfs/home/akhann16/code/rtp-network/out") #on remote desktop 
par(mar=c(1,1,1,1)) #for figures
ls()

### 


# Read data ---------------------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
list.files(path=data_dir)
individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))


# Create dictionary: cluster id, nodes ---------------------------

clusters <- 
  individuals_dt %>% select(StudyID, ClusteredHIVTrace005, ClusteredHIVTrace015)
head(clusters)

## 005
head(clusters$ClusteredHIVTrace005, 100)
ids_in_005_clusters <- which(substr(clusters$ClusteredHIVTrace005, 1, 3) == "HIV")
length(ids_in_005_clusters)

studyids_in_005_clusters <- individuals_dt$StudyID[ids_in_005_clusters]
length(studyids_in_005_clusters)

clusterIDs_005 <- clusters$ClusteredHIVTrace005[ids_in_005_clusters]
length(clusterIDs_005)

## 015
head(clusters$ClusteredHIVTrace015, 100)
ids_in_015_clusters <- which(substr(clusters$ClusteredHIVTrace015, 1, 3) == "HIV")
length(ids_in_015_clusters)

studyids_in_015_clusters <- individuals_dt$StudyID[ids_in_015_clusters]
length(studyids_in_015_clusters)

clusterIDs_015 <- clusters$ClusteredHIVTrace015[ids_in_015_clusters]
length(clusterIDs_015)


# Create named list with study ids in each cluster ---------------------------
 
## 005
(table(individuals_dt$ClusteredHIVTrace005, exclude = NULL))
cluster_names_005 <- names((table(individuals_dt$ClusteredHIVTrace005, exclude = NULL)))[-1]

study_ids_at_clusterid_005 = as.list(rep(NA, length(cluster_names_005)))
names(study_ids_at_clusterid_005) = cluster_names_005

for (i in 1:length(study_ids_at_clusterid_005)){
  study_ids_at_clusterid_005[[names(study_ids_at_clusterid_005)[i]]] <-  
    (individuals_dt$StudyID[(which(individuals_dt$ClusteredHIVTrace005 == names(study_ids_at_clusterid_005)[[i]]))])
}

study_ids_at_clusterid_005
length(study_ids_at_clusterid_005)

## 015


# Create edgelist ---------------------------



## 005
el_matrix_005 <- NULL

for (i in 1:length(study_ids_at_clusterid_005)){
  if (length(study_ids_at_clusterid_005[[i]]) > 1){
    el_matrix_005 <- rbind(el_matrix_005, t(combn(as.character(study_ids_at_clusterid_005[[i]]), 2)))
  }
}
 
dim(el_matrix_005) 

el_cluster_size <- NULL
for (i in 1:length(study_ids_at_clusterid_005)){
    el_cluster_size <- c(el_cluster_size, unlist(length(study_ids_at_clusterid_005[[i]])))
  }
sort(el_cluster_size)

n_ties <- unlist(lapply(el_cluster_size, function(x) choose(x, 2)))
sum(n_ties)

 ## 015


# Save edgelists ---------------------------
saveRDS(el_matrix_005, "el_matrix_005.RDS")

