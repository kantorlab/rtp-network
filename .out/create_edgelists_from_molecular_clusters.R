rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)

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


# Create edgelist ---------------------------


## 005
(table(individuals_dt$ClusteredHIVTrace005, exclude = NULL))
cluster_names_005 <- names((table(individuals_dt$ClusteredHIVTrace005, exclude = NULL)))[-1]

ids_hivtrace99 <- which(individuals_dt$ClusteredHIVTrace005 == "HIVTRACE_99") #99
list_studyids_at_hivtrace99 <- (individuals_dt$StudyID)[ids_hivtrace99]

ids_hivtrace98 <- which(individuals_dt$ClusteredHIVTrace005 == "HIVTRACE_98") #98
list_studyids_at_hivtrace98 <- (individuals_dt$StudyID)[ids_hivtrace98]

empty_list = as.list(NULL)
empty_list[["HIVTRACE99"]] =  list_studyids_at_hivtrace99
empty_list[["HIVTRACE98"]] =  list_studyids_at_hivtrace98

empty_list

study_ids_at_clusterid_005 = as.list(rep(NA, length(cluster_names_005)))
names(study_ids_at_clusterid_005) = cluster_names_005

for (i in 1:length(study_ids_at_clusterid_005)){
  study_ids_at_clusterid_005[[names(study_ids_at_clusterid_005)[i]]] <-  
    (individuals_dt$StudyID[(which(individuals_dt$ClusteredHIVTrace005 == names(study_ids_at_clusterid_005)[[i]]))])
  #cat("\n")
}

study_ids_at_clusterid_005

## 015



# Save edgelists
