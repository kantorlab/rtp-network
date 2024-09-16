rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)
library(sna)


# Setup ---------------------------

par(mar=c(1,1,1,1)) #for figures
ls()

### 


# Set seed ---------------------------

set.seed(1234567)


# Read data ---------------------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
net_dt_old <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))
net_dt <- read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv")
individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))


# Preliminary Summaries ---------------------------

dim(net_dt)
str(net_dt)
sort(colnames(net_dt))
#View(net_dt)
#kntir::kable(net_dt)

dim(individuals_dt)
colnames(individuals_dt)

table(net_dt$InterviewRank, exclude = NULL)
table(net_dt$InterviewRank, exclude = NULL)/
  sum(table(net_dt$InterviewRank, exclude = NULL))



# Restrict `individuals_dt` only to persons who are sequenced ---------------------------

table(individuals_dt$Sequence, exclude=NULL)
individuals_dt_sequenced_id <- which(individuals_dt$Sequence == "True")
length(individuals_dt_sequenced_id)
individuals_dt_sequenced <- individuals_dt$StudyID[individuals_dt_sequenced_id]
individuals_dt_sequenced_dt <- individuals_dt[individuals_dt_sequenced_id,]
dim(individuals_dt_sequenced_dt)


individuals_dt <- individuals_dt_sequenced_dt 
## because we want to constrain the analysis only with those who have sequences sequences 



# Restrict `net_dt` only to links where a partner is named ---------------------------

net_dt_non_missing_studyidto <- net_dt[which(net_dt$StudyIDTo != ""), ]
dim(net_dt_non_missing_studyidto)

net_dt <- net_dt_non_missing_studyidto
## because we want to constrain the analysis per the title of the section


# Needed function ---------------------------
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# How many unique clusters are the individuals in? ---------------------------

length(unique(individuals_dt$ClusteredHIVTrace005))
length(unique(individuals_dt$ClusteredHIVTrace015))
length(unique(individuals_dt$ClusteredPhyloAny))

sort(table(individuals_dt$ClusteredHIVTrace005, exclude = NULL)) #cluster sizes at 0.05% distance
sort(table(individuals_dt$ClusteredHIVTrace015, exclude = NULL)) #cluster sizes at 0.15% distance
sort(table(individuals_dt$ClusteredPhyloAny, exclude = NULL)) #ClusteredPhyloAny


# How many unique molecular clusters have only one individual? ---------------------------

length(which(table(individuals_dt$ClusteredHIVTrace005) == 1)) #0
length(which(table(individuals_dt$ClusteredHIVTrace015) == 1)) #0
length(which(table(individuals_dt$ClusteredPhyloAny) == 1)) #1

cluster_sizes_005 <- table(individuals_dt$ClusteredHIVTrace005)
cluster_sizes_015 <- table(individuals_dt$ClusteredHIVTrace015)
cluster_sizes_clusteredphyloany <- table(individuals_dt$ClusteredPhyloAny)

cluster_sizes_005 <- cluster_sizes_005[which(names(table(individuals_dt$ClusteredHIVTrace005)) != "")]
cluster_sizes_015 <- cluster_sizes_015[which(names(table(individuals_dt$ClusteredHIVTrace015)) != "")]
cluster_sizes_clusteredphyloany <- cluster_sizes_clusteredphyloany[which(names(table(individuals_dt$ClusteredPhyloAny)) != "")]
  
summary(as.numeric(cluster_sizes_005[which(as.numeric(cluster_sizes_005) > 1)]))
summary(as.numeric(cluster_sizes_015[which(as.numeric(cluster_sizes_015) > 1)]))
summary(as.numeric(cluster_sizes_clusteredphyloany[which(as.numeric(cluster_sizes_clusteredphyloany) > 1)]))

length(as.numeric(cluster_sizes_005[which(as.numeric(cluster_sizes_005) > 1)]))
length(as.numeric(cluster_sizes_015[which(as.numeric(cluster_sizes_015) > 1)]))
length(as.numeric(cluster_sizes_clusteredphyloany[which(as.numeric(cluster_sizes_clusteredphyloany) > 1)]))

theoretical_max_ties_HIVTRACE005_ge2_members <- 
  lapply(cluster_sizes_005[which(as.numeric(cluster_sizes_005) > 1)], function(x) choose(x, 2)*2)
theoretical_max_ties_HIVTRACE015_ge2_members <- 
  lapply(cluster_sizes_015[which(as.numeric(cluster_sizes_015) > 1)], function(x) choose(x, 2)*2)
theoretical_max_ties_CLUSTEREDPHYLOANY_ge2_members <- 
  lapply(cluster_sizes_clusteredphyloany[which(as.numeric(cluster_sizes_clusteredphyloany) > 1)], function(x) choose(x, 2)*2)

summary(as.numeric(theoretical_max_ties_HIVTRACE005_ge2_members))
summary(as.numeric(theoretical_max_ties_HIVTRACE015_ge2_members))
summary(as.numeric(theoretical_max_ties_CLUSTEREDPHYLOANY_ge2_members))

# For individuals in each of these defined molecular clusters, how many are in the named list?

named_pt_dt <- cbind(net_dt$StudyIDFrom, net_dt$StudyIDTo)

## Take specific examples (Distance = 005)

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

class(cluster_sizes_005)
names(cluster_sizes_005)

class(cluster_sizes_015)
names(cluster_sizes_015)

calc_num_in_named_partners <- 
  # FOR HIV TRACE 005 and 015
  function(distance="005"){
    
    named_pt_num_list <- NULL
    
    if (distance == "005") {
      clusters_List = cluster_sizes_005
    }
    else if (distance == "015") {
      clusters_List = cluster_sizes_015
    }
    else if (distance == "clusteredphyloany") {
      clusters_List = cluster_sizes_clusteredphyloany
    }
    
    for (i in names(clusters_List)){
      
      #cat("Clusters list: ", clusters_List, "\n")
      
      if (distance != "clusteredphyloany"){
      label_col <- paste0("ClusteredHIVTrace", distance)
      }
      else if (distance == "clusteredphyloany"){
        label_col <- "ClusteredPhyloAny"
      }
      
      cluster_distance <- which(individuals_dt[[label_col]] == i) 
      
      #cat("Cluster distance: ", cluster_distance, "\n")
      
      studyids_cluster_distance <- individuals_dt$StudyID[cluster_distance] 
      #print("Study IDs cluster distance: ", class(studyids_cluster_distance), "\n", "\n")
      
      identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_cluster_distance)
      identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_cluster_distance)
      
      named_pt_num <- intersect(identify_p1_rows, identify_p2_rows)       
      named_pt_num_list <- c(named_pt_num_list, length(named_pt_num))
      
    }
    
    
    return(named_pt_num_list)
  }

a <- calc_num_in_named_partners(distance="005")
b <- calc_num_in_named_partners(distance="015")
c <- calc_num_in_named_partners(distance="clusteredphyloany")

length(a)
table(a[-1], exclude = NULL)

length(b)
table(b[-1], exclude = NULL)

length(c)
table(c[-1], exclude = NULL)

named_partner_nums_005 <- cbind(a[-1])
named_partner_nums_015 <- cbind(b[-1])
named_partner_nums_clusteredphyloany <- cbind(c[-1])

summary(named_partner_nums_005[which(as.numeric(cluster_sizes_005) > 1)])      
summary(named_partner_nums_015[which(as.numeric(cluster_sizes_015) > 1)]) 
summary(named_partner_nums_clusteredphyloany[which(as.numeric(cluster_sizes_clusteredphyloany) > 1)]) 



# Testing --

## Distance = 015

### HIV TRACE 1(Expected 0. )
HIVTRACE_1_015 <- which(individuals_dt$ClusteredHIVTrace015 == "HIVTRACE_1") 
# 2 indidviduals in cluster HIVTRACE_1
studyids_HIVTRACE_1_015 <- individuals_dt$StudyID[HIVTRACE_1_015] 
#Study IDs of these 1 individuals

studyids_HIVTRACE_1_015  %in% net_dt$StudyIDFrom 
#None of the IDs appear in the STUDYIDFROM col in named partner data set 
studyids_HIVTRACE_1_015  %in% net_dt$StudyIDTo 
# None of the IDs appear in the STUDYIDTO column

identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_HIVTRACE_1_015)
identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_HIVTRACE_1_015)

intersect(identify_p1_rows, identify_p2_rows) #2 such occurences out of max of 9 Choose 2 = 36??

named_pt_dt[identify_p1_rows,]
named_pt_dt[identify_p2_rows,]


## EXPECTATION MATCHED

### HIV TRACE 81 (Expected named partners: 7. Matched)
HIVTRACE_81_015 <- which(individuals_dt$ClusteredHIVTrace015 == "HIVTRACE_81") 
# 22 indidviduals in cluster HIVTRACE_81
studyids_HIVTRACE_81_015 <- individuals_dt$StudyID[HIVTRACE_81_015] 
#Study IDs of these 22 individuals

studyids_HIVTRACE_81_015  %in% net_dt$StudyIDFrom 
#14 of the IDs appear in the STUDYIDFROM col in named partner data set 
studyids_HIVTRACE_81_015  %in% net_dt$StudyIDTo 
# 7 of the IDs appear in the STUDYIDTO column

identify_p1_rows <- which(net_dt$StudyIDFrom %in% studyids_HIVTRACE_81_015)
identify_p2_rows <- which(net_dt$StudyIDTo %in% studyids_HIVTRACE_81_015)

intersect(identify_p1_rows, identify_p2_rows) #2 such occurences out of max of 9 Choose 2 = 36??

named_pt_dt[identify_p1_rows,]
named_pt_dt[identify_p2_rows,]

## EXPECTATION MATCHED

#Repeat named partnership/cluster size distribution with "ClusteredPhyloAny" sequence ---------------------------


# How many persons are naming partners ---------------------------

unique_naming_persons <- net_dt %>%
  select(StudyIDFrom) %>%
  distinct()

count_unique_naming_persons <- individuals_dt %>%
  filter(StudyID %in% unique_naming_persons$StudyIDFrom)

nrow(count_unique_naming_persons)


# How many persons are naming partners per cluster ---------------------------

# function
calc_num_persons_naming_partners <- 
  function(distance="005") {
    
    persons_naming_num_list <- NULL
    
    if (distance == "005") {
      clusters_List = cluster_sizes_005
    }
    else if (distance == "015") {
      clusters_List = cluster_sizes_015
    }
    else if (distance == "clusteredphyloany") {
      clusters_List = cluster_sizes_clusteredphyloany
    }
    
    for (i in names(clusters_List)) {
      
      if (distance != "clusteredphyloany") {
        label_col <- paste0("ClusteredHIVTrace", distance)
      }
      else if (distance == "clusteredphyloany") {
        label_col <- "ClusteredPhyloAny"
      }
      
      cluster_distance <- which(individuals_dt[[label_col]] == i) 
      
      studyids_cluster_distance <- individuals_dt$StudyID[cluster_distance]
      
      # Check individuals who named partners
      persons_naming <- unique(net_dt$StudyIDFrom[net_dt$StudyIDFrom %in% studyids_cluster_distance])
      
      persons_naming_num_list <- c(persons_naming_num_list, length(persons_naming))
      
    }
    
    return(persons_naming_num_list)
  }

a1 <- calc_num_persons_naming_partners(distance="005")
b1 <- calc_num_persons_naming_partners(distance="015")
c1 <- calc_num_persons_naming_partners(distance="clusteredphyloany")

length(a1)
table(a1[-1], exclude = NULL)

length(b1)
table(b1[-1], exclude = NULL)

length(c1)
table(c1[-1], exclude = NULL)

naming_partner_persons_005 <- cbind(a1[-1])
naming_partner_persons_015 <- cbind(b1[-1])
naming_partner_persons_clusteredphyloany <- cbind(c1[-1])

summary(naming_partner_persons_005)      
summary(naming_partner_persons_005[which(as.numeric(cluster_sizes_005) > 1)]) # the conditional adds only 1 NA, no difference otherwise

summary(naming_partner_persons_015) 
summary(naming_partner_persons_015[which(as.numeric(cluster_sizes_015) > 1)]) # as above

summary(naming_partner_persons_clusteredphyloany) 
summary(naming_partner_persons_clusteredphyloany[which(as.numeric(cluster_sizes_clusteredphyloany) > 1)]) # as above


# How many persons are naming partners in the same cluster ---------------------------

calc_persons_naming_within_cluster <- function(distance="005") {
  
  within_cluster_naming_list <- NULL
  
  if (distance == "005") {
    clusters_List = cluster_sizes_005
  }
  else if (distance == "015") {
    clusters_List = cluster_sizes_015
  }
  else if (distance == "clusteredphyloany") {
    clusters_List = cluster_sizes_clusteredphyloany
  }
  
  for (i in names(clusters_List)) {
    
    if (distance != "clusteredphyloany") {
      label_col <- paste0("ClusteredHIVTrace", distance)
    }
    else if (distance == "clusteredphyloany") {
      label_col <- "ClusteredPhyloAny"
    }
    
    cluster_distance <- which(individuals_dt[[label_col]] == i) 
    studyids_cluster_distance <- individuals_dt$StudyID[cluster_distance]
    
    # Initial count for the current cluster
    cluster_count <- 0
    
    # For each individual in the cluster, check their named partners 
    for (study_id in studyids_cluster_distance) {
      
      # Get the partners they named
      named_partners <- net_dt$StudyIDTo[net_dt$StudyIDFrom == study_id]
      
      # Count how many of these partners are also in the cluster
      cluster_count <- cluster_count + sum(named_partners %in% studyids_cluster_distance)
      
    }
    
    within_cluster_naming_list <- c(within_cluster_naming_list, cluster_count)
    
  }
  
  return(within_cluster_naming_list)
}


a2 <- calc_persons_naming_within_cluster(distance="005")
b2 <- calc_persons_naming_within_cluster(distance="015")
c2 <- calc_persons_naming_within_cluster(distance="clusteredphyloany")

summary(a2)
summary(b2)
summary(c2)


## The `calc_persons_naming_within_cluster` gives the same answer 
## as the `calc_num_in_named_partners()` function above because:

# `calc_num_in_named_partners`:
#   
# - For each cluster, identifies the individuals that are a part of it.
# - For these individuals, determines the partners they have named.
# - Checks whether these named partners are also within the same cluster.
# - Aggregates the results based on the count of unique named relationships within the cluster.

# calc_persons_naming_within_cluster:
#   
# - For each cluster, identifies the individuals that are a part of it.
# - For these individuals, identifies the partners they have named.
# - Checks whether these named partners are also within the same cluster.
# - Aggregates the results based on the number of instances where cluster individuals named partners from the same cluster.



# Create molecular links for the phylogenetic clusters ---------------------------

## Adapt old code: https://github.com/kantorlab/rtp-network/blob/159c04c8ade62462b666159ff35549a352478c1f/archive/create_edgelists_from_molecular_clusters.R#L1-L103

clusters <- 
  individuals_dt %>% select(StudyID, ClusteredPhyloAny)
head(clusters)

head(clusters$ClusteredPhyloAny, 100)
ids_in_phylo_clusters <- which(!is.na(clusters$ClusteredPhyloAny))
length(ids_in_phylo_clusters)

studyids_in_phylo_clusters <- individuals_dt$StudyID[ids_in_phylo_clusters]
length(studyids_in_phylo_clusters)

clusterIDs_phylo <- clusters$ClusteredPhyloAny[ids_in_phylo_clusters]
length(clusterIDs_phylo)

# create named list with study ids in each cluster 

## phylo
(table(individuals_dt$ClusteredPhyloAny, exclude = NULL))
cluster_names_phylo <- names((table(individuals_dt$ClusteredPhyloAny, exclude = NULL)))[-1]

study_ids_at_clusterid_phylo = as.list(rep(NA, length(cluster_names_phylo)))
names(study_ids_at_clusterid_phylo) = cluster_names_phylo

for (i in 1:length(study_ids_at_clusterid_phylo)){
  study_ids_at_clusterid_phylo[[names(study_ids_at_clusterid_phylo)[i]]] <-  
    (individuals_dt$StudyID[(which(individuals_dt$ClusteredPhyloAny == names(study_ids_at_clusterid_phylo)[[i]]))])
}

study_ids_at_clusterid_phylo
length(study_ids_at_clusterid_phylo)

# construct molecular edgelists

el_matrix_phylo <- NULL

for (i in 1:length(study_ids_at_clusterid_phylo)){
  if (length(study_ids_at_clusterid_phylo[[i]]) > 1){
    el_matrix_phylo <- rbind(el_matrix_phylo, t(combn(as.character(study_ids_at_clusterid_phylo[[i]]), 2)))
  }
}

dim(el_matrix_phylo) 

el_cluster_size <- NULL
for (i in 1:length(study_ids_at_clusterid_phylo)){
  el_cluster_size <- c(el_cluster_size, unlist(length(study_ids_at_clusterid_phylo[[i]])))
}
sort(el_cluster_size)

n_ties <- unlist(lapply(el_cluster_size, function(x) choose(x, 2)))
sum(n_ties)

summary(el_cluster_size)


# Investigate overlap between partner links and molecular links ---------------------------

class(el_matrix_phylo)
head(el_matrix_phylo)

partner_net_el <- cbind(net_dt$StudyIDFrom, net_dt$StudyIDTo)
class(partner_net_el)
head(partner_net_el)

# Convert matrices to data frames
phylo_el <- as.data.frame(el_matrix_phylo, stringsAsFactors = FALSE)
ct_el <- as.data.frame(partner_net_el, stringsAsFactors = FALSE)

phylo_el$uid <- apply(phylo_el, 1, function(x) paste(sort(x), collapse = "-"))
ct_el$uid <- apply(ct_el, 1, function(x) paste(sort(x), collapse = "-"))

head(phylo_el)
head(ct_el)

## any duplicated edges in ct_el (consider directionality)
duplicates <- ct_el %>%
  filter(duplicated(uid) | duplicated(uid, fromLast = TRUE))
dim(duplicates) # duplicates occur because of interviews at multiple times

ct_el_unique <- ct_el %>%
  distinct(uid, .keep_all = TRUE) #keeping first occurence of each edge

dim(ct_el_unique)


# Identify common links
common_links <- intersect(phylo_el$uid, ct_el$uid)

# Count common links
num_common_links <- length(common_links)

# Print the number of common links
print(num_common_links)

## vanilla ovelap
num_common_links/length(phylo_el$uid)
num_common_links/length(ct_el$uid)

## jaccard index
union_of_edge_sets <- unique(c(phylo_el$uid,ct_el$uid))
length(union_of_edge_sets)
num_common_links/length(union_of_edge_sets)

## How many partner contact tracing (social) network clusters?
ct_net_unique <- network(ct_el_unique, directed=TRUE)
#gplot(ct_net_unique) #commented out for saving time

## Check number of nodes and edges
ct_net_unique

set.network.attribute(ct_net_unique, "directed", FALSE) 
##consider graph as undirected for counting clusters

is.directed(ct_net_unique)  # Should return FALSE

## compute the components (i.e., clusters) on the updated network
num_components <- sna::components(ct_net_unique)
print(paste("Number of connected components:", num_components))

## distribution of cluster sizes
component_sizes <- sna::component.dist(ct_net_unique)
#head(component_sizes)
summary(component_sizes$csize)


# Plot molecular network
phylo_net <- network(phylo_el, directed=FALSE)
phylo_net

#gplot(phylo_net, usearrows = FALSE)


# Overlap with 005 ---------------------------

## See https://github.com/kantorlab/rtp-network/blob/159c04c8ade62462b666159ff35549a352478c1f/archive/create_edgelists_from_molecular_clusters.R#L1-L103

clusters_trace <- 
  individuals_dt %>% select(StudyID, ClusteredHIVTrace005, ClusteredHIVTrace015)
head(clusters_trace)

## 005
head(clusters$ClusteredHIVTrace005, 100)
ids_in_005_clusters <- which(substr(clusters_trace$ClusteredHIVTrace005, 1, 3) == "HIV")
length(ids_in_005_clusters)

studyids_in_005_clusters <- individuals_dt$StudyID[ids_in_005_clusters]
length(studyids_in_005_clusters)

clusterIDs_005 <- clusters_trace$ClusteredHIVTrace005[ids_in_005_clusters]
length(clusterIDs_005)

## create named list with study ids in each cluster 
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
head(study_ids_at_clusterid_005)

## create edgelist
el_matrix_005 <- matrix(ncol = 2, nrow = 0)

## iterate over each cluster
for (i in 1:length(study_ids_at_clusterid_005)) {
  # Check if the cluster has more than one ID
  if (length(study_ids_at_clusterid_005[[i]]) > 1) {
    # Create combinations of pairs and add to the edgelist
    el_matrix_005 <- rbind(el_matrix_005, t(combn(as.character(study_ids_at_clusterid_005[[i]]), 2)))
  }
}

head(el_matrix_005)
dim(el_matrix_005) 

el_df_005 <- as.data.frame(el_matrix_005, stringsAsFactors = FALSE)
names(el_df_005) <- c("Source", "Target")

el_df_005$uid <- apply(el_df_005, 1, 
  function(x) paste(sort(x), collapse = "-"))
head(el_df_005)

common_links_005 <- intersect(el_df_005$uid, ct_el$uid)
length(common_links_005)

length(common_links_005)/length(ct_el$uid)
length(common_links_005)/length(el_df_005$uid)


# Overlap with 015 ---------------------------

## 015
head(clusters$ClusteredHIVTrace015, 100)
ids_in_015_clusters <- which(substr(clusters_trace$ClusteredHIVTrace015, 1, 3) == "HIV")
length(ids_in_015_clusters)

studyids_in_015_clusters <- individuals_dt$StudyID[ids_in_015_clusters]
length(studyids_in_015_clusters)

clusterIDs_015 <- clusters_trace$ClusteredHIVTrace015[ids_in_015_clusters]
length(clusterIDs_015)

## create named list with study ids in each cluster 
(table(individuals_dt$ClusteredHIVTrace015, exclude = NULL))
cluster_names_015 <- names((table(individuals_dt$ClusteredHIVTrace015, exclude = NULL)))[-1]

study_ids_at_clusterid_015 = as.list(rep(NA, length(cluster_names_015)))
names(study_ids_at_clusterid_015) = cluster_names_015

for (i in 1:length(study_ids_at_clusterid_015)){
  study_ids_at_clusterid_015[[names(study_ids_at_clusterid_015)[i]]] <-  
    (individuals_dt$StudyID[(which(individuals_dt$ClusteredHIVTrace015 == names(study_ids_at_clusterid_015)[[i]]))])
}

study_ids_at_clusterid_015
length(study_ids_at_clusterid_015)
head(study_ids_at_clusterid_015)

## create edgelist
el_matrix_015 <- matrix(ncol = 2, nrow = 0)

## iterate over each cluster
for (i in 1:length(study_ids_at_clusterid_015)) {
  # Check if the cluster has more than one ID
  if (length(study_ids_at_clusterid_015[[i]]) > 1) {
    # Create combinations of pairs and add to the edgelist
    el_matrix_015 <- rbind(el_matrix_015, t(combn(as.character(study_ids_at_clusterid_015[[i]]), 2)))
  }
}

head(el_matrix_015)
dim(el_matrix_015) 

el_df_015 <- as.data.frame(el_matrix_015, stringsAsFactors = FALSE)
names(el_df_015) <- c("Source", "Target")

el_df_015$uid <- apply(el_df_015, 1, 
  function(x) paste(sort(x), collapse = "-"))
head(el_df_015)

common_links_015 <- intersect(el_df_015$uid, ct_el$uid)
length(common_links_015)

length(common_links_015)

length(common_links_015)/length(ct_el$uid)
length(common_links_015)/length(el_df_015$uid)








# Save Object ---------------------------


# # Create an environment and assign the objects to it
eda_env <- new.env()
eda_env$individuals_dt <- individuals_dt
eda_env$net_dt <- net_dt
eda_env$named_pt_idx_005 <- a
eda_env$named_pt_idx_015 <- b
eda_env$named_pt_idx_clusteredphyloany <- c
eda_env$ct_net_unique <- ct_net_unique
eda_env$phylo_net <- phylo_net
eda_env$common_links <- common_links
eda_env$phylo_el <- phylo_el
eda_env$ct_el <- ct_el
eda_env$ct_el_unique <- ct_el_unique


# Save the environment as an RDS file
saveRDS(eda_env, "eda_objects.rds")