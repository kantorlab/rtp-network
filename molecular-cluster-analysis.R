rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)
library(sna)


# Setup ---------------------------

setwd("/users/akhann16/code/rtp-network/out") #on rstudio (VIA OOD)
par(mar=c(1,1,1,1)) #for figures
ls()

### 

# Read data ---------------------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
net_dt_old <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))
net_dt <- read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv")
individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))

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
length(which(table(individuals_dt$ClusteredPhyloAny) == 1)) #0

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


#Save Object ---------------------------

#save.image(file="EDA.RData")

# # Create an environment and assign the objects to it
eda_env <- new.env()
eda_env$individuals_dt <- individuals_dt
eda_env$net_dt <- net_dt
eda_env$named_pt_idx_005 <- a
eda_env$named_pt_idx_015 <- b
eda_env$named_pt_idx_clusteredphyloany <- c

# Save the environment as an RDS file
saveRDS(eda_env, "eda_objects.rds")
