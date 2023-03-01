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

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D30_20211013_V1"
list.files(path=data_dir)
net_dt <- read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))
individuals_dt <- read.csv(paste0(data_dir, "/Individuals.csv"))

dim(net_dt)
str(net_dt)
sort(colnames(net_dt))
View(net_dt)

dim(individuals_dt)
colnames(individuals_dt)

table(net_dt$InterviewRank, exclude = NULL)
table(net_dt$InterviewRank, exclude = NULL)/
  sum(table(net_dt$InterviewRank, exclude = NULL))


# Needed function ---------------------------
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Create network object ---------------------------

net_dt_mat <- cbind(as.character(net_dt[,1]), as.character(net_dt[,2]))
net <- as.network(net_dt_mat, bipartite = F, directed = T)
net
length(net %v% "vertex.names")
net %v% "vertex.names"

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

net_dt_r1_mat <- cbind(as.character(net_dt_r1[,1]), as.character(net_dt_r1[,2]))
net_r1 <- as.network(net_dt_r1_mat, bipartite = F, directed = T)
net_r1
length(net_r1 %v% "vertex.names")
head(net_r1 %v% "vertex.names")

## Client Reached
table(net_dt$ClientReached, exclude = NULL)
table(net_dt$ClientReached, exclude = NULL)/sum(table(net_dt$ClientReached, 
                                                         exclude = NULL))
table(net_dt_r1$ClientReached, exclude = NULL)
table(net_dt_r1$ClientReached, exclude = NULL)/sum(table(net_dt_r1$ClientReached, 
                                                         exclude = NULL))

# Properties of full network ---------------------------

## Study ID From
table(net_dt$StudyIDFrom, exclude = NULL)
initial_study_id_from <- substr(net_dt$StudyIDFrom, 1, 1)
table(initial_study_id_from, exclude = NULL)

## Study ID To
table(net_dt$StudyIDTo, exclude = NULL)
initial_study_id_to <- substr(net_dt$StudyIDTo, 1, 1)
table(initial_study_id_to, exclude = NULL)


## Interview ID
table(net_dt$InterviewID, exclude = NULL)
interview_id_last_digit <- substrRight(as.character(net_dt$InterviewID), 2)
table(interview_id_last_digit, exclude = NULL)


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

## Referral Date
net_dt$ReferralDate <- as.Date(net_dt$ReferralDate)
Referral_date_in_Year <- format(net_dt$ReferralDate, "%Y")
table(Referral_date_in_Year, exclude = NULL)
class(net_dt$ReferralDate)
names(net_dt$ReferralDate)
table(net_dt$ReferralDate, exclude = NULL)

xtabs(~factor(Referral_date_in_Year, exclude = NULL) + 
        factor(net_dt$InterviewType, exclude = NULL))

## Reported
table(net_dt$Reported, exclude = NULL)

## Reported Data
net_dt$ReportedDate <- as.Date(net_dt$ReportedDate)
table(net_dt$ReportedDate, exclude = NULL)
ReportedDate_in_Year <- format(net_dt$ReportedDate, "%Y")
table(ReportedDate_in_Year, exclude = NULL)
data.frame(table(ReportedDate_in_Year, exclude = NULL), byrow = T)

## Index HIV Test Date
net_dt$IndexHIVTestDate <- as.Date(net_dt$IndexHIVTestDate)
table(net_dt$IndexHIVTestDate, exclude = NULL)
Index_HIV_Test_Date_in_Year <- format(net_dt$IndexHIVTestDate, "%Y")
table(Index_HIV_Test_Date_in_Year, exclude = NULL)
data.frame(table(Index_HIV_Test_Date_in_Year, exclude = NULL), byrow = T)

## Identified Date 
net_dt$IdentifiedDate <- as.Date(net_dt$IdentifiedDate)
table(net_dt$IdentifiedDate, exclude = NULL)
IdentifiedDateinYear <- format(net_dt$IdentifiedDate, "%Y")
table(IdentifiedDateinYear, exclude = NULL)
data.frame(table(IdentifiedDateinYear, exclude = NULL), byrow = T)

## Identified Risk
table(net_dt$IdentifiedRisk, exclude = NULL)

## Notify
table(net_dt$Notify, exclude = NULL)

## NotifyWho
table(net_dt$NotifyWho, exclude = NULL)
sort(table(net_dt$NotifyWho, exclude = NULL))

## Notify Method
sort(table(net_dt$NotifyMethod, exclude = NULL))

## Already Index Case
sort(table(net_dt$AlreadyIndexCase, exclude = NULL))

## Client Reached
sort(table(net_dt$ClientReached, exclude = NULL))

## Client Not Reached
sort(table(net_dt$ClientNotReached, exclude = NULL))

## Partner Services Offered Date 
PartnerServicesOfferedDate <- (net_dt$PartnerServicesOfferedDate) #as.Date command not working
YYYY_PartnerServicesOfferedDate <- substr(PartnerServicesOfferedDate, 1, 4)
data.frame(t(table(YYYY_PartnerServicesOfferedDate)), byrow=T)

## Partner Services Accepted 
table(net_dt$PartnerServicesAccepted, exclude = NULL)

## Partner Services Not Accepted 
sort(table(net_dt$PartnerServicesNotAccepted, exclude = NULL))
table(table(net_dt$PartnerServicesNotAccepted, exclude = NULL))

## Referred to HIV Test
sort(table(net_dt$ReferredToHIVTest, exclude = NULL))

## Final HIV Test Date
FinalHIVTestDate <- (net_dt$FinalHIVTestDate) #as.Date command not working
YYYY_FinalHIVTestDate <- substr(FinalHIVTestDate, 1, 4)
data.frame(t(table(YYYY_FinalHIVTestDate)), byrow=T)

## Final HIV Test
sort(table(net_dt$FinalHIVTest, exclude = NULL))

## ReferredPrep
sort(table(net_dt$ReferredPrEP, exclude = NULL))

# Network Diagrams
plot(net)
gplot(net,             
        #usearrows=FALSE,
      edge.lwd=0.5,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = FALSE,
      vertex.cex = 1,
      #mode = "mds"
      #vertex.col = vertex.col,
      #vertex.border = vertex.border,
      #vertex.sides = vertex.sides,
      #edge.col = edge.col,
      #coord = fix_coord
)

plot(net_r1)
gplot(net_r1,             
      #usearrows=FALSE,
      edge.lwd=0.5,
      edge.lty=3,
      #usecurve=TRUE,
      displayisolates = FALSE,
      vertex.cex = 1,
      #mode = "mds"
      #vertex.col = vertex.col,
      #vertex.border = vertex.border,
      #vertex.sides = vertex.sides,
      #edge.col = edge.col,
      #coord = fix_coord
)

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

options(max.print = 10000)

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

Reduce(intersect, list(msm_ids, idu_ids))
Reduce(intersect, list(HRH_ids, idu_ids))
Reduce(intersect, list(HRH_ids, msm_ids))
Reduce(intersect, list(msm_ids, idu_ids, HRH_ids))

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
unique(individuals_dt$ClusteredPhyloAny) #351 unique clusters

sort(table(individuals_dt$ClusteredHIVTrace005, exclude = NULL)) #cluster sizes at 0.05% distance
sort(table(individuals_dt$ClusteredHIVTrace015, exclude = NULL)) #cluster sizes at 0.15% distance
sort(table(individuals_dt$ClusteredPhyloAny, exclude = NULL)) #ClusteredPhyloAny


# How many unique molecular clusters have only one individual? ---------------------------

length(which(table(individuals_dt$ClusteredHIVTrace005) == 1)) #34 clusters only have 1 member
length(which(table(individuals_dt$ClusteredHIVTrace015) == 1)) #35 clusters only have 1 member
length(which(table(individuals_dt$ClusteredPhyloAny) == 1)) #35 clusters only have 1 member

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
    
    if (distance == "005"){
      clusters_List = cluster_sizes_005
    } else if (distance == "015")
      clusters_List = cluster_sizes_015
    
    for (i in names(clusters_List)){
      
      #cat("Clusters list: ", clusters_List, "\n")
      
      label_col <- paste0("ClusteredHIVTrace", distance)
      #cat("Label col: ", label_col, "\n")
      
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

length(a)
table(a[-1], exclude = NULL)

length(b)
table(b[-1], exclude = NULL)

named_partner_nums_005 <- cbind(a[-1])
named_partner_nums_015 <- cbind(b[-1])

summary(named_partner_nums_005[which(as.numeric(cluster_sizes_005) > 1)])      
summary(named_partner_nums_015[which(as.numeric(cluster_sizes_015) > 1)])     


## Function for ClusteredPhlyoAny
phyloany_cluster_ids <- individuals_dt$ClusteredPhyloAny

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
eda_env$named_pt_net <- net
eda_env$named_pt_idx_005 <- a
eda_env$named_pt_idx_015 <- b

# Save the environment as an RDS file
saveRDS(eda_env, "eda_objects.rds")
