rm(list=ls())

# Libraries -------------

library(data.table)
library(dplyr)
library(stringi)
library(network)


# Datasets -------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
partner_db_old <- as.data.table(read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))) 
partner_db <- as.data.table(read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv"))
genomic_db <- as.data.table(read.csv(paste0(data_dir, "/Individuals.csv")))



# Utilites -------------

source("utils/compare-descriptives-function.R")
source("utils/compute-cascade.R")


# Descriptives -------------

# How many persons in the Genomic DB are sequenced?
table(genomic_db$Sequence, exclude=NULL)
genomic_db_sequenced_id <- which(genomic_db$Sequence == "True")
length(genomic_db_sequenced_id)
genomic_db_sequenced <- genomic_db$StudyID[genomic_db_sequenced_id]
genomic_db_sequenced_dt <- genomic_db[genomic_db_sequenced_id,]
dim(genomic_db_sequenced_dt)

# Compute intersections between sequenced persons in genomic db and partnerdb
length(intersect(genomic_db_sequenced, partner_db$StudyIDFrom))
length(intersect(genomic_db_sequenced, partner_db$StudyIDTo))
length(intersect(partner_db$StudyIDFrom, partner_db$StudyIDTo))
length(Reduce(intersect, list(genomic_db_sequenced, partner_db$StudyIDFrom, partner_db$StudyIDTo)))

# How many of the interviewed index cases who are also in the genomic DB provided identifiable partner data?
partner_db_non_missing_studyidto <- partner_db[which(partner_db$StudyIDTo != ""), ]
index_cases_who_named_partners <- intersect(genomic_db_sequenced, partner_db_non_missing_studyidto$StudyIDFrom)
length(index_cases_who_named_partners)

# How many of the interviewed index cases who are also in the genomic DB did not provide identifiable partner data?
intersected_persons_partner_genomic_db <- intersect(genomic_db_sequenced, partner_db$StudyIDFrom)
intersected_persons_without_named_partner <- intersected_persons_partner_genomic_db[!intersected_persons_partner_genomic_db %in% partner_db_non_missing_studyidto$StudyIDFrom]
length(intersected_persons_without_named_partner)  

# How many total partners are named by the index cases in the genomic DB who named partners?
length(unique(partner_db_non_missing_studyidto$StudyIDTo))

# Compute Jaccard coefficient

## identify unique individuals
partner_individuals_from <- unique(partner_db$StudyIDFrom)
partner_individuals_to <- unique(partner_db$StudyIDTo)

## combine the 'StudyIDFrom' and 'StudyIDTo' for partner_db
partner_individuals <- unique(c(partner_individuals_from, partner_individuals_to))
length(partner_individuals)

## compute intersection and union lengths
intersection_length <- length(intersect(genomic_db_sequenced, partner_individuals))
union_length <- length(union(genomic_db_sequenced, partner_individuals))

## calculate coefficient
jaccard_coefficient <- intersection_length / union_length
jaccard_coefficient

# How many are Genomic DB persons are present/not present in Partner DB and vice versa 
length(which(genomic_db_sequenced %in% partner_individuals))
length(which(!genomic_db_sequenced %in% partner_individuals))

length(which(partner_individuals %in% genomic_db_sequenced))
length(which(!partner_individuals %in% genomic_db_sequenced))

# How many named partners are also sequenced 
named_partners_in_genomic_db <- intersect(genomic_db_sequenced, partner_db$StudyIDTo)
id_named_partners_in_genomic_db <- which(genomic_db_sequenced %in% named_partners_in_genomic_db)

length(which(is.na(genomic_db$ClusteredPhyloAny)))
length(which(is.na(genomic_db$ClusteredPhyloAny)))

table(genomic_db$Sequence, exclude=NULL)
table(genomic_db$SequenceSubtype, exclude=NULL)

table(genomic_db[id_named_partners_in_genomic_db]$Sequence, exclude=NULL)

# How many named partners in the Partner DB are contacted?
unique_named_partners <- unique(partner_db$StudyIDTo)
id_unique_named_partners <- which(partner_db$StudyIDTo %in% unique_named_partners)

colnames(partner_db)
table(partner_db$ClientReached, exclude=NULL)
table(partner_db$NoReachWhy, exclude=NULL)

table(partner_db[id_unique_named_partners,]$ClientReached, exclude=NULL)

# How many sequenced persons cluster phylogenetically?
seq_pers_cluster_phylo <- nrow(genomic_db_sequenced_dt) - length(which(is.na(genomic_db_sequenced_dt$ClusteredPhyloAny)))
seq_pers_cluster_phylo/nrow(genomic_db_sequenced_dt)

# How many index cases (who appear in partner DB) and are sequenced are found to cluster phylogenetically?
seq_index_cases_nmd_pts <- which(genomic_db_sequenced_dt$StudyID %in% index_cases_who_named_partners)
seq_index_cases_nmd_pts_dt <- genomic_db_sequenced_dt[seq_index_cases_nmd_pts,]
ans1 <- nrow(seq_index_cases_nmd_pts_dt) -  length(which(is.na(seq_index_cases_nmd_pts_dt$ClusteredPhyloAny)))
ans1/nrow(seq_index_cases_nmd_pts_dt)

# How many named partners (who appear in partner DB) and are sequenced are found to cluster phylogenetically?
sequenced_who_are_named_pts <- (intersect(genomic_db_sequenced, partner_db$StudyIDTo))
sequenced_who_are_named_pts_id <- which(genomic_db_sequenced_dt$StudyID %in% sequenced_who_are_named_pts)
sequenced_who_are_named_pts_dt <- genomic_db_sequenced_dt[sequenced_who_are_named_pts_id,] 
ans2 <- nrow(sequenced_who_are_named_pts_dt) -  length(which(is.na(sequenced_who_are_named_pts_dt$ClusteredPhyloAny)))
ans2/nrow(sequenced_who_are_named_pts_dt)

# Comparison Table ("Table 1") -------------

## Column 1: Sequenced Index cases in Genomic DB
length(unique(genomic_db_sequenced_dt$StudyID))
compare_descriptives(dt=genomic_db_sequenced_dt)


## Suggested Column 2: Descriptors on Contact Tracing Database 
length(partner_individuals)
head(partner_individuals)

length(which(partner_individuals %in% genomic_db$StudyID))
length(which(partner_individuals_from %in% genomic_db$StudyID))

length(which(partner_individuals %in% genomic_db_sequenced_dt$StudyID))
length(which(partner_individuals_from %in% genomic_db_sequenced_dt$StudyID))
length(which(partner_individuals_to %in% genomic_db_sequenced_dt$StudyID))
length(which(!partner_individuals_to %in% partner_individuals_from))
partner_individuals_to[which(!partner_individuals_to %in% partner_individuals_from)]

## Sequenced Index cases who were interviewed in Partner DB
sequenced_and_interviewed <- intersect(genomic_db_sequenced, partner_db$StudyIDFrom)
sequenced_and_interviewed_idx <- which(genomic_db_sequenced_dt$StudyID %in% sequenced_and_interviewed)
sequenced_and_interviewed_db <- genomic_db_sequenced_dt[sequenced_and_interviewed_idx,]
dim(sequenced_and_interviewed_db)

table(sequenced_and_interviewed_db$DemoGender) #gender
table(sequenced_and_interviewed_db$DemoGender)/sum(table(sequenced_and_interviewed_db$DemoGender))

table(sequenced_and_interviewed_db$DemoRace) #race
table(sequenced_and_interviewed_db$DemoRace)/sum(table(sequenced_and_interviewed_db$DemoRace))

compare_descriptives(dt=sequenced_and_interviewed_db) 


## Sequenced Index cases who were interviewed in Partner DB and named at least one partner

sequenced_and_named_pt_idx <- which(genomic_db_sequenced_dt$StudyID %in% index_cases_who_named_partners)
sequenced_and_named_pt_dt <- genomic_db_sequenced_dt[sequenced_and_named_pt_idx,]
dim(sequenced_and_named_pt_dt)

compare_descriptives(dt=sequenced_and_named_pt_dt) 

## Named partners in the genomic DB
named_partners_in_genomic_dt <- genomic_db_sequenced_dt[id_named_partners_in_genomic_db,]
dim(named_partners_in_genomic_dt)

compare_descriptives(dt=named_partners_in_genomic_dt) 


# Cascade Table ("Table 2") -------------

## category 1 - combined categories (all)
length(index_cases_who_named_partners) # row 1 - all
length(unique(partner_db_non_missing_studyidto$StudyIDTo))
table(partner_db_non_missing_studyidto$ClientReached, exclude = NULL)
#table(partner_db_non_missing_studyidto$HIVTested, exclude = NULL)

named_partners_tested_countstudyidfrom <- 
  partner_db %>%
  filter(HIVTested == 1) %>%
  pull(StudyIDFrom) %>%
  unique()

length(named_partners_tested_countstudyidfrom)

named_partners_tested_studyidto <- 
  partner_db %>%
  filter(HIVTested == 1) %>%
  pull(StudyIDTo) %>%
  unique() 

length(named_partners_tested_studyidto) 

unique_named_pt_contacts_reported_by_IPs <- 
  partner_db_non_missing_studyidto$StudyIDTo

length(which(unique_named_pt_contacts_reported_by_IPs %in% 
               genomic_db_sequenced_dt$StudyID))

named_contacts_in_genomic_dataset <- 
  unique_named_pt_contacts_reported_by_IPs[which(unique_named_pt_contacts_reported_by_IPs %in% 
                                                   genomic_db_sequenced_dt$StudyID)]
length(named_contacts_in_genomic_dataset) # number of reported partners in genomic database


## verify that there are 152 common participants between 
## persons in the genomic dataset and partners named in the contact tracing dataset

named_contacts_rows_from_genomic_dt <- 
  genomic_db_sequenced_dt[StudyID %in% named_contacts_in_genomic_dataset,,]
dim(named_contacts_rows_from_genomic_dt)

length(intersect(genomic_db_sequenced_dt$StudyID, named_contacts_in_genomic_dataset))

## check diagnosis dates for the intersection of named partners with genomic databse
table(substr(named_contacts_rows_from_genomic_dt$HIVDxDate, 1, 4), exclude = NULL)
sum(table(substr(named_contacts_rows_from_genomic_dt$HIVDxDate, 1, 4), exclude = NULL))


length(named_contacts_rows_from_genomic_dt$HIVDxDate)
length(which(named_contacts_rows_from_genomic_dt$HIVDxDate == ""))
table(genomic_db_sequenced_dt[StudyID %in% 
                                named_contacts_in_genomic_dataset,,]$HIVDxDate, exclude = NULL)

table(genomic_db_sequenced_dt[StudyID %in% 
                                named_contacts_in_genomic_dataset,,]$Sequence, exclude = NULL)


length(intersect(genomic_db_sequenced, partner_db$StudyIDTo))
length(Reduce(intersect, list(genomic_db_sequenced, partner_db$StudyIDFrom, 
                              partner_db$StudyIDTo)))


## all
compute_cascade("all")

## msm 
compute_cascade("msm")

## idu 
compute_cascade("idu")

## hrh 
compute_cascade("hrh")

## white
compute_cascade("white")

## black
compute_cascade("black")

## asian
compute_cascade("asian")

## other
compute_cascade("other")

## hispanic
compute_cascade("hispanic")

## nonhispanic
compute_cascade("nonhispanic")

