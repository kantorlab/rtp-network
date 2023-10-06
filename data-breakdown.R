rm(list=ls())
library(data.table)
library(dplyr)
library(stringi)
library(network)

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
partner_db_old <- as.data.table(read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))) 
partner_db <- as.data.table(read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv"))
genomic_db <- as.data.table(read.csv(paste0(data_dir, "/Individuals.csv")))

# compute intersections betwee genomic db and partnerdb
length(intersect(genomic_db$StudyID, partner_db$StudyIDFrom))
length(intersect(genomic_db$StudyID, partner_db$StudyIDTo))
length(intersect(partner_db$StudyIDFrom, partner_db$StudyIDTo))
length(Reduce(intersect, list(genomic_db$StudyID, partner_db$StudyIDFrom, partner_db$StudyIDTo)))

# How many of the interviewed index cases who are also in the genomic DB provided identifiable partner data?
partner_db_non_missing_studyidto <- partner_db[which(partner_db$StudyIDTo != ""), ]
index_cases_who_named_partners <- intersect(genomic_db$StudyID, partner_db_non_missing_studyidto$StudyIDFrom)
length(index_cases_who_named_partners)

# How many of the interviewed index cases who are also in the genomic DB did not provide identifiable partner data?
intersected_persons_partner_genomic_db <- intersect(genomic_db$StudyID, partner_db$StudyIDFrom)
intersected_persons_without_named_partner <- intersected_persons_partner_genomic_db[!intersected_persons_partner_genomic_db %in% partner_db_non_missing_studyidto$StudyIDFrom]
length(intersected_persons_without_named_partner)  

# How many total partners are named by the 497 index cases in the genomic DB who named partners?
length(unique(partner_db_non_missing_studyidto$StudyIDTo))