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

# Filter partner_db to only rows where StudyIDTo is not missing
partner_db_non_missing_studyidto <- partner_db[which(partner_db$StudyIDTo != ""), ]
overlap_count <- length(intersect(genomic_db$StudyID, partner_db_non_missing_studyidto$StudyIDFrom))
overlap_count

# Start with those that intersect with genomic_db$StudyID and partner_db$StudyIDFrom
intersected_persons <- intersect(genomic_db$StudyID, partner_db$StudyIDFrom)

# Then filter these individuals based on missing or empty StudyIDTo in partner_db
persons_without_valid_partner <- intersected_persons[!intersected_persons %in% partner_db_non_missing_studyidto$StudyIDFrom]
length(persons_without_valid_partner)  # This should give 407

