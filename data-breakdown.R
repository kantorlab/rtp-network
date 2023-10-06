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

# Identify all unique StudyIDFroms 
length(unique(partner_db$StudyIDFrom))

# Identify all unique StudyIDTos 
length(unique(partner_db$StudyIDTo))

# Identify all unique StudyIDs in the partner DB  
partner_db_el <- (cbind(partner_db$StudyIDFrom, partner_db$StudyIDTo))
dim(partner_db_el)
length(which((partner_db$StudyIDFrom) == ""))
length(which((partner_db$StudyIDTo) == ""))
non_missing_studyidto <- which(partner_db$StudyIDTo != "")
length(non_missing_studyidto)
non_missing_studyidto_partner_db_el <- partner_db_el[non_missing_studyidto,]
dim(non_missing_studyidto_partner_db_el)
partner_db_non_missing_studyidto <- partner_db[non_missing_studyidto,]
dim(partner_db_non_missing_studyidto)

# Are all studyids in genomic DB interviewed as studyidfrom
genomic_studyid_notin_studyidfrom <- 
  genomic_db$StudyID[which(!genomic_db$StudyID %in% partner_db$StudyIDFrom)]
length(genomic_studyid_notin_studyidfrom)

# How many unique study ids in the genomic database are interviewed as studyidfrom
genomic_studyid_in_studyidfrom <- genomic_db$StudyID[which(genomic_db$StudyID %in% partner_db_non_missing_studyidto$StudyIDFrom)]
length(genomic_studyid_in_studyidfrom)

genomic_studyid_notin_studyidfrom <- genomic_db$StudyID[which(!genomic_db$StudyID %in% partner_db_non_missing_studyidto$StudyIDFrom)]
length(genomic_studyid_notin_studyidfrom)

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


# Identify all unique StudyIDTos 
length(unique(partner_db$StudyIDTo))
length(unique(partner_db_non_missing_studyidto$StudyIDTo))

# Identify the union of the two above - unique index cases or partners 
length(unique(union(partner_db$StudyIDFrom, partner_db$StudyIDTo)))
length(unique(union(partner_db_non_missing_studyidto$StudyIDFrom, 
  partner_db_non_missing_studyidto$StudyIDTo)))


# Identify their intersection
length(unique(intersect(partner_db$StudyIDFrom, partner_db$StudyIDTo)))


# How many are in the intersection of Partner DB and Molecular DB?
individuals_in_partner_db_studyidfrom_or_to <- 
  which(genomic_db$StudyID %in% c(partner_db$StudyIDFrom, partner_db$StudyIDTo))

id_individuals_in_partner_db_studyidfrom_or_to <- genomic_db$StudyID[individuals_in_partner_db_studyidfrom_or_to]

length(individuals_in_partner_db_studyidfrom_or_to)

length(unique(individuals_in_partner_db_studyidfrom_or_to))



individuals_in_partner_db_non_missing_studyidto <- 
  which(genomic_db$StudyID %in% 
  c(partner_db_non_missing_studyidto$StudyIDFrom, 
  partner_db_non_missing_studyidto$StudyIDTo))

id_individuals_in_partner_db_non_missing_studyidto <- 
  genomic_db$StudyID[individuals_in_partner_db_non_missing_studyidto]

length(individuals_in_partner_db_non_missing_studyidto)

length(unique(individuals_in_partner_db_non_missing_studyidto))


# How many individuals in the Genomic DB are index cases (i.e., tose naming partners)?
sequenced_persons_naming_partners <- 
  which(genomic_db$StudyID %in% partner_db$StudyIDFrom)
length(sequenced_persons_naming_partners)

sequenced_persons_naming_partners_non_missing_studyidto <- 
  which(genomic_db$StudyID %in% partner_db_non_missing_studyidto$StudyIDFrom)
length(sequenced_persons_naming_partners_non_missing_studyidto)


# How many individuals in the Genomic DB are named partners?
sequenced_persons_as_named_partners <- 
  which(genomic_db$StudyID %in% partner_db$StudyIDTo)
length(sequenced_persons_as_named_partners)

sequenced_persons_as_named_partners_non_missing_studyidto <- 
  which(genomic_db$StudyID %in% partner_db_non_missing_studyidto$StudyIDTo)
length(sequenced_persons_as_named_partners_non_missing_studyidto)



# How many index cases appearing the partner DB named partners?
individuals_in_partner_db_studyidfrom_wo_emptyStudyIDTo <- 
  which(genomic_db$StudyID %in% partner_db_without_empty_StudyIDTo$StudyIDFrom)
length(individuals_in_partner_db_studyidfrom_wo_emptyStudyIDTo)

# How many named partners of index cases in the partner DB who named someone?
length(unique(partner_db_without_empty_StudyIDTo$StudyIDTo))


# How many unique persons are in the genomic data?
length(unique(genomic_db$StudyID))

# How many unique clusters are the individuals in?
length(unique(genomic_db$ClusteredPhyloAny)) 

# How many individuals are *not* clustered?
length(which(is.na(genomic_db$ClusteredPhyloAny)))

# How many individuals are clustered?
length(unique(genomic_db$StudyID)) - length(which(is.na(genomic_db$ClusteredPhyloAny)))

# There is a discrepancy:
## there are 904 index cases (StudyIDFrom).  
## there are 154 named partners(StudyIDTo).  
