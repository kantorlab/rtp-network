# =====================================================================
# Title: Computing database overlap

# Description: 
  # Consolidate second section of the results           

# =====================================================================


# ============================
# Section 0.0: Load libraries and Read Data 
# ============================

# 0.1 Libraries -------------
rm(list=ls())
library(data.table)
library(dplyr)
library(here)


# 0.2 Data -------------

partner_db <- readRDS("derived_data/partner_db.rds")
partner_db_non_missing_studyidto <- readRDS("derived_data/partner_db_non_missing_studyidto.rds")

genomic_db_sequenced_dt <- readRDS("derived_data/genomic_db_sequenced_dt.rds")


# 0.3 Functions -------------
source(here("utils", "compare-descriptives-function.R"))

# =====================================================================


# ============================
# Section 1.0: Person Overlap
# ============================

genomic_db_sequenced_id <- which(genomic_db_sequenced_dt$Sequence == "True")
genomic_db_sequenced <- genomic_db_sequenced_dt$StudyID[genomic_db_sequenced_id]
length(genomic_db_sequenced)

# Compute intersections between sequenced persons in genomic db and partnerdb
common_persons <- intersect(genomic_db_sequenced, partner_db$StudyIDFrom)
n_common_persons <- length(common_persons)

# % of unique in GDB
unique_persons_in_gdb <- unique(genomic_db_sequenced)
length(unique_persons_in_gdb)
n_common_persons/length(unique_persons_in_gdb)

# % of unique in CTDB
unique_studyidfrom_ctdb <- unique(partner_db$StudyIDFrom)
unique_studyidto_ctdb <- unique(partner_db$StudyIDTo)
unique_all_ctdb <- 
    unique(
        c(unique_studyidfrom_ctdb, 
        unique_studyidto_ctdb)) #total number of unique persons in CTDB
length(unique_all_ctdb)

n_common_persons/length(unique_all_ctdb)

# =====================================================================

# ============================
# Section 2.0: Link Overlap
# ============================

## see https://github.com/kantorlab/rtp-network/blob/4cf5d635ffee33899e41bcdb9c7883b290d54a68/molecular-cluster-analysis.R#L521-L567

# =====================================================================


# ============================
# Section 3.0: Jaccard Coefficients
# ============================

## person level
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

## link level
   ## Compute Jaccard coefficient (link-level) ---------
   ## See https://github.com/kantorlab/rtp-network/blob/138e5f9b0c80e103f1cd383099c4b04ca757d948/molecular-cluster-analysis.R#L521-L567

# =====================================================================


# ============================
# Section 4.0: Demographic/Behavioral Characteristics at IncreasinglyGranular Overlaps
# ============================

## Identify overlapping groups

    ## Number of unique persons in GDB
    length(unique_persons_in_gdb)

    ## Number of common persons between the two databases
    n_common_persons

    ## Number of common persons who provided partner data
    common_who_provided_partner_data <- 
        intersect(genomic_db_sequenced, partner_db_non_missing_studyidto$StudyIDFrom)
    
    length(common_who_provided_partner_data)

    # Partners named by the index cases in the genomic DB who named partners
        ## All partners
        index_cases_who_named_partners <- intersect(genomic_db_sequenced, partner_db_non_missing_studyidto$StudyIDFrom)
        length(index_cases_who_named_partners)
        
        row_id_sequenced_index_cases_who_named_pts <- which(partner_db_non_missing_studyidto$StudyIDFrom %in% index_cases_who_named_partners)
        
        n_total_pts_named_by_sequenced_index_cases_who_named_pts <- (partner_db_non_missing_studyidto$StudyIDTo[row_id_sequenced_index_cases_who_named_pts])
        length(n_total_pts_named_by_sequenced_index_cases_who_named_pts) #total
        
        length(unique(n_total_pts_named_by_sequenced_index_cases_who_named_pts))

        ## Partners Named by Index Cases who appeared in both databses
           partners_named_by_index_cases_in_gdb <- 
            intersect(
            unique(n_total_pts_named_by_sequenced_index_cases_who_named_pts), 
            unique_persons_in_gdb
            )
            
            length(partners_named_by_index_cases_in_gdb)
                
## Compute Behavioral Characteristics (Table 1)
    ## All persons in GDB
    length(unique(genomic_db_sequenced_dt$StudyID))
    compare_descriptives(dt=genomic_db_sequenced_dt)

    ## individuals in gdb and ctdb (n=894)
    head(common_persons)
    common_persons_sequenced_id <- which(genomic_db_sequenced %in% common_persons)
    common_persons_sequenced_dt <- genomic_db_sequenced_dt[common_persons_sequenced_id,] 

    compare_descriptives(dt=common_persons_sequenced_dt)

    ## common persons who provided partner data
    common_persons_who_provided_partner_data_sequenced_id <- 
        which(genomic_db_sequenced %in% common_who_provided_partner_data)
    length(common_persons_who_provided_partner_data_sequenced_id)
    common_persons_who_provided_partner_data_sequenced_dt <- 
        genomic_db_sequenced_dt[common_persons_who_provided_partner_data_sequenced_id,] 
    dim(common_persons_who_provided_partner_data_sequenced_dt)

    compare_descriptives(dt=common_persons_who_provided_partner_data_sequenced_dt)

    ## partners named by inde cases who appeared in both databses
    partners_named_by_index_cases_in_gdb_sequenced_id <- 
        which(genomic_db_sequenced %in% partners_named_by_index_cases_in_gdb)
    length(partners_named_by_index_cases_in_gdb_sequenced_id)
    partners_named_by_index_cases_in_gdb_sequenced_dt <- 
        genomic_db_sequenced_dt[partners_named_by_index_cases_in_gdb_sequenced_id,]
    dim(partners_named_by_index_cases_in_gdb_sequenced_dt)

    compare_descriptives(dt=partners_named_by_index_cases_in_gdb_sequenced_dt)

# ============================
# Section 5.0: Phylogenetic Clustering at Incresingly Granular Overlaps
# ============================
