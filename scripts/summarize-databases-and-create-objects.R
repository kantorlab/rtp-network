# =====================================================================
# Title: Data Processing for Contact Tracing and Genomic Databases

# Description: 
  # This script analyzes data in the first paragraph of the results.
  # Also creates objects to be used in the later analysis.            

# =====================================================================


rm(list=ls())

# Libraries -------------

library(data.table)
library(dplyr)
library(stringi)
library(lubridate)

# ============================
# Section 0.0: Read Data and Functions
# ============================

# 0.1 Datasets -------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
partner_db_old <- as.data.table(read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))) 
partner_db <- as.data.table(read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv"))
genomic_db <- as.data.table(read.csv(paste0(data_dir, "/Individuals.csv")))

# ============================


# ============================
# Section 1.0: Partner Contact Tracing Database 
# ============================


  dim(partner_db)
  glimpse(partner_db)

  n_unique_studyidfrom <- unique(partner_db$StudyIDFrom)
  n_unique_studyidto <- unique(partner_db$StudyIDTo)

  length(n_unique_studyidfrom) #persons interviewed as unique cases
  length(n_unique_studyidto) #persons interviewed as unique named partners

  length(unique(c(n_unique_studyidfrom, n_unique_studyidto))) #total number of unique persons in CTDB

  ## how many study ids from provided identifiable partner data (i.e., non-blank study id to)?
  partner_db_non_missing_studyidto <- partner_db[which(partner_db$StudyIDTo != ""), ]
  dim(partner_db_non_missing_studyidto)

  ## how many index cases provided identifiable partner data
  length(unique((partner_db_non_missing_studyidto$StudyIDFrom)))

  ## how many index cases did not provide identifiable partner data at at least one interview
  dt_with_unnamed_partners <- partner_db[which(partner_db$StudyIDTo == ""), ]
  length(unique(dt_with_unnamed_partners$StudyIDFrom))
 

  # how many index cases did not name partners at any of their interviews
  unique_studyidfrom_non_missing <- unique(partner_db_non_missing_studyidto$StudyIDFrom)
  unique_studyidfrom_unnamed <- unique(dt_with_unnamed_partners$StudyIDFrom)
  index_cases_no_partners <- setdiff(unique_studyidfrom_unnamed, unique_studyidfrom_non_missing)

  length(index_cases_no_partners)

  length(index_cases_no_partners)+length(unique((partner_db_non_missing_studyidto$StudyIDFrom)))

  ## how many unique partners named
  length(unique((partner_db_non_missing_studyidto$StudyIDTo)))

  n_unique_studyidfrom_non_missing_studyidto <- unique(partner_db_non_missing_studyidto$StudyIDFrom)
  n_unique_studyidto_non_missing_studyidto <- unique(partner_db_non_missing_studyidto$StudyIDTo)

  length(c(n_unique_studyidfrom_non_missing_studyidto))
  length(c(n_unique_studyidto_non_missing_studyidto))

  length(
    unique(
      c(n_unique_studyidfrom_non_missing_studyidto,
        n_unique_studyidto_non_missing_studyidto
      )
    )
  )

  ## date of referral 
  tab_yr_referral <- 
    table(substr(partner_db_non_missing_studyidto$DateofReferral, 1, 4), exclude = NULL)


  summary(as.numeric(tab_yr_referral))
  mean(tab_yr_referral)

  ## compute number of index cases and named partners per year
    partner_db_non_missing_studyidto <- partner_db_non_missing_studyidto %>%
      mutate(Year = substr(DateofReferral, 1, 4))

    ### count unique StudyIDFrom for each year (index cases)
    index_cases_per_year <- partner_db_non_missing_studyidto %>%
      group_by(Year) %>%
      summarize(IndexCases = n_distinct(StudyIDFrom))

    ### count unique StudyIDTo for each year (named partners)
    named_partners_per_year <- partner_db_non_missing_studyidto %>%
      group_by(Year) %>%
      summarize(NamedPartners = n_distinct(StudyIDTo))

    ### merge the results
    yearly_summary <- index_cases_per_year %>%
      left_join(named_partners_per_year, by = "Year")

    ### print the summary
    print(yearly_summary)
    
    summary(yearly_summary$IndexCases) #Index Cases 
    yearly_summary$Year[which.min(yearly_summary$IndexCases)]
    yearly_summary$Year[which.max(yearly_summary$IndexCases)]

    summary(yearly_summary$NamedPartners) #Named Partners
    yearly_summary$Year[which.min(yearly_summary$NamedPartners)]
    yearly_summary$Year[which.max(yearly_summary$NamedPartners)]

  ## How many partner naming links?
   ## Computed at https://github.com/kantorlab/rtp-network/blob/1746d195fcd30880a79b6c9592145fc79f3c1077/molecular-cluster-analysis.R#L540-L549
   ## The answer is 1373, adjusting for edges found at multiple interview points.   


  ## Distribution of size and number of partner naming clusters
  ### see https://github.com/kantorlab/rtp-network/blob/b87b4ddbb253b21d620eb69e1b2edccd0ae5f36d/molecular-cluster-analysis.R#L570-L586

  ## one person who was named as partner but not interviewed as index
  n_unique_studyidfrom <- unique(partner_db$StudyIDFrom)
  n_unique_studyidto <- unique(partner_db$StudyIDTo)
  n_unique_ctdb <- unique(c(n_unique_studyidfrom, n_unique_studyidto))

  length(n_unique_studyidfrom)
  length(n_unique_studyidto)
  length(n_unique_ctdb)

  unique_ctdb_not_in_from <- setdiff(n_unique_ctdb, n_unique_studyidfrom)
  unique_ctdb_not_in_from



# ============================
# Section 2.0: Genomic Database
# ============================

  # How many persons in the Genomic DB are sequenced?
  table(genomic_db$Sequence, exclude=NULL)
  genomic_db_sequenced_id <- which(genomic_db$Sequence == "True")
  length(genomic_db_sequenced_id)
  genomic_db_sequenced <- genomic_db$StudyID[genomic_db_sequenced_id]
  genomic_db_sequenced_dt <- genomic_db[genomic_db_sequenced_id,]
  dim(genomic_db_sequenced_dt)

  # How many sequenced persons are in a molecular cluster

  n_not_in_cluster <- 
    length(which(is.na(genomic_db_sequenced_dt$ClusteredPhyloAny)))

  n_present_in_cluster <- nrow(genomic_db_sequenced_dt) - n_not_in_cluster

  n_present_in_cluster

  # Year of Diagnosis
  genomic_db_sequenced_dt$HIVDxDate <- ymd(genomic_db_sequenced_dt$HIVDxDate)

  ## extract year, month, day
  genomic_db_sequenced_dt$derived_HIVDX_year <- year(genomic_db_sequenced_dt$HIVDxDate)
  genomic_db_sequenced_dt$derived_HIVDX_month <- month(genomic_db_sequenced_dt$HIVDxDate)
  genomic_db_sequenced_dt$derived_HIVDX_day <- day(genomic_db_sequenced_dt$HIVDxDate) 

  table(genomic_db_sequenced_dt$derived_HIVDX_year)
  summary(genomic_db_sequenced_dt$derived_HIVDX_year)

######################

# ============================
# Section 3.0: Data Management
# ============================

# Create 'derived_data' directory if it doesn't exist
if (!dir.exists("derived_data")) {
  dir.create("derived_data")
}

# Save the processed data objects

saveRDS(partner_db, file = "derived_data/partner_db.rds")
saveRDS(partner_db_non_missing_studyidto, file = "derived_data/partner_db_non_missing_studyidto.rds")
saveRDS(genomic_db_sequenced_dt, file = "derived_data/genomic_db_sequenced_dt.rds")

######################
