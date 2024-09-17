# =====================================================================
# Title: 
  ## Computing partner naming cascades, among all named partners 
  ## and all partners named by index cases who provided partner data 

# Description: 
  ## Consolidate third section of the results           

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

# =====================================================================
