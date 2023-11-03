# Jon's question

# Problem statement:
# number of named partners
# number of notifiable partners
# Number of partners with HIV status Newly Determined
# Number of New HIV Positive Partners
# Number of Newly determined HIV Negative Partners
# Number of HIV Negative Partners Referred to PrEP

# Libraries ------------

rm(list=ls())
library(data.table)
library(dplyr)
library(stringi)
library(network)


# Read Jon's data ------------

jon_path <- "/gpfs/data/rkantor/rtp/analyses/"
ids_of_interest_dt <- read.csv(paste0(jon_path, "indexID.csv"), header = TRUE)
ids_of_interest <- ids_of_interest_dt$x
head(ids_of_interest)


# Read data used for ID Week ------------

data_dir <- "/gpfs/data/rkantor/rtp/datasets/D51_20230512_unified"
list.files(path=data_dir)
partner_db_old <- as.data.table(read.csv(paste0(data_dir, "/ContactTracingNetwork.csv"))) 
partner_db <- as.data.table(read.csv("/gpfs/data/rkantor/rtp/shared_dir/ContactTracingNetwork20230726.csv"))
genomic_db <- as.data.table(read.csv(paste0(data_dir, "/Individuals.csv")))


# Check if all ids_of_interest are in genomic db ------------

l1 <- length(ids_of_interest)
l2 <- length(which(ids_of_interest %in% genomic_db$StudyID))
l1 == l2


# How many of the IDs of interest are interviwed (studyidfrom) in the Partner DB ------------

ids_of_interest_in_studyidfrom <- which(ids_of_interest %in% (partner_db$StudyIDFrom))
length(ids_of_interest_in_studyidfrom)


# How many of the IDs of interest are named (studyidto) in the Partner DB ------------

ids_of_interest_in_studyidto <- which(ids_of_interest %in% (partner_db$StudyIDTo))
length(ids_of_interest_in_studyidto)


# How many of the IDs of interest are both interviewed and named ------------

length(intersect(ids_of_interest_in_studyidfrom, ids_of_interest_in_studyidto)) # All of them





