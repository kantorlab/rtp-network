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
l2 <- length(intersect(ids_of_interest, genomic_db$StudyID))
l1 == l2


# How many of the IDs of interest are interviwed (studyidfrom) in the Partner DB ------------

ids_of_interest_in_studyidfrom <- intersect(ids_of_interest, (partner_db$StudyIDFrom))
length(ids_of_interest_in_studyidfrom)


# How many of the IDs of interest are named (studyidto) in the Partner DB ------------

ids_of_interest_in_studyidto <- intersect(ids_of_interest, partner_db$StudyIDTo)
length(ids_of_interest_in_studyidto)


# How many of the IDs of interest are both interviewed and named ------------

length(intersect(ids_of_interest_in_studyidfrom, ids_of_interest_in_studyidto))# All of them


# How many partners are named by the IDs of interest ------------

partner_db_ids_of_interest <- partner_db[StudyIDFrom %in% ids_of_interest,,]
dim(partner_db_ids_of_interest)  
length(which(partner_db_ids_of_interest$StudyIDTo == ""))

partner_db_ids_of_interest_wo_empty_studyidto <-partner_db_ids_of_interest[StudyIDTo != "",,]
dim(partner_db_ids_of_interest_wo_empty_studyidto)


# Now look at Jon's questions ------------

# number of named partners
length(unique(partner_db_ids_of_interest_wo_empty_studyidto$StudyIDTo))

# number of notifiable partners
table(partner_db_ids_of_interest_wo_empty_studyidto$CanNotify, exclude = NULL)
## 1 = yes
table(partner_db_ids_of_interest_wo_empty_studyidto$CanNotify, exclude = NULL)/
  sum(table(partner_db_ids_of_interest_wo_empty_studyidto$CanNotify, exclude = NULL))

# Number of partners with HIV status Newly Determined
table(partner_db_ids_of_interest_wo_empty_studyidto$HIVTested, exclude = NULL) #1=tested for HIV
table(partner_db_ids_of_interest_wo_empty_studyidto$HIVTestResult_Final, exclude = NULL) #seems to be 0

# Number of New HIV Positive Partners
  ## same as above?

# Number of Newly determined HIV Negative Partners
table(partner_db_ids_of_interest_wo_empty_studyidto$HIVTestResult_Final, 
      exclude = NULL) #seems to be 0
    ## i need to check what code 12 indicates, which 2 people have

# Number of HIV Negative Partners Referred to PrEP
table(partner_db_ids_of_interest_wo_empty_studyidto$referredToPrEP, 
      exclude = NULL) 
   # 2 named partners have non-negative entries, 1 seems to be referred to PrEP
