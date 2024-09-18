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
library(lubridate)
library(here)


# 0.2 Data -------------

partner_db <- readRDS("derived_data/partner_db.rds")
partner_db_non_missing_studyidto <- readRDS("derived_data/partner_db_non_missing_studyidto.rds")

genomic_db_sequenced_dt <- readRDS("derived_data/genomic_db_sequenced_dt.rds")


# 0.3 Functions -------------
source(here("utils", "compute-cascade.R"))

# =============================

# ============================
# Section 1.0: Partners named by all index cases 
  ## n(partners) = 1342
  ## n(index_cases) = 675 
# ============================

    ## num indexes who provided partner data
    indexes_who_provided_partner_data <- 
      unique((partner_db_non_missing_studyidto$StudyIDFrom))
    length(indexes_who_provided_partner_data)

    ## num partners named by indexes who provided partner data
    partners_named_by_indexes <- 
        unique((partner_db_non_missing_studyidto$StudyIDTo))
    length(partners_named_by_indexes)

    ## with how many partners were contacts attempted
    table(partner_db_non_missing_studyidto$ClientReached, exclude=NULL)
    partners_attempted_contacts <- 
        partner_db_non_missing_studyidto %>%
        filter(ClientReached %in% c(0, 1)) %>%
        pull(StudyIDTo)
    length(partners_attempted_contacts)
    length(unique(partners_attempted_contacts))

    ## how many successfully reached
    partners_reached <- 
        partner_db_non_missing_studyidto %>%
        filter(ClientReached == 1) %>%
        pull(StudyIDTo)
        length(partners_reached)
        length(unique(partners_reached))

    ## how many successfully tested
    table(partner_db_non_missing_studyidto$HIVTested, exclude=NULL)
    partners_tested <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested == 1) %>%
        select(StudyIDFrom, StudyIDTo)
    str(partners_tested)
    length(unique(partners_tested$StudyIDFrom))
    length(unique(partners_tested$StudyIDTo))

    partners_tested_client_reached <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested == 1 & ClientReached == 1) 
    str(partners_tested_client_reached)
    length(unique(partners_tested_client_reached$StudyIDFrom))
    length(unique(partners_tested_client_reached$StudyIDTo))

    ## chacteristics of those tested & client reached
    table(partners_tested_client_reached$HIVTested, exclude=NULL)
    table(partners_tested_client_reached$AlreadyIndex, exclude=NULL)
    table(partners_tested_client_reached$AcceptPCRS, exclude=NULL)
    table(partners_tested_client_reached$ReferredToHIVTest, exclude=NULL)
    table(partners_tested_client_reached$HIVTestResult_Final, exclude=NULL)
    table(partners_tested_client_reached$referredToPrEP, exclude=NULL)

    ## for partners, was timing of HIV Test before/after PCRS was offered 
        ## replace NAs
        partners_tested_client_reached <- partners_tested_client_reached %>%
        mutate(
            DatePCRSOffered = na_if(DatePCRSOffered, ""),
            DateofHIVTest_Final = na_if(DateofHIVTest_Final, "")
        )

        ## parse dates
        partners_tested_client_reached <- partners_tested_client_reached %>%
        mutate(
            DatePCRSOffered_parsed = ymd(DatePCRSOffered),
            DateofHIVTest_Final_parsed = ymd(DateofHIVTest_Final)
        )

        ## filter records
        partners_with_both_dates <- 
            partners_tested_client_reached %>% 
            filter(!is.na(DatePCRSOffered_parsed) & !is.na(DateofHIVTest_Final_parsed))

        ## determine timing
        partners_with_both_dates <- partners_with_both_dates %>%
            mutate(
                TestBeforePCRS = DateofHIVTest_Final_parsed < DatePCRSOffered_parsed,
                TestAfterPCRS = DateofHIVTest_Final_parsed >= DatePCRSOffered_parsed
            )


        ## total partners with both dates
        total_partners_with_both_dates <- nrow(partners_with_both_dates)
        total_partners_with_both_dates

        ## number of partners tested before PCRS was offered
        num_test_before_PCRS <- sum(partners_with_both_dates$TestBeforePCRS, na.rm = TRUE)
        num_test_before_PCRS

        ## number of partners tested after PCRS was offered
        num_test_after_PCRS <- sum(partners_with_both_dates$TestAfterPCRS, na.rm = TRUE)
        num_test_after_PCRS

    ## characteristics of those not tested
    partners_not_tested <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested != 1 | is.na(HIVTested)) 
    str(partners_not_tested)

    table(partners_not_tested$HIVTested, exclude=NULL)
    table(partners_not_tested$AlreadyIndex, exclude=NULL)
    table(partners_not_tested$AcceptPCRS, exclude=NULL)
    table(partners_not_tested$ReferredToHIVTest, exclude=NULL)
    table(partners_not_tested$HIVTestResult_Final, exclude=NULL)
    table(partners_not_tested$referredToPrEP, exclude=NULL)


    ## cross tabulate confirmed test with already index

        ## create confirmed testing variable
        partner_db_non_missing_studyidto <- 
        partner_db_non_missing_studyidto %>%
            mutate(ConfirmedTested = ifelse(HIVTested == 1, "Yes", "No"))

        ## xtab between confirmedtested and alreadyindex 
        count_tab <- 
            xtabs(~ factor(ConfirmedTested, exclude=NULL) + 
            factor(AlreadyIndex, exclude=NULL), 
            data = partner_db_non_missing_studyidto)
        count_tab
        prop.table(count_tab, margin = 1)

        ## 3-way xtab between confirmedtested, alreadyindex, and referredtohivtest
        count_tab_3way <- xtabs(
            ~ factor(ConfirmedTested, exclude = NULL) +
                factor(AlreadyIndex, exclude = NULL) +
                factor(ReferredToHIVTest, exclude = NULL),
            data = partner_db_non_missing_studyidto
            )
        ftable(count_tab_3way)

        ## improve formatting
        df_tab <- as.data.frame(count_tab_3way)

        # Rename the columns for clarity
        colnames(df_tab) <- c("ConfirmedTested", "AlreadyIndex", "ReferredToHIVTest", "Count")

        # View the data frame
        print(df_tab)


    ## how many partners are diagnosed and sequenced
    sum(partners_named_by_indexes %in% genomic_db_sequenced_dt$StudyID)
    sum(unique(partners_named_by_indexes) %in% unique(genomic_db_sequenced_dt$StudyID))

    ## CASCADE 
    table(partner_db_non_missing_studyidto$CanNotify, exclude=NULL)
    table(partner_db_non_missing_studyidto$ClientReached, exclude=NULL)
    table(partner_db_non_missing_studyidto$NoReachWhy, exclude=NULL)
    table(partner_db_non_missing_studyidto$HIVTested, exclude=NULL)
    table(partner_db_non_missing_studyidto$AlreadyIndex, exclude=NULL)
    table(partner_db_non_missing_studyidto$HIVTestResult_Final, exclude=NULL)
    table(partner_db_non_missing_studyidto$referredToPrEP, exclude=NULL)

# ============================= 

# ============================
# Section 2.0: Partners named by index cases who provided partner data 
  ## n(partners) = 1056
  ## n(index_cases) = 494 
# ============================