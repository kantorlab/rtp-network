# =====================================================================
# Title: 
  ## Computing Cascades:
  ## Overall:  (1) among all named partners 
  ##           (2) among partners named by index cases who provided partner data 
  ## Stratified:(1) by behavior, race and ethnicity among all named partners 

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
genomic_db_sequenced <- genomic_db_sequenced_dt$StudyID


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
    length(partner_db_non_missing_studyidto$StudyIDTo)

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
    length(partners_reached)

    ## how many referred for testing
    table(partner_db_non_missing_studyidto$ReferredToHIVTest, exclude=NULL)
    unique_partners_referred_to_hiv_test <-
        partner_db_non_missing_studyidto %>%
        filter(ReferredToHIVTest == 1) %>%
        distinct(StudyIDTo) %>%
        pull()
    length(unique_partners_referred_to_hiv_test)

    ## how many successfully tested
    table(partner_db_non_missing_studyidto$HIVTested, exclude=NULL)
    partners_tested <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested == 1) %>%
        select(StudyIDFrom, StudyIDTo)
    str(partners_tested)
    length(unique(partners_tested$StudyIDFrom))
    length((partners_tested$StudyIDTo))


    partners_tested_client_reached <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested == 1 & ClientReached == 1) 
    str(partners_tested_client_reached)
    length(unique(partners_tested_client_reached$StudyIDFrom))
    length(unique(partners_tested_client_reached$StudyIDTo))

    ## final hiv test
    table(partner_db_non_missing_studyidto$HIVTestResult_Final, exclude=NULL)
    length(which(!is.na(partner_db_non_missing_studyidto$HIVTestResult_Final))) #agrees with meghan's estimate, email: 12/06/2024
     ## per meghan's email (12/06/2024), these are coded as:
        # 3: Rapid test negative – Our DIS used to do rapid tests at RIDOH for any partners that wanted to come in but we stopped this once COVID hit and have not picked the activity back up since
        # 6: HIV-1 positive
        # 11: HIV-1 negative
        # 12: HIV negative    

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

        partners_with_both_dates$StudyIDTo
        

    ## characteristics of those not tested
    partners_not_tested <- 
        partner_db_non_missing_studyidto %>%
        filter(HIVTested != 1 | is.na(HIVTested)) 
    str(partners_not_tested)

    table(partners_not_tested$HIVTested, exclude=NULL)
    table(partners_not_tested$AlreadyIndex, exclude=NULL)
    table(partners_not_tested$AcceptPCRS, exclude=NULL)
    table(partners_not_tested$ReferredToHIVTest, exclude=NULL)
    table(partners_not_tested$HIVTestResult_Final, exclude=NULL); sum(table(partners_not_tested$HIVTestResult_Final, exclude=NULL))
    table(partners_not_tested$referredToPrEP, exclude=NULL)

    ## What about the HIV status for the partners who were reached but not tested?
        ## Filter data on partners not tested but reached
        partners_not_tested_clientreached1 <- 
            partners_not_tested %>%
                filter(ClientReached == 1)
        dim(partners_not_tested_clientreached1)
    
        ## Check the HIV test result for these partners is in the CTDB
        table(partners_not_tested_clientreached1$HIVTestResult_Final, exclude=NULL)

        ## Check if these partners are already recorded as index cases in the CTDB
        table(partners_not_tested_clientreached1$AlreadyIndex, exclude=NULL)
        
        ## Check if these partners are known and recorded in the sequenced dataset 
        sum(partners_not_tested_clientreached1$StudyIDFrom %in% genomic_db_sequenced)
        sum(partners_not_tested_clientreached1$StudyIDTo %in% genomic_db_sequenced)
        length(genomic_db_sequenced)

        sum(partners_not_tested_clientreached1$StudyIDFrom %in% genomic_db_sequenced)/
            nrow(partners_not_tested_clientreached1)

        
        sum(partners_not_tested_clientreached1$StudyIDTo %in% genomic_db_sequenced)/
            nrow(partners_not_tested_clientreached1)

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

    ## compare dates of diagnosis relative to the date PCRS was offered for named partners who are diagnosed and sequenced
        ## set up
        partners_diagnosed_sequenced <- partners_named_by_indexes[which(partners_named_by_indexes %in% genomic_db_sequenced_dt$StudyID)]
        length(partners_diagnosed_sequenced)
        partners_diagnosed_sequenced
        pt_data_on_pts_diagnosed_sequenced <- partner_db_non_missing_studyidto[which(partner_db_non_missing_studyidto$StudyIDTo %in% partners_diagnosed_sequenced),]
        dim(pt_data_on_pts_diagnosed_sequenced) #181 total partner rows corresponding to nominations by index cases 
        unique(pt_data_on_pts_diagnosed_sequenced$StudyIDTo) #152 unique partners
        pt_data_on_pts_diagnosed_sequenced$DatePCRSOffered


        pt_seq_diagnosed_gdb_id_dt <- genomic_db_sequenced_dt[which(genomic_db_sequenced_dt$Study %in% partners_diagnosed_sequenced),]
        table(pt_seq_diagnosed_gdb_id_dt$HIVDxDate) #date of HIV diagnosis

        # merge the two datasets on the partner ID
        merged_data <- merge(
        pt_data_on_pts_diagnosed_sequenced,
        pt_seq_diagnosed_gdb_id_dt,
        by.x = 'StudyIDTo',
        by.y = 'StudyID'
        )

        dim(merged_data)

        # replace empty strings with NA in DatePCRSOffered
        merged_data$DatePCRSOffered[merged_data$DatePCRSOffered == ""] <- NA

        # convert DatePCRSOffered and HIVDxDate to Date format
        merged_data$DatePCRSOffered <- as.Date(merged_data$DatePCRSOffered, format = '%Y-%m-%d')
        head(merged_data$DatePCRSOffered, 25)
        
        merged_data$HIVDxDate <- as.Date(merged_data$HIVDxDate, format = '%Y-%m-%d')
        head(merged_data$HIVDxDate, 25)


        # Calculate the time difference in days
        merged_data$TimeDiff <- as.numeric(merged_data$HIVDxDate - merged_data$DatePCRSOffered)
        summary(merged_data$TimeDiff)
        length(which(!is.na(merged_data$TimeDiff)))

        length(which(!is.na(merged_data$TimeDiff)))
        length(which(merged_data$TimeDiff > 0))
        length(which(merged_data$TimeDiff < 0))
        length(which(merged_data$TimeDiff == 0))

        # Summary of the time differences
        summary(merged_data$TimeDiff)

        # Number of partners diagnosed before PCRS was offered
        diagnosed_before_pcrs <- sum(merged_data$TimeDiff < 0, na.rm = TRUE)

        # Number of partners diagnosed after PCRS was offered
        diagnosed_after_pcrs <- sum(merged_data$TimeDiff > 0, na.rm = TRUE)

        # Number of partners diagnosed on the same day as PCRS was offered
        diagnosed_on_same_day <- sum(merged_data$TimeDiff == 0, na.rm = TRUE)

        # Output the results
        cat("Number of partners diagnosed before PCRS was offered:", diagnosed_before_pcrs, "\n")
        cat("Number of partners diagnosed after PCRS was offered:", diagnosed_after_pcrs, "\n")
        cat("Number of partners diagnosed on the same day as PCRS was offered:", diagnosed_on_same_day, "\n")





    ## Other variables of interest
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


# How many of the interviewed index cases who are also in the genomic DB provided identifiable partner data?
index_cases_who_named_partners <- intersect(genomic_db_sequenced,
     partner_db_non_missing_studyidto$StudyIDFrom)
length(index_cases_who_named_partners)

# How many of the interviewed index cases who are also in the genomic DB did not provide identifiable partner data?
intersected_persons_partner_genomic_db <- intersect(genomic_db_sequenced, partner_db$StudyIDFrom)
intersected_persons_without_named_partner <- intersected_persons_partner_genomic_db[!intersected_persons_partner_genomic_db %in% partner_db_non_missing_studyidto$StudyIDFrom]
length(intersected_persons_without_named_partner)  

# How many total partners are named by the index cases in the genomic DB who named partners?
row_id_sequenced_index_cases_who_named_pts <- 
    which(partner_db_non_missing_studyidto$StudyIDFrom %in% index_cases_who_named_partners)
n_total_pts_named_by_sequenced_index_cases_who_named_pts <- (partner_db_non_missing_studyidto$StudyIDTo[row_id_sequenced_index_cases_who_named_pts])
length(n_total_pts_named_by_sequenced_index_cases_who_named_pts) #total
length(unique(n_total_pts_named_by_sequenced_index_cases_who_named_pts))

## Partner naming cascade for the persons who are named by the 494 overlapping persons between CTDB and GDB
unique_n_total_pts_named_by_sequenced_index_cases_who_named_pts <- 
    unique(n_total_pts_named_by_sequenced_index_cases_who_named_pts)
row_id_unique_n_total_pts_named_by_sequenced_index_cases_who_named_pts <-
  which(partner_db_non_missing_studyidto$StudyIDTo %in% unique_n_total_pts_named_by_sequenced_index_cases_who_named_pts)
length(row_id_unique_n_total_pts_named_by_sequenced_index_cases_who_named_pts)

pt_dt_named_pt_of_494 <- 
  partner_db_non_missing_studyidto[row_id_unique_n_total_pts_named_by_sequenced_index_cases_who_named_pts,]
dim(pt_dt_named_pt_of_494)
table(pt_dt_named_pt_of_494$HIVTested, exclude=NULL)


## What about the HIV status for the partners who were reached but not tested?
    table(pt_dt_named_pt_of_494$HIVTested, exclude=NULL)
    length(c(which(pt_dt_named_pt_of_494$HIVTested != 1), which(is.na(pt_dt_named_pt_of_494$HIVTested))))

    ## Filter data on partners reached
    pt_dt_named_pt_of_494clientreached1 <- 
        pt_dt_named_pt_of_494  %>%
            filter(ClientReached == 1)
    table(pt_dt_named_pt_of_494clientreached1$ClientReached, exclude=NULL)
    table(pt_dt_named_pt_of_494clientreached1$HIVTested, exclude=NULL)

    ## Filter data on partners not tested but reached
    pt_dt_named_pt_of_494_nottested_clientreached1 <- 
       pt_dt_named_pt_of_494clientreached1  %>%
            filter(HIVTested != 1 | is.na(HIVTested)) 
    dim(pt_dt_named_pt_of_494_nottested_clientreached1)
    
    table(pt_dt_named_pt_of_494_nottested_clientreached1$HIVTested, exclude=NULL)
    
    length(c(which(pt_dt_named_pt_of_494_nottested_clientreached1$HIVTested != 1), 
            which(is.na(pt_dt_named_pt_of_494_nottested_clientreached1$HIVTested))))



    ## Check the HIV test result for these partners is in the CTDB
    table(pt_dt_named_pt_of_494_nottested_clientreached1$HIVTestResult_Final, exclude=NULL)

    ## Check if these partners are already recorded as index cases in the CTDB
    table(pt_dt_named_pt_of_494_nottested_clientreached1$AlreadyIndex, exclude=NULL)
    
    ## Check if these partners are known and recorded in the sequenced dataset 
    sum(pt_dt_named_pt_of_494_nottested_clientreached1$StudyIDFrom %in% genomic_db_sequenced)
    sum(pt_dt_named_pt_of_494_nottested_clientreached1$StudyIDTo %in% genomic_db_sequenced)
    length(genomic_db_sequenced)

    sum(pt_dt_named_pt_of_494_nottested_clientreached1$StudyIDFrom %in% genomic_db_sequenced)/
        nrow(pt_dt_named_pt_of_494_nottested_clientreached1)

    sum(pt_dt_named_pt_of_494_nottested_clientreached1$StudyIDTo %in% genomic_db_sequenced)/
        nrow(pt_dt_named_pt_of_494_nottested_clientreached1)

## Cascade
table(pt_dt_named_pt_of_494$CanNotify, exclude=NULL)
table(pt_dt_named_pt_of_494$ClientReached, exclude=NULL)
table(pt_dt_named_pt_of_494$AcceptPCRS, exclude=NULL)  
table(pt_dt_named_pt_of_494$NoReachWhy, exclude=NULL)
table(pt_dt_named_pt_of_494$ReferredToHIVTest, exclude=NULL)
table(pt_dt_named_pt_of_494$HIVTested, exclude=NULL)
table(pt_dt_named_pt_of_494$AlreadyIndex, exclude=NULL)
table(pt_dt_named_pt_of_494$HIVTestResult_Final, exclude=NULL)
table(pt_dt_named_pt_of_494$referredToPrEP, exclude=NULL)

## partners named by index cases who appeared in both databses (n=121)
### See here https://github.com/kantorlab/rtp-network/blob/d40ab6379a70dd87f0869b5d2a6c25294a57e183/scripts/database-overlap.R#L203-L209

# ============================
# Section 3.0: Stratified Cascades 
# ============================

## msm 
compute_cascade("msm", genomic_db = genomic_db_sequenced_dt)

## idu 
compute_cascade("idu", genomic_db = genomic_db_sequenced_dt)

## hrh 
compute_cascade("hrh", genomic_db = genomic_db_sequenced_dt)

## white
compute_cascade("white", genomic_db = genomic_db_sequenced_dt)

## black
compute_cascade("black", genomic_db = genomic_db_sequenced_dt)

## asian
compute_cascade("asian", genomic_db = genomic_db_sequenced_dt)

## other
compute_cascade("other", genomic_db = genomic_db_sequenced_dt)

## hispanic
compute_cascade("hispanic", genomic_db = genomic_db_sequenced_dt)

## nonhispanic
compute_cascade("nonhispanic", genomic_db = genomic_db_sequenced_dt)

