compute_cascade <- function(cat, genomic_db){
  
  allowed_cats <- c("msm", "idu", "hrh", 
                      "white", "black", "asian", "other",
                      "hispanic", "nonhispanic")
  
  if(!cat %in% allowed_cats) {
    stop(paste0("Invalid group. Must be one of ", allowed_cats, "."))
  }
  
 if (cat == "msm"){
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & RiskMSM == "True",,]
  } else if (cat == "hrh"){
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & RiskHeterosexual == "True",,]
  } else if (cat == "idu") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & RiskIDU == "True",,]
  } else if (cat == "white") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoRace == "White",,]
  } else if (cat == "black") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoRace == "Black",,]
  } else if (cat == "asian") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoRace == "Asian",,]
  } else if (cat == "other") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoRace == "Other",,]
  } else if (cat == "hispanic") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoHispanic == "True",,]
  } else if (cat == "nonhispanic") {
    cat_in_genomic_db_who_named_partners <- 
      genomic_db[StudyID %in% index_cases_who_named_partners & DemoHispanic == "False",,]
  }

  nrow(cat_in_genomic_db_who_named_partners)
  
  partner_db_of_cat_who_named_partners <- 
    partner_db[StudyIDFrom %in% cat_in_genomic_db_who_named_partners$StudyID,,] 
  dim(partner_db_of_cat_who_named_partners)
  
  length(unique(partner_db_of_cat_who_named_partners$StudyIDFrom))
  length(unique(partner_db_of_cat_who_named_partners$StudyIDTo))
  table(partner_db_of_cat_who_named_partners$HIVTested, exclude = NULL)
  table(partner_db_of_cat_who_named_partners$ClientReached, exclude = NULL)
  
  cat_named_partners_tested_countstudyidfrom <- 
    partner_db_of_cat_who_named_partners %>%
    filter(HIVTested == 1) %>%
    pull(StudyIDFrom) %>%
    unique()
  
  length(cat_named_partners_tested_countstudyidfrom)
  
  cat_named_partners_tested_studyidto <- 
    partner_db_of_cat_who_named_partners %>%
    filter(HIVTested == 1) %>%
    pull(StudyIDTo) %>%
    unique() 
  
  length(cat_named_partners_tested_studyidto) 
  
  cat_reported_partners_in_indivcatals_dt <- 
    intersect(genomic_db_sequenced_dt$StudyID, partner_db_of_cat_who_named_partners$StudyIDTo)
  
  diag_dates_sum <- 
    sum(table(genomic_db[which(genomic_db_sequenced_dt$StudyID %in% 
                               cat_reported_partners_in_indivcatals_dt),]$HIVDxDate, exclude = NULL))
  
  seqs_table <- 
    table(genomic_db[which(genomic_db$StudyID %in% cat_reported_partners_in_indivcatals_dt),]$Sequence, exclude = NULL)

  return_objs <- list()
  return_objs[["num_cat_in_genomic_db_who_named_partners"]] <- nrow(cat_in_genomic_db_who_named_partners)
  return_objs[["dim_partner_db_of_cat_who_named_partners)"]] <- dim(partner_db_of_cat_who_named_partners)
  return_objs[["len_partner_db_of_cat_who_named_partners_StudyIDFrom"]] <- length(unique(partner_db_of_cat_who_named_partners$StudyIDFrom))
  return_objs[["len_partner_db_of_cat_who_named_partners_StudyIDTo"]] <- length(unique(partner_db_of_cat_who_named_partners$StudyIDTo))
  return_objs[["tab_partner_db_of_cat_who_named_partners_HIVTested"]] <- table(partner_db_of_cat_who_named_partners$HIVTested, exclude = NULL)
  return_objs[["tab_partner_db_of_cat_who_named_partners_ClientReached"]] <- table(partner_db_of_cat_who_named_partners$ClientReached, exclude = NULL)
  return_objs[["len_cat_named_partners_tested_countstudyidfrom"]] <- length(cat_named_partners_tested_countstudyidfrom)
  return_objs[["len_cat_named_partners_tested_studyidto"]] <- length(cat_named_partners_tested_studyidto) 
  return_objs[["cat_reported_partners_in_indivcatals_dt"]] <- cat_reported_partners_in_indivcatals_dt
  return_objs[["diag_dates_sum"]] <- diag_dates_sum
  return_objs[["seqs_table"]] <- seqs_table
  
  return(return_objs)
  
}

