split_named_pt_indicator_counts_by_attributes <- 
  
  function(data){
  
  return_objs <- list()
  
  return_objs[["tab_data_named_partner_indicator"]] <- 
    table(data$named_partner_indicator, exclude = NULL)
  
  ## DemoGender
  table(data$DemoGender, exclude = NULL)
  
  return_objs[["demogender"]] <-
    xtabs(~factor(data$DemoGender, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  
  
  ## DemoRace
  table(data$DemoRace, exclude = NULL)
  return_objs[["demorace"]] <-
    xtabs(~factor(data$DemoRace, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  ## Sequence
  table(data$RecodedSequenceSubtype, exclude = NULL)
  return_objs[["sequence_subtype"]] <-
  xtabs(~factor(data$RecodedSequenceSubtype, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  ## MSM
  table(data$RiskMSM, exclude = NULL)
  return_objs[["msm"]] <-
    xtabs(~factor(data$RiskMSM, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  ## Heterosexual
  table(data$RiskHeterosexual, exclude = NULL)
  return_objs[["hrh"]] <-
    xtabs(~factor(data$RiskHeterosexual, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  ## IDU
  table(data$RiskIDU, exclude = NULL)
  return_objs[["idu"]] <-
    xtabs(~factor(data$RiskIDU, exclude = NULL) +
          factor(data$named_partner_indicator, exclude = NULL)
  )
  
  ## age at HIV diagnosis
  summary(data$HIVDxAge)
  return_objs[["hivdxage"]] <-
    data %>%
    group_by(named_partner_indicator) %>%
    summarise(mean_HIVDxAge = mean(HIVDxAge, na.rm = TRUE))

  return(return_objs)
  }
