# Write functions to run the regression model

predict_named_partner_indicator<- 
  # Predict Partner Naming from Molecular Clusters
  
  function(dataset, molClusterType) {
  # Model 1: DemoGender
  
  # recode DemoGender to combine XMF and not reported into one category
  dataset$DemoGender <- as.character(dataset$DemoGender)
  dataset$DemoGender[dataset$DemoGender %in% c("XMF", "")] <- "XMF_not_reported"
  dataset$DemoGender <- as.factor(dataset$DemoGender)
  
  # relevel DemoGender to make Female the reference category
  dataset$DemoGender <- relevel(dataset$DemoGender, ref = "F")
  
  #fit models
  corstr <- "exchangeable"
  
  if (molClusterType == "clusteredphyloany"){
    id = dataset$ClusteredPhyloAny
  } else if (molClusterType == "trace005"){
    id = dataset$ClusteredHIVTrace005
  } else if (molClusterType == "trace015"){
    id = dataset$ClusteredHIVTrace015
  }
  
  browser()
  m1 <- geem(named_partner_indicator ~ DemoGender, data = dataset, 
             family = "binomial", id=id, corstr=corstr)
  m1_summary <- summary(m1)
  xtabs1 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(DemoGender, exclude = NULL), data = dataset)
  
  # Model 2: DemoRace
  # relevel DemoGender to make Black the reference category
  dataset$DemoRace <- as.factor(dataset$DemoRace)
  dataset$DemoRace <- relevel(dataset$DemoRace, ref = "Black")
  
  m2 <- glm(named_partner_indicator ~ DemoRace, data = dataset, family = "binomial")
  m2_summary <- summary(m2)
  xtabs2 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(DemoRace, exclude = NULL), data = dataset)
  
  # Model 3: RecodedSequenceSubtype
  m3 <- glm(named_partner_indicator ~ as.factor(RecodedSequenceSubtype), 
            data = dataset, family = "binomial")
  m3_summary <- summary(m3)
  xtabs3 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(RecodedSequenceSubtype, exclude = NULL), data = dataset)
  
  # Model 4: RiskMSM
  dataset$RiskMSM <- as.factor(dataset$RiskMSM)
  dataset$RiskMSM <- relevel(dataset$RiskMSM, ref="False")
  
  m4 <- glm(named_partner_indicator ~ RiskMSM, data = dataset, family = "binomial")
  m4_summary <- summary(m4)
  xtabs4 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(RiskMSM, exclude = NULL), data = dataset)
  
  # Model 5: RiskHeterosexual
  dataset$RiskHeterosexual <- as.factor(dataset$RiskHeterosexual)
  dataset$RiskHeterosexual <- relevel(dataset$RiskHeterosexual, ref="False")
  
  m5 <- glm(named_partner_indicator ~ RiskHeterosexual, data = dataset, family = "binomial")
  m5_summary <- summary(m5)
  xtabs5 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(RiskHeterosexual, exclude = NULL), data = dataset)
  
  # Model 6: RiskIDU
  dataset$RiskIDU <- as.factor(dataset$RiskIDU)
  dataset$RiskIDU <- relevel(dataset$RiskIDU, ref="False")
  
  m6 <- glm(named_partner_indicator ~ RiskIDU, data = dataset, family = "binomial")
  m6_summary <- summary(m6)
  xtabs6 <- xtabs(~ factor(named_partner_indicator, exclude = NULL) + 
                    factor(RiskIDU, exclude = NULL), data = dataset)
  
  # Model 7: HIVDxAge
  m7 <- glm(named_partner_indicator ~ HIVDxAge, data = dataset, family = "binomial")
  m7_summary <- summary(m7)
  
  return(list(
    DemoGender_summary = m1_summary,
    DemoGender_xtabs = xtabs1,
    DemoRace_summary = m2_summary,
    DemoRace_xtabs = xtabs2,
    RecodedSequenceSubtype_summary = m3_summary,
    RecodedSequenceSubtype_xtabs = xtabs3,
    RiskMSM_summary = m4_summary,
    RiskMSM_xtabs = xtabs4,
    RiskHRH_summary = m5_summary,
    RiskHRH_xtabs = xtabs5,
    RiskIDU_summary = m6_summary,
    RiskIDU_xtabs = xtabs6,
    HIVDxAge_summary = m7_summary
  ))
}

### Write a function to run the converse analysis:


predict_clustering_indicator <- 
  # Predict Molecular Clustering of Named Partnerships
  
  function(dataset, clustering_type) {
  # Choose the appropriate clustering indicator variable based on the input clustering type
  if (clustering_type == "005") {
    clustering_indicator <- "indicator_005_cluster"
  } else if (clustering_type == "015") {
    clustering_indicator <- "indicator_015_cluster"
  } else if (clustering_type == "phyloany") {
    clustering_indicator <- "indicator_phyloany_cluster"
  } else {
    stop("Invalid clustering_type. Choose one of '005', '015', or 'phyloany'.")
  }
  
  # Model 1: DemoGender
  m1 <- glm(as.formula(paste(clustering_indicator, "~ DemoGender")), data = dataset, family = "binomial")
  m1_summary <- summary(m1)
  xtabs1 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + factor(DemoGender, exclude = NULL)")), data = dataset)
  
  # Model 2: DemoRace
  # relevel DemoGender to make Black the reference category
  dataset$DemoRace <- as.factor(dataset$DemoRace)
  dataset$DemoRace <- relevel(dataset$DemoRace, ref = "Black")
  
  m2 <- glm(as.formula(paste(clustering_indicator, "~ DemoRace")), 
            data = dataset, family = "binomial")

  m2_summary <- summary(m2)
  xtabs2 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + 
                                   factor(DemoRace, exclude = NULL)")), data = dataset)
  
  # Model 3: RecodedSequenceSubtype
  # relevel RecodedSequenceSubtype to make B the reference category
  dataset$RecodedSequenceSubtype <- as.factor(dataset$RecodedSequenceSubtype)
  dataset$RecodedSequenceSubtype <- relevel(dataset$RecodedSequenceSubtype, ref = "B")
  
  m3 <- glm(as.formula(paste(clustering_indicator, "~ RecodedSequenceSubtype")), 
            data = dataset, family = "binomial")
  m3_summary <- summary(m3)
  xtabs3 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + 
                                   factor(RecodedSequenceSubtype, exclude = NULL)")), data = dataset)
  
  # Model 4: RiskMSM
  # relevel to make non-MSM the reference category
  dataset$RiskMSM <- as.factor(dataset$RiskMSM)
  dataset$RiskMSM <- relevel(dataset$RiskMSM, ref = "False")
  
  m4 <- glm(as.formula(paste(clustering_indicator, "~ RiskMSM")), 
            data = dataset, family = "binomial")
  m4_summary <- summary(m4)
  xtabs4 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + 
                                   factor(RiskMSM, exclude = NULL)")), data = dataset)
  
  # Model 5: RiskHRH
  # relevel to make non-HRH the reference category
  dataset$RiskHeterosexual <- as.factor(dataset$RiskHeterosexual)
  dataset$RiskHeterosexual <- relevel(dataset$RiskHeterosexual, ref = "False")
  
  m5 <- glm(as.formula(paste(clustering_indicator, "~ RiskHeterosexual")), 
            data = dataset, family = "binomial")
  m5_summary <- summary(m5)
  xtabs5 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + 
                                   factor(RiskHeterosexual, exclude = NULL)")), data = dataset)
  
  # Model 6: RiskIDU
  # relevel to make non-HRH the reference category
  dataset$RiskIDU <- as.factor(dataset$RiskIDU)
  dataset$RiskIDU <- relevel(dataset$RiskIDU, ref = "False")
  
  m6 <- glm(as.formula(paste(clustering_indicator, "~ RiskIDU")), 
            data = dataset, family = "binomial")
  m6_summary <- summary(m6)
  xtabs6 <- xtabs(as.formula(paste("~ factor(", clustering_indicator, ", exclude = NULL) + 
                                   factor(RiskIDU, exclude = NULL)")), data = dataset)
  
  # Model 7: HIVDxAge
  m7 <- glm(as.formula(paste(clustering_indicator, "~ HIVDxAge")), 
            data = dataset, family = "binomial")
  m7_summary <- summary(m7)
  hivdxage_summary <- tapply(dataset$HIVDxAge, dataset[[clustering_indicator]], summary)
  
  return(list(
    DemoGender_summary = m1_summary,
    DemoGender_xtabs = xtabs1,
    DemoRace_summary = m2_summary,
    DemoRace_xtabs = xtabs2,
    RecodedSequenceSubtype_summary = m3_summary,
    RecodedSequenceSubtype_xtabs = xtabs3,
    RiskMSM_summary = m4_summary,
    RiskMSM_xtabs = xtabs4,
    RiskHRH_summary = m5_summary,
    RiskHRH_xtabs = xtabs5,
    RiskIDU_summary = m6_summary,
    RiskIDU_xtabs = xtabs6,
    HIVDxAge_summary = m7_summary
  ))
}




