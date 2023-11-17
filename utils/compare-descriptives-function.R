# write a function to compute the descriptives on different subsets

compare_descriptives <- function(dt){
  
  gender <- table(dt$DemoGender) #gender
  gender_prop <- table(dt$DemoGender)/sum(table(dt$DemoGender))
  
  race <- table(dt$DemoRace) #race
  race_prop <- table(dt$DemoRace)/sum(table(dt$DemoRace))
  
  ethnicity <- table(dt$DemoHispanic) #ethnicity
  ethnicity_prop <- table(dt$DemoHispanic)/sum(table(dt$DemoHispanic))
  
  summ_age_at_diag <- summary(dt$HIVDxAge) #age at hiv diagnosis
  
  breaks = c(0, 2000, 2006, 2011, 2021) #year of HIV diagnosis
  labels = c("<2000", "2001-2005", "2006-2010", "2011-2020")
  
  dt[, hivdiagnosis_year_group := 
                            cut(as.numeric(substr(HIVDxDate, 1, 4)), 
                                breaks = breaks, 
                                labels = labels)]
  
  tab_age_at_diag <- table(dt$hivdiagnosis_year_group)
  tab_age_at_diag_prop <- 
    table(dt$hivdiagnosis_year_group)/sum(
    table(dt$hivdiagnosis_year_group)
  )
  
  msm <- table(dt$RiskMSM, exclude = NULL) #msm
  msm_prop <- 
    table(dt$RiskMSM, exclude = NULL)/sum(
    table(dt$RiskMSM, exclude = NULL) #msm
  )
  
  idu <- table(dt$RiskIDU, exclude = NULL) #IDU
  idu_prop <- 
    table(dt$RiskIDU, exclude = NULL)/sum(
    table(dt$RiskIDU, exclude = NULL) #IDU
  )
  
  het <- table(dt$RiskHeterosexual, exclude = NULL) #Heterosexual
  het_prop <- 
    table(dt$RiskHeterosexual, exclude = NULL)/sum(
    table(dt$RiskHeterosexual, exclude = NULL) #Heterosexual
  )
  
  
  return_objs <- list()
  return_objs[["gender"]] <- list(gender, gender_prop)
  return_objs[["race"]] <- list(race, race_prop)
  return_objs[["ethnicity"]] <- list(ethnicity, ethnicity_prop)
  return_objs[["age_at_diag"]] <- list(summ_age_at_diag, tab_age_at_diag, tab_age_at_diag_prop)
  return_objs[["msm"]] <- list(msm, msm_prop)
  return_objs[["idu"]] <- list(idu, idu_prop)
  return_objs[["het"]] <- list(het, het_prop)
  
  return(return_objs)
}