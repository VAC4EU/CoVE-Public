
#Needed libraries

# ---- libraries ---- #

#Get population
population <- readRDS(paste0(d3Dir,"D3_HO_P_TO_HE_P_analytic_dataset.rds"))[matched == T,]

#Add a column to distinct between adolescents and adults for the stratification
population <- population[, agegroup := fifelse(ageband_at_matching_date == "12-17", "Adolescents", "Adults")]


#RE What is the aim of these varaibles? They are also in myvars and and not al these varaibles are suited to calculate a percentage?
myselectedvars <- c("age_at_matching_date", "sex_at_instance_creation", "flu_vax_prev_5years_at_matching_date", "num_contacts_yearbefore_at_matching_date")
###



###RECreate a variable with only the variables that have a 0/1
prevVars <- paste0(tolower(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == TRUE & TYPE == "TF" ,][["STUDY_VARIABLES"]]), "_at_matching_date")
prevVars <- prevVars[prevVars %in% colnames(population)]
preVars <- c(prevVars, "flu_vax_prev_5years_at_matching_date")

##RE What is the aim of these variables? All the variables in valid_vars_full_cor are also in valid_vars_no_cor? This overlap I do not grasp given the naming of the variable
##REAnd what to do if other type of varaibles then 0/1
valid_vars_no_cor <- vector()
valid_vars_reduced_cor <- vector()
valid_vars_full_cor <- vector()

###Assumptions correction
# 1 We want to use only include variables that have at least 2 or more results. This can be used for all types of data
# 2 Exclude if over 10 categories because of performence issues

variable <- prevVars[1]
#And what to do with categorical(this is not possible) data, sex(this makes no sense) and age(this gives the incidence of being 1 year old)? 
for (variable in prevVars){
  
  #Retrieve unique values in the variable
  valUnique <- na.omit(unique(population[[variable]]))
  
  if(length(valUnique) >= 2  & length(valUnique) <= 10){
    
    if(sum(as.numeric(valUnique) %in% c(1,0)) == 2){ 
      prevalence <- length(which(population[[variable]] == 1))/nrow(population)*100
      
      if(prevalence >= 0.02){
        
        if(variable %in% myselectedvars){valid_vars_reduced_cor <- append(valid_vars_reduced_cor, variable)}
        valid_vars_full_cor <- append(valid_vars_full_cor, variable)
        
      } 
    }else{print(paste0("Not a boolean variable ",variable))}
    
  }else{warning(paste0("Wrong format for correction for ", variable))}
}



population$exposed <- ifelse(population$exposed_or_comparator == "exposed", 1, 0)

#population <- SetToInteger(population, c("agegroup", "matched_cohort"))$Data
###RE. Add first a check if any varaibles are in the vectors

if(length(valid_vars_reduced_cor) > 0){
  p.score_reduced_cor <- glm(f.build("exposed", c(valid_vars_reduced_cor)), # "matched_cohort","exposed","agegroup",
                             data = population[, c("exposed", valid_vars_reduced_cor), with = F], #, "matched_cohort""exposed","agegroup",
                             family = "binomial"
  )$fitted.values
  
  population$IPTW_reduced_cor <- with(population, exposed + (1 - exposed) * p.score_reduced_cor / (1 - p.score_reduced_cor))
  population$IPTW_reduced_cor_trim <- trim(population$IPTW_reduced_cor, at = .99)
  
  reduced_corrected_IPTW <- matrix(c(min(population$IPTW_reduced_cor_trim),
                                     max(population$IPTW_reduced_cor_trim),
                                     mean(population$IPTW_reduced_cor_trim),
                                     quantile(population$IPTW_reduced_cor_trim),
                                     sd(population$IPTW_reduced_cor_trim)))
  rownames(reduced_corrected_IPTW) <- c("min", "max", "mean", "p0", "p25", "p50", "p75", "p99", "sd")
  colnames(reduced_corrected_IPTW) <- "IPTW reduced correction descriptives"
  #View(reduced_corrected_IPTW)
  
}else{
  p.score_reduced_cor <- NULL
  print("No reduced correction variables")
}

if(length(valid_vars_full_cor) > 0){
  p.score_full_cor <- glm(f.build("exposed", c(valid_vars_full_cor)), #, "matched_cohort","agegroup",
                          data = population[, c("exposed", valid_vars_full_cor), with = F], #, "matched_cohort","exposed","agegroup",
                          family = "binomial"
  )$fitted.values
  
  ## Is this the same as 1/p.score_full_cor like in propensity score/ inverse probability weighting? Please annotade the used method.
  population$IPTW_full_cor <- with(population, exposed + (1 - exposed) * p.score_full_cor / (1 - p.score_full_cor))
  population$IPTW_full_cor_trim <- trim(population$IPTW_full_cor, at = .99)
  
  #min, p25, p50, p75, p99, max, mean, std
  full_corrected_IPTW <- matrix(c(min(population$IPTW_full_cor_trim),
                                  max(population$IPTW_full_cor_trim),
                                  mean(population$IPTW_full_cor_trim),
                                  quantile(population$IPTW_full_cor_trim),
                                  sd(population$IPTW_full_cor_trim)))
  rownames(full_corrected_IPTW) <- c("min", "max", "mean", "p0", "p25", "p50", "p75", "p99", "sd")
  colnames(full_corrected_IPTW) <- "IPTW full correction descriptives"
  #View(full_corrected_IPTW)
  
}else{
  p.score_full_cor  <- NULL
  print("No full correction variables")
}


