
#Specify population
#populationName <- "HO_P_TO_HE_P"



#Get population
population <- readRDS(paste0(d3Dir,"D3_",populationName,"_analytic_dataset.rds"))[matched == T,] #[,matched_cohort := populationName]
#population <- population[date_final_censoring_days <= 210 ,]
#population <- population[, tp_covid19test_cov_at_matching_date := sample(c(0,1), nrow(population), replace = T)]

#children is not matched on first brand like the other groups. therefore first brand is filled with the brand of there exposed pair. 
###

if(populationName == "children"){
  
  population <- merge(x = population, 
                      y = population[exposed_or_comparator == "exposed", .(pair_id, type_vax1)], 
                      by = "pair_id", all.x = T, 
                      allow.cartesian = F
  )[,type_vax1 := type_vax1.y ][,type_vax1.x := NULL ][,type_vax1.y := NULL ]
  
  
  
}



###

###
if(populationName %in% c("HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB")){
  
  population <- merge(x = population, 
                      y = population[exposed_or_comparator == "exposed", .(pair_id, type_vax3)], 
                      by = "pair_id", all.x = T, 
                      allow.cartesian = F
  )[,type_vax3 := type_vax3.y ][,type_vax3.x := NULL ][,type_vax3.y := NULL ]
  
  
  
}
###


#Working on additional stratifications
###
#Prepare columns for stratification
###
stratifyClinic <- c("immunodeficiency_at_matching_date",
                    "cancer_at_matching_date",
                    "im_transplantrecipient_cov_at_matching_date",
                    "g_severerenaldisease_ch_at_matching_date",
                    "o_down_cov_at_matching_date",
                    "any_covid_at_matching_date"
)


for(i in stratifyClinic){

  if(i %in% colnames(population)){population <-population[, eval(i) := as.character(get(i))]
  }else{
    population <-population[, eval(i) := as.character("0")]
  }


}

#Add no covid and tested
###

colls_npos <- c("any_covid_at_matching_date", "tp_covid19test_cov_at_matching_date") 
colls_npos <- all(colls_npos %in% colnames(population))

if(colls_npos){
  population[, neg_tested2 := sum(any_covid_at_matching_date == "0" & tp_covid19test_cov_at_matching_date > 0),by = "pair_id" ][neg_tested2 == 2 , neg_tested := as.character("1")][, neg_tested2 := NULL]
}
###


#Get dates for gentic variants

dates <- ImportPattern("covid_genetic_variants", metaDir)

if(DAP %in% dates[["DAP_NAME"]]){dates <- dates[DAP_NAME == DAP,]}else{
  dates <- dates[DAP_NAME == "ALL",]
}



###



#Because renaming was done for other programmers, that is not in line with the meta information of the program, it is renamed back.
###
new <- c("first_FREE_COVID", "FREE_COVID_at_matching_date", "first_covid_sev2", "first_covid_sev3", "inf_at_matching_date")
old <- c("first_covid_after_md_date",  "any_covid_at_matching_date", "first_SEVEREcovid_after_md_date", "DEATHcovid_date", "flu_vax_prev_5years_at_matching_date")
setCols <- tolower(old) %in% colnames(population)
setnames(population, tolower(old)[setCols], tolower(new)[setCols])
###

#Check if needed agebands are in population and assign agegroups needed for stratification
###
if(populationName == "children"){
      agebandsNeeded <-  c("5-11", "12-17")
}else{
      agebandsAdults <-  c("30-49", "18-29", "50-59", "60-69", "70-79", "80+")
      agebandsAdolocents <-  c("12-17")
      agebandsNeeded <- c(agebandsAdolocents, agebandsAdults)
      rm(agebandsAdolocents, agebandsAdults)
      gc()
}




missingAgebands <-  agebandsNeeded[!agebandsNeeded %in% population$ageband_at_matching_date]
if(length(missingAgebands) > 0) warning(paste0("Agebands ",paste0(missingAgebands, collapse = ",")," are missing"))

population <- population[ageband_at_matching_date %in% agebandsNeeded, ]

#Add a column to distinct between adolescents and adults for the stratification
if(populationName == "children"){
  population <- population[, agegroup := "Children"]
}else{
  population <- population[, agegroup := fifelse(ageband_at_matching_date == "12-17", "Adolescents", "Adults")]
}

rm(agebandsNeeded, missingAgebands)

###

###ReCreate a variable with only the variables that have a 0/1
#prevVars <- paste0(tolower(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == TRUE & TYPE == "TF" ,][["STUDY_VARIABLES"]]), "_at_matching_date")
prevVars <- paste0(tolower(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == T ,][["STUDY_VARIABLES"]]), "_at_matching_date")
prevVars <- prevVars[prevVars %in% colnames(population)]
prevVars <- unique(c("agegroup" ,prevVars,  "sex_at_instance_creation")) #, "l_georegion_cov_at_matching_date"

#GLM cannot take missings. So na's are removed first
###
#if("bmi_at_matching_date" %in% colnames(population)){
#  population$bmi_at_matching_date <- as.numeric(population$bmi_at_matching_date)
#  population$bmi_at_matching_date <- cut(population$bmi_at_matching_date, breaks =  c(0,18.5,25,27,30,40,100), labels = as.character(c(1,2,3,4,5,6)))
#  
#}

#varsCat <- paste0(tolower(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == T & TYPE %in% c("CAT", "BMI") ,][["STUDY_VARIABLES"]]), "_at_matching_date")
varsSum <- paste0(tolower(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == T & TYPE %in% c("SUM", "SCORE") ,][["STUDY_VARIABLES"]]), "_at_matching_date")

#varsCat <- c(varsCat[varsCat %in% colnames(population)], "sex_at_instance_creation")
varsSum <- varsSum[varsSum %in% colnames(population)]

if(length(varsSum) > 0) for(i in varsSum){population[is.na(get(i)), eval(i) := 0]}
#if(length(varsCat) > 0) for(i in varsCat){population[is.na(get(i)), eval(i) := "UNK"]}

#Delete variables as decided by the PI
prevVars <- prevVars[!prevVars %in% c("l_smokestatus_cov_at_matching_date", "bmi_at_matching_date")]
#Exclude variables that are used in stratification
#prevVars <- prevVars[!prevVars %in% stratifyClinic]

###


valid_vars_full_cor <- vector()



###Assumptions correction
# 1 We want to use only include variables that have at least 2 or more results. This can be used for all types of data
# 2 Exclude if over 10 categories because of performance issues
# 3 Exclude if for binary the incidence is very low or for most of the subjects are NA

#variable <- prevVars[1]
#And what to do with categorical(this is not possible) data, sex(this makes no sense) and age(this gives the incidence of being 1 year old)? 
for (variable in prevVars){
  
  #Retrieve unique values in the variable
  valUnique <- na.omit(unique(population[[variable]]))
  
  if(length(valUnique) >= 2 & (is.numeric(population[[variable]]) | length(valUnique) <= 10) &  !any(is.na(population[[variable]]))  ){#& length(valUnique) <= 10
  
        if(sum(as.character(valUnique) %in% c("1","0")) == 2){ 
            prevalence <- length(which(population[[variable]] == 1))/nrow(population)*100
            if(prevalence >= 0.02) valid_vars_full_cor <- append(valid_vars_full_cor, variable)
            
        
          }else{
            prevalence <- sum(!is.na(population[[variable]]))/nrow(population)*100
            if(prevalence >= 0.02) valid_vars_full_cor <- append(valid_vars_full_cor, variable)
            
        }
    rm(prevalence)
  }else{warning(paste0("Wrong format for correction for ", variable))}
  rm(valUnique)
}

rm(prevVars)
population$exposed <- ifelse(population$exposed_or_comparator == "exposed", 1, 0)



#population <- SetToInteger(population, c("agegroup", "matched_cohort"))$Data
###RE. Add first a check if any varaibles are in the vectors


if(length(valid_vars_full_cor) > 0){
  p.score_full_cor <- glm(f.build("exposed", valid_vars_full_cor), #, "matched_cohort"
                          data = population[, c("exposed", valid_vars_full_cor), with = F], #, "matched_cohort"
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
  
  saveRDS(list(full_corrected_IPTW, valid_vars_full_cor), paste0(outputDir,populationName,"_IPW_characteristics.rds"))
  
  rm(p.score_full_cor, full_corrected_IPTW, valid_vars_full_cor)
}else{
  
  print("No full correction variables.")
}


#---- Creating output tables ----#
# Adults
# ----- all covid ---- #



outcomes <- paste0("first_" ,tolower(ImportPattern(pat = "study_variables", dir = metaDir)[OUTCOME == T ,][["STUDY_VARIABLES"]]))
outcomes <- outcomes[outcomes %in% colnames(population)]
populations <- populationName #unique(population$matched_cohort)
ageGroup <- unique(population$agegroup)


combinations <- as.data.table(expand.grid(outcomes, ageGroup, populations))
colnames(combinations) <- c("outc", "ageGroup", "pop") 



schemeTmp <- lapply(1:nrow(combinations), function(x){
 
              outc <- as.character(combinations[x,][["outc"]])
              ageGroup <- as.character(combinations[x,][["ageGroup"]])
              pop <- as.character(combinations[x,][["pop"]])
              
              #colsAll <- c("agegroup", "matched_cohort")
              #valuesAll <- c(ageGroup, pop)
              colsAll <- c("agegroup")
              valuesAll <- c(ageGroup)
                  
             return(  
               list(list(name = "Overall", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    list(name = "1st dose PFIZER", outcome = outc, col = c("type_vax1", colsAll, "free_covid_at_matching_date"), value = c("PFIZER", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    list(name = "1st dose MODERNA", outcome = outc, col = c("type_vax1", colsAll, "free_covid_at_matching_date"), value = c("MODERNA", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    list(name = "1st dose ASTRAZENECA", outcome = outc, col = c("type_vax1", colsAll, "free_covid_at_matching_date"), value = c("ASTRAZENECA", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    
                    if(pop %in% c("HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB")) list(name = "3th dose PFIZER", outcome = outc, col = c("type_vax3", colsAll, "free_covid_at_matching_date"), value = c("PFIZER", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop %in% c("HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB")) list(name = "3th dose MODERNA", outcome = outc, col = c("type_vax3", colsAll, "free_covid_at_matching_date"), value = c("MODERNA", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop %in%c("HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB")) list(name = "3th dose ASTRAZENECA", outcome = outc, col = c("type_vax3", colsAll, "free_covid_at_matching_date"), value = c("ASTRAZENECA", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    
                    list(name = "0-6 days", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 0, en = 6, stdt = NULL, endt = NULL),
                    list(name = "7-13 days", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 7, en = 13, stdt = NULL, endt = NULL),
                    list(name = "14-29 days", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 14, en = 29, stdt = NULL, endt = NULL),
                    list(name = "2nd month (30-59 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 30, en = 59, stdt = NULL, endt = NULL),
                    list(name = "3rd month (60-89 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 60, en = 89, stdt = NULL, endt = NULL),
                    list(name = "4th month (90-120 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 90, en = 120, stdt = NULL, endt = NULL),
                    list(name = "5th month (121-150 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 121, en = 150, stdt = NULL, endt = NULL),
                    list(name = "6th month (151-180 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 151, en = 180, stdt = NULL, endt = NULL),
                    list(name = "7th month (181-210 days)", outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = 181, en = 210, stdt = NULL, endt = NULL),
                    
                      list(name = "Immunodeficiency or Immunosuppressant", outcome = outc, col = c("immunodeficiency_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("1", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      list(name = "Cancer or malignant tumor", outcome = outc, col = c("cancer_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("1", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      list(name = "Severed renal disease", outcome = outc, col = c("g_severerenaldisease_ch_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("1", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      list(name = "Transplant recipient", outcome = outc, col = c("im_transplantrecipient_cov_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("1", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      list(name = "Down Syndrome", outcome = outc, col = c("o_down_cov_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("1", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      list(name = "Covid", outcome = outc, col = c("free_covid_at_matching_date", colsAll), value = c("1", valuesAll), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                      
                    if(colls_npos) list(name = "No covid and tested", outcome = outc, col = c("neg_tested", colsAll), value = c("1", valuesAll), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    
                    
                    
                    if(pop == "children") list(name = "Age 5-11", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("5-11", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop == "children") list(name = "Age 12-17", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("12-17", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    #if(pop != "children" & ageGroup == "Adolescents") list(name = "12-17", outcome = outc, col = c("ageband_at_matching_date", colsAll), value = c("12-17", valuesAll), st = NULL, en = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age 18-29", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("18-29", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age 30-49", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("30-49", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age 50-59", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("50-59", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age 60-69", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("60-69", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age  70-79", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("70-79", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    if(pop != "children" & ageGroup == "Adults") list(name = "Age 80+", outcome = outc, col = c("ageband_at_matching_date", colsAll, "free_covid_at_matching_date"), value = c("80+", valuesAll, "0"), st = NULL, en = NULL, stdt = NULL, endt = NULL),
                    
                    list(name = paste0(dates[1, ST_PREDELTA],"-", dates[1, EN_PREDELTA]), outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = NULL, en = NULL, stdt = dates[1, as.Date(ST_PREDELTA, format = "%Y%m%d")], endt = dates[1, as.Date(EN_PREDELTA, format = "%Y%m%d")]),
                    list(name = paste0(dates[1, ST_DELTA],"-", dates[1, EN_DELTA]), outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = NULL, en = NULL, stdt = dates[1, as.Date(ST_DELTA, format = "%Y%m%d")], endt = dates[1, as.Date(EN_DELTA, format = "%Y%m%d")]),
                    list(name = paste0(dates[1, ST_OMNI],"-", dates[1, EN_OMNI]), outcome = outc, col = c(colsAll, "free_covid_at_matching_date"), value =  c(valuesAll, "0"), st = NULL, en = NULL, stdt = dates[1, as.Date(ST_OMNI, format = "%Y%m%d")], endt = dates[1, as.Date(EN_OMNI, format = "%Y%m%d")])
                    
                    #dates[1, ST_PREDELTA]
                    #dates[1, as.Date(ST_PREDELTA, format = "%Y%m%d")]
                    #dates[1, as.Date(EN_PREDELTA, format = "%Y%m%d")]
                    
                    
                     ###
                    
        
                    ###
               )
               
               
)
              rm(colsAll, valuesAll)              
}
)

scheme <- list()
for(i in 1:length(schemeTmp)){
  scheme <- append(scheme, schemeTmp[[i]] )
  
}

scheme <- scheme[which(!sapply(scheme, is.null))]



output <- matrix(NA, nrow = length(scheme), ncol = 22)
graph.output <- vector("list", length(scheme))

colnames(output) <- c("population", "outcome", "name", "agegroup","Control persondays", "Control cases of covid", "Control IR/100,000", "Exposed persondays", 
                      "Exposed cases of covid", "Exposed IR/100,000", "Rate diff/100,000", "U 95% CI", "L 95% CI",
                      "crude HR", "L CI", "U CI", "HR adjusted", "L CI", "U CI", "VE adjusted", "L CI", "U CI")

if(nrow(population) > 0){

for(i in 1:length(scheme)){

print(paste0("Calculate statistical output ", i, " out of ", length(scheme)))

tmp <- GetStatisticalOutcomes(
      dataset = population,
      timeStart = scheme[[i]]$st,
      timeEnd = scheme[[i]]$en, 
      c.exposed = "exposed",
      c.strata = scheme[[i]]$col,
      v.strata = scheme[[i]]$value,
      d.t0 = "date_start",
      d.outcome = scheme[[i]]$outcome, 
      d.cencoring = "date_final_censoring",
      id = "person_id",
      id.pair = "pair_id",
      IPTW_weights = "IPTW_full_cor_trim",
      dateStart = scheme[[i]]$stdt,
      dateEnd = scheme[[i]]$endt 
)

output[i, 5:22] <- tmp$table
output[i, 1] <- populationName #scheme[[i]]$value[which(scheme[[i]]$col == "matched_cohort")]
output[i, 2] <- scheme[[i]]$outcome
output[i, 3] <- scheme[[i]]$name
output[i, 4] <- scheme[[i]]$value[which(scheme[[i]]$col == "agegroup")]

pathReport<-paste0(outputDir,paste0("df_",populationName,"_new.test.csv"))
write.csv(output ,pathReport, row.names = TRUE)

graph.output[[i]] <- list( 
  graph.object = tmp$graph,
  graph.popname = populationName, #scheme[[i]]$value[which(scheme[[i]]$col == "matched_cohort")]
  graph.outcome =  scheme[[i]]$outcome,
  graph.name = scheme[[i]]$name,
  graph.agegroup = scheme[[i]]$value[which(scheme[[i]]$col == "agegroup")]
)
pathGraph <- paste0(outputDir, populationName,"_graph.rds")
saveRDS(graph.output, pathGraph)

# dbWriteTable(mydb, "dataset" ,
#              tmp$dataset[, population := populationName][, outcome := scheme[[i]]$outcome][, name := scheme[[i]]$name][, agegroup :=  scheme[[i]]$value[which(scheme[[i]]$col == "agegroup")]], 
#              append = T)


rm(tmp)



}
}
rm(outcomes, populations, ageGroup, combinations, schemeTmp, scheme, output, graph.output, pathReport, pathGraph, colls_npos)
rm(population, variable)
gc()





