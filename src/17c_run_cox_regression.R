#---- Creating output tables ----#
# Adults
# ----- all covid ---- #

outcomes <- paste0("first_" ,tolower(ImportPattern(pat = "study_variables", dir = metaDir)[OUTCOME == T ,][["STUDY_VARIABLES"]]))
outcomes <- outcomes[outcomes %in% colnames(population)]
populations <- unique(population$matched_cohort)
ageGroup <- unique(population$agegroup)


combinations <- as.data.table(expand.grid(outcomes, ageGroup, populations))
colnames(combinations) <- c("outc", "ageGroup", "pop") 


schemeTmp <- lapply(1:nrow(combinations), function(x){
  
  outc <- as.character(combinations[x,][["outc"]])
  ageGroup <- as.character(combinations[x,][["ageGroup"]])
  pop <- as.character(combinations[x,][["pop"]])
  
  
  return(  
    list(list(name = "Overall adults", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = NULL, en = NULL),
         list(name = "1st dose PFIZER", outcome = outc, col = c("type_vax1", "agegroup", "matched_cohort"), value = c("PFIZER", ageGroup, pop), st = NULL, en = NULL),
         list(name = "1st dose MODERNA", outcome = outc, col = c("type_vax1", "agegroup", "matched_cohort"), value = c("MODERNA", ageGroup, pop), st = NULL, en = NULL),
         list(name = "1st dose ASTRAZENECA", outcome = outc, col = c("type_vax1", "agegroup", "matched_cohort"), value = c("ASTRAZENECA", ageGroup, pop), st = NULL, en = NULL),
         list(name = "0-6 days", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 0, en = 6),
         list(name = "7-13 days", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 7, en = 13),
         list(name = "14-29 days", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 14, en = 29),
         list(name = "2nd month (30-59 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 30, en = 59),
         list(name = "3rd month (60-89 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 60, en = 89),
         list(name = "4th month (90-120 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 90, en = 120),
         list(name = "5th month (121-150 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 121, en = 150),
         list(name = "6th month (151-180 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 151, en = 180),
         list(name = "7th month (181-210 days)", outcome = outc, col = c("agegroup", "matched_cohort"), value =  c(ageGroup, pop), st = 181, en = 210)
    )  
  )
}
)

scheme <- list()
for(i in 1:length(schemeTmp)){
  scheme <- append(scheme, schemeTmp[[i]] )
  
}





output <- matrix(NA, nrow = length(scheme), ncol = 22)

colnames(output) <- c("population", "outcome", "name", "agegroup","HE persondays", "HE cases of covid", "HE IR/100,000", "HO persondays", 
                      "HO cases of covid", "HO IR/100,000", "Rate diff/100,000", "U 95% CI", "L 95% CI",
                      "crude HR", "L CI", "U CI", "HR adjusted", "L CI", "U CI", "VE adjusted", "L CI", "U CI")

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
    IPTW_weights = "IPTW_full_cor_trim" 
  )
  
  output[i, 5:22] <- tmp
  output[i, 1] <- scheme[[i]]$value[which(scheme[[i]]$col == "matched_cohort")]
  output[i, 2] <- scheme[[i]]$outcome
  output[i, 3] <- scheme[[i]]$name
  output[i, 4] <- scheme[[i]]$value[which(scheme[[i]]$col == "agegroup")]
  
  
  
  pathReport<-paste0(outputDir,"df_HO_P_to_HE_P_new.test.csv")
  write.csv(output ,pathReport, row.names = TRUE)
  
  
  rm(tmp)
  
}



