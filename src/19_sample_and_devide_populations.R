
#Load all subjects that are in the population
file <- readRDS(paste0(d3Dir,"D3_matched_population_main_study_complete.rds"))

# Get all pair_ids
matched_pairs <- readRDS(paste0(d3Dir, "MATCHED_PAIRS.rds"))

# Select only pairs with a control
matched_pairs <- matched_pairs[!is.na(Control), ]
keys <- c("Control", "T0_CONTROL", "matched_cohort")
setkeyv(matched_pairs, keys)

# Select only one row for each strata defined by key
matched_pairs <- matched_pairs[, .SD[sample(.N, min(.N, 1))], by = keys]

# Filter information from D3_matched_population_main_study_variables
D3_matched_population <- merge(matched_pairs[, .(pair_ID)], file, all.x = T, by.x = "pair_ID", by.y = "pair_id")
setnames(D3_matched_population, "pair_ID", "pair_id")

D3_unmatched_population <- file[!(pair_id %in% matched_pairs[, pair_ID])]
D3_unmatched_population <- D3_unmatched_population[, matched := F]
file <- rbindlist(list(D3_matched_population, D3_unmatched_population), use.names = T)

# #Select only the controls
# controls <- file[exposed_or_comparator == "comparator", ][,.(pair_id, person_id)]
# 
# #From those controls check the subjects that are more then once a control
# duplicated <- controls$person_id[duplicated(controls$person_id)]
# 
# check1 <- length(unique(duplicated))
# 
# #Retrieve all pairs with the duplicated subjects
# duplicated <- controls[person_id %in% duplicated,]
# 
# #Take sample
# selected <- duplicated[,.SD[sample(.N, min(1,.N))],by = "person_id"]
# 
# #Store subjects that are only once a control
# noSample <- file[!pair_id %in% duplicated$pair_id,]
# 
# #Store of the subjects that are more then once a control only the randomly selected pairs
# sample <- file[pair_id %in% selected$pair_id,]
# 
# if(nrow(sample)/2 != check1) stop("sample not correct")
# 
# rm(file)
# gc()
# 
# #Make the total file again
# file <- rbind(noSample,sample)

#Store as rds files for further steps

saveRDS(file, paste0(d3Dir,"D3_matched_population_main_study.rds"))

for(i in unique(file[["matched_cohort"]])){
  
  tmp <- file[matched_cohort == i,][,matched_cohort := NULL] 
  saveRDS(tmp, paste0(d3Dir,"D3_",i,"_analytic_dataset.rds"))
  
  rm(tmp)
  gc()
}



# rm(controls, selected, duplicated, noSample, sample, check1)
gc()


#person_id, sex_at_instance_creation, age_at_time0, ageband_at_time0, type_vax1, type_vax2, homologous_P, L_GEOREGION_COV_at_time0, immunodeficiency_at_time0,cancer_at_time0,Im_TRANSPLANTRECIPIENT_COV_at_time0,G_SEVERERENALDISEASE_CH_at_time0,O_DOWN_COV_at_time0
