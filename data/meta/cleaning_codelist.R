library(data.table)

event_codelist <- data.table::fread(here::here("data", "meta", "old_20220930.csv"))
DP_codelist <- data.table::fread(here::here("data", "meta", "old_ALL_drugs_full_codelist.csv"))
study_variable <- data.table::fread(here::here("data", "meta", "study_variables.csv"))

event_codelist[, Varname := paste(system, event_abbreviation, type, sep = "_")]

# setdiff(unique(event_codelist$Varname), study_variable$STUDY_VARIABLES)
# setdiff(study_variable$STUDY_VARIABLES, unique(event_codelist$Varname))
# 
# setdiff(unique(DP_codelist$drug_abbreviation), study_variable$STUDY_VARIABLES)
# setdiff(study_variable$STUDY_VARIABLES, unique(DP_codelist$drug_abbreviation))

event_codelist <- event_codelist[Varname %in% study_variable[, STUDY_VARIABLES]][, Varname := NULL]
DP_codelist <- DP_codelist[drug_abbreviation %in% study_variable[, STUDY_VARIABLES], ]

data.table::fwrite(event_codelist, here::here("data", "meta", "20220930.csv"))
data.table::fwrite(DP_codelist, here::here("data", "meta", "ALL_drugs_full_codelist.csv"))
