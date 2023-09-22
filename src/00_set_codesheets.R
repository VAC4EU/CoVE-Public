#Aim: In this script all available codesheets in metadata are processed, so they are ready for use thru the pipeline.
#The outputs are rds files with information stored by CDM tables MEDICINES, EVENTS, VACCINES. 
#Also an additional table is made storing information not merely for 1 specific CDM table

#Prepare EVENTS codes
CODESHEET_EVENTS <- ImportPattern(
  dir = metaDir, 
  pat = "20220930",
  append = F
)[,.(coding_system,code,event_abbreviation, type, tags, system, code_original)][, Outcome := paste0(system,"_", event_abbreviation, "_", type)][, ':=' (event_abbreviation = NULL, type = NULL, system = NULL)]

# Recode the coding system vocabularies
CODESHEET_EVENTS[, coding_system := data.table::fcase(
  coding_system %in% c("ICD10", "ICD10CM", "ICD10DA"), "ICD10CM",
  coding_system %in% c("Free_text"), "Free_text",
  coding_system %in% c("ICD9CM", "MTHICD9", "ICD9"), "ICD9CM",
  coding_system %in% c("ICPC"), "ICPC",
  coding_system %in% c("ICPC2P", "ICPC2EENG", "ICPC2"), "ICPC2P",
  coding_system %in% c("RCD2", "RCD"), "READ",
  coding_system %in% c("MEDCODEID", "SCTSPA", "SNOMEDCT_US", "SPA_EXT", "SNM", "SCTSPA_SNS", "MedCodeId", "MDR",
                       "ICPC2ICD10ENG"), "SNOMED", 
  coding_system %in% c("ATC"), "ATC"
)]

# Duplicated ICD9CM and ICD10CM to add ICD9 and ICD10
temp_df_1 <- CODESHEET_EVENTS[coding_system == "ICD9CM", ][, coding_system := "ICD9"]
temp_df_2 <- CODESHEET_EVENTS[coding_system == "ICD10CM", ][, coding_system := "ICD10"]

# Merge the resulting datasets
CODESHEET_EVENTS <- rbindlist(list(CODESHEET_EVENTS, temp_df_1, temp_df_2))

rm(temp_df_1, temp_df_2)

CODESHEET_EVENTS_TEMP <-  unique(CODESHEET_EVENTS[toupper(tags) == toupper("narrow"),][, any_narrow := T][,.(Outcome, any_narrow)])
CODESHEET_EVENTS <- merge(x = CODESHEET_EVENTS, y = CODESHEET_EVENTS_TEMP, by = "Outcome", all.x = T)
CODESHEET_EVENTS <- CODESHEET_EVENTS[toupper(tags) == toupper("narrow") |  Outcome == "M_ARTASEPTIC_AESI" | (is.na(any_narrow) & is.na(tags)),]

#Removing Smoke status from the codesheet since it wa decided to only use the varaible via additional concepts
###
CODESHEET_EVENTS <- CODESHEET_EVENTS[Outcome != "L_SMOKESTATUS_COV",]
###

coding_systems <- unique(CODESHEET_EVENTS[!is.na(coding_system),][["coding_system"]])

rm(CODESHEET_EVENTS_TEMP)

#Load DAP specific concepts
DAP_SPECIFIC_CONCEPTS <- ImportPattern(
  dir = metaDir, 
  pat = "specific_additional_concepts.csv",
  append = F
  
)[DAP_NAME == DAP]

x <- colnames(DAP_SPECIFIC_CONCEPTS)
x <- x[substr(x, 1, 3) %in% c("col", "val") & nchar(gsub("[0-9]","", substr(x, 4,nchar(x)))) == 0]

DAP_SPECIFIC_CONCEPTS <- DAP_SPECIFIC_CONCEPTS[, c("DAP_NAME", "table", "StudyVar", "keep", "date_column", x) , with = F ][table != "VACCINES",] 
rm(x)

setnames(DAP_SPECIFIC_CONCEPTS, "StudyVar", "Outcome")


###

# Prepare Medincines codes
CODESHEET_MEDICINES <-  ImportPattern(
  dir = metaDir, 
  pat = "ALL_drugs_full_codelist.csv",
  append = F
  
)

CODESHEET_MEDICINES <- CODESHEET_MEDICINES[, Outcome := drug_abbreviation][, CodeSystem := "ATC"]

setnames(CODESHEET_MEDICINES, c("atc_code"), c("Code"))

CODESHEET_MEDICINES <- CODESHEET_MEDICINES[, .(Code, Outcome, CodeSystem)]

#Prepare VACCINES codes
#This icoming via ATC codes AND/OR DAP specific annotations in vx_type
DAP_SPECIFIC_VACCINES <- ImportPattern(
  dir = metaDir, 
  pat = "specific_additional_concepts.csv",
  append = F
  
)[DAP_NAME == DAP][table == "VACCINES",]


lapply(colnames(DAP_SPECIFIC_VACCINES)[substr(colnames(DAP_SPECIFIC_VACCINES),1,3) == "col"], 
       function(x) DAP_SPECIFIC_VACCINES[get(x) == "vx_type", Code := get(paste0("val",substr(x,4,nchar(x))))]
)
DAP_SPECIFIC_VACCINES <- DAP_SPECIFIC_VACCINES[, `:=` (CodeSystem =  "vx_type", Outcome = StudyVar) ][, c("CodeSystem", "Code", "Outcome")  , with = F]

CODESHEET_VACCINES <-  ImportPattern(
  dir = metaDir, 
  pat = "vaccines_codelist.csv",
  append = F
  
)[, CodeSystem := "vx_atc"]
setnames(CODESHEET_VACCINES, c("StudyVar", "atc_codes"), c("Outcome", "Code"))

CODESHEET_VACCINES <- rbindlist(list(DAP_SPECIFIC_VACCINES, CODESHEET_VACCINES), fill = T, use.names = T)
rm(DAP_SPECIFIC_VACCINES)

#Prepare Algoritms: this consists of a file that specifies grouping and a file that specifies scorings
###
 ALG <-  ImportPattern(
    dir = metaDir, 
    pat = "algorithms.csv",
    append = F
    
  )
 
#Set all to upper so no mistakes can be made due to capital sensitivity
CODESHEET_EVENTS <- CODESHEET_EVENTS[,Outcome := toupper(Outcome)]
CODESHEET_VACCINES <- CODESHEET_VACCINES[,Outcome := toupper(Outcome)]
DAP_SPECIFIC_CONCEPTS <- DAP_SPECIFIC_CONCEPTS[,Outcome := toupper(Outcome)]
CODESHEET_MEDICINES <- CODESHEET_MEDICINES[,Outcome := toupper(Outcome)]
ALG <- ALG[,STUDY_VARIABLES := toupper(STUDY_VARIABLES)]
ALG <- ALG[, NEW_STUDY_VARIABLES := toupper(NEW_STUDY_VARIABLES)]
ALL_CONCEPTS <- unique(toupper(ImportPattern(pat = "study_variables", dir = metaDir)[["STUDY_VARIABLES"]]))

if(any(duplicated(toupper(ImportPattern(pat = "study_variables", dir = metaDir)[["STUDY_VARIABLES"]])))){print("Duplicated study variables")}

#Check if meta files form the program are valid
###
test <- unique(c(ALG[["NEW_STUDY_VARIABLES"]],ALG[["NEW_STUDY_VARIABLES"]]))
check1 <- test[!test %in% ALL_CONCEPTS]
if(length(check1) > 0) print(paste0(paste0(check1, collapse = ", "), " need to be added in study_variables.csv because they are in the algorithm file"))

check2a <- unique(ALG[OR == T,][["STUDY_VARIABLES"]])
check2b <- unique(c(CODESHEET_EVENTS[["Outcome"]],CODESHEET_VACCINES[["Outcome"]],CODESHEET_MEDICINES[["Outcome"]],DAP_SPECIFIC_CONCEPTS[["Outcome"]]))

check2 <- check2a[!check2a %in% check2b]
if(length(check2) > 0) print(paste0(paste0(check2, collapse = ", "), " are not specified but needed for the algorithms"))

check3a <- unique(ALG[["NEW_STUDY_VARIABLES"]])
check3b <- ALL_CONCEPTS[!ALL_CONCEPTS %in% check3a]

check3 <- check3b[!check3b %in% check2b]
if(length(check3) > 0) print(paste0(paste0(check3, collapse = ", "), " are not specified"))

###


saveRDS(CODESHEET_EVENTS[Outcome %in% ALL_CONCEPTS,], file = paste0(dbDir, "CODES_EVENTS.rds" ))
saveRDS(CODESHEET_VACCINES[Outcome %in% ALL_CONCEPTS,], file = paste0(dbDir, "CODES_VACCINES.rds" ))
saveRDS(DAP_SPECIFIC_CONCEPTS[Outcome %in% ALL_CONCEPTS,], file = paste0(dbDir, "CODES_ADDITIONAL.rds" ))
saveRDS(CODESHEET_MEDICINES[Outcome %in% ALL_CONCEPTS,], file = paste0(dbDir, "CODES_MEDICINES.rds" ))
saveRDS(ALG, file = paste0(dbDir, "ALG.rds" ))
rm(CODESHEET_VACCINES,DAP_SPECIFIC_CONCEPTS, CODESHEET_MEDICINES, CODESHEET_EVENTS, ALG)
gc()


#Create file with lookback values 
###
lookbackDefinitions <- ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == T,][, .(STUDY_VARIABLES, LOOKBACK, LOOKBACK_UNIT)][, LOOKBACK := as.numeric(LOOKBACK)][, STUDY_VARIABLES := toupper(STUDY_VARIABLES)]
lookbackDefinitions <- lookbackDefinitions[is.na(LOOKBACK) , `:=` (LOOKBACK = 999, LOOKBACK_UNIT = "year") ]

#Handle lookback times by setting all to days
lookbackDefinitions <- lookbackDefinitions[LOOKBACK_UNIT == "year", DAYS := LOOKBACK * 365.25][LOOKBACK_UNIT == "day", DAYS := LOOKBACK][LOOKBACK_UNIT == "month", DAYS := LOOKBACK * 30][LOOKBACK_UNIT == "week", DAYS := LOOKBACK * 7]

saveRDS(lookbackDefinitions, file = paste0(dbDir, "LOOKBACK.rds" ))

rm(lookbackDefinitions)
###