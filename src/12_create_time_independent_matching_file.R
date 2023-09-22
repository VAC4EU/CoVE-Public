
#Prepare person information files for file for matching. Per matching population a boolean is set per subject.
#Inputs: study population files (D4_STUDY_POPULATION_CHILD/D4_STUDY_POPULATION.rds), clean_vaccines and clean_spells
#Outputs: a file with the persons and person level information (sex, population, etc) and a file with the observation periods.

#Get the matching variables that are stored in the persons table of the CDM. This needs to be included as time independed person information
matchingPersons <- unique(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TIMEDEP == F,][["STUDY_VARIABLES"]])

#Get the included subjects for the populations
personsChild <- readRDS(paste0(d4Dir, "D4_STUDY_POPULATION_CHILD.rds"))
personsPop <- readRDS(paste0(d4Dir, "D4_STUDY_POPULATION.rds"))
persons <- rbind(personsChild, personsPop)

PERSONS <- readRDS(paste0(d3Dir, "D3_PERSONS.rds"))[, .(person_id, sex_at_instance_creation, date_birth)]
persons <- merge(persons, PERSONS, all.x = T, by = "person_id")

#Get the spells information that whre cleaned
spells <- readRDS(paste0(d3Dir, "D3_CLEAN_SPELLS.rds"))[,.(person_id, entry_spell_category, exit_spell_category)]

#Get the vaccine information that needs to be added as time independent person level information
vaccines <- readRDS(paste0(d3Dir, "D3_VACCINES_CURATED.rds"))[, .(person_id, date_curated, dose_curated,
                                                                  manufacturer_curated)]

#Make a file with 1 row per subject that can be added to the person file
vaccines <- data.table::dcast(vaccines, person_id ~ dose_curated, value.var = c("date_curated", "manufacturer_curated"))

checkCols <- paste0("date_curated_", c(1:4))

for(i in 1:length(checkCols)){
  
  col2 <- paste0("manufacturer_curated_", i)
  
  if(!checkCols[i] %in% colnames(vaccines)){
    vaccines <- vaccines[, eval(checkCols[i]) := as.Date(integer(0), origin = "1970-01-01") ]
    vaccines <- vaccines[, eval(col2) := as.character() ]
  }
  rm(col2)
}

rm(checkCols)

#Harmonize column names
changeColsDate <- colnames(vaccines)[grepl("date_curated", colnames(vaccines))]
setnames(vaccines, changeColsDate, gsub("date_curated_", "date_vax", changeColsDate ))
changeColsMan <- colnames(vaccines)[grepl("manufacturer_curated", colnames(vaccines))]
setnames(vaccines, changeColsMan, gsub("manufacturer_curated_", "type_vax", changeColsMan ))
rm(changeColsDate, changeColsMan)

#Add the vaccines 
combine <- merge(x = persons, y = vaccines, by = "person_id", all.x = T)

# Add columns until the max number of vaccines in the study
max_number_doses <- 4

substrRight <- function(x, n){
  char_x <- nchar(x)
  substr(x, char_x - n + 1, char_x)
}

columns_for_vaccinated <- colnames(combine)[grepl("[1-9]$", colnames(combine))]
effective_max_number_doses <- max(as.integer(substrRight(columns_for_vaccinated, 1)))
missing_doses <- setdiff(seq_len(max_number_doses), seq_len(effective_max_number_doses))

if (length(missing_doses) != 0) {
  combinations <- expand.grid(new_date_manufacturer_cols, missing_doses)
  columns_to_create <- do.call(paste, c(combinations, sep = "_"))
  combine[, (columns_to_create[grepl("^type_vax", columns_to_create)]) := NA_character_]
  combine[, (columns_to_create[grepl("^date_vax", columns_to_create)]) := NA_Date_]
}

setnames(combine, c("entry_spell_category", "exit_spell_category"), c("ST2", "EN2"))

#Create population variables: this is essential for the matching. for every side in the matching for every population a boolean varaible is needed.
#Note that this is not finsched yet for all the matcing populations. Only child, HO_P and HE_P are finalized.
###
inputMatching <- copy(combine)

combine <- combine[, CHILD :=  fifelse(person_id %in% unlist(personsChild[, .(person_id)]), T, F)]
saveRDS(combine, paste0(d3Dir, "Combine.rds"))

#Booleans for population HO_P_to_HE_P
#
inputMatching <- inputMatching[, HO_P :=  fifelse(type_vax1 == type_vax2, T, F, na = F) ]
inputMatching <- inputMatching[, HE_P :=  fifelse(type_vax1 != type_vax2, T, F, na = F) ]
#

#Add intermediate booleans
#
inputMatching <- inputMatching[, B :=  fifelse(!is.na(type_vax3) , T, F) ]
#inputMatching <- inputMatching[, NB :=  fifelse(is.na(type_vax3) , T, F) ]
#inputMatching <- inputMatching[, NONE :=  fifelse(is.na(type_vax1) , T, F) ]

inputMatching <- inputMatching[, CHILD :=  fifelse(person_id %in% unlist(personsChild[, .(person_id)]), T, F) ]
#inputMatching <- inputMatching[, STUDYPOP :=  fifelse(person_id %in% personsChild , T, F) ]
inputMatching <- inputMatching[, HO_B :=  fifelse(type_vax2 == type_vax3, T, F, na = F) ]
inputMatching <- inputMatching[, HE_B :=  fifelse(type_vax2 != type_vax3, T, F, na = F) ]
#



#Booleans for population HO_P_HO_B_to_NB
#
inputMatching <- inputMatching[, HO_P_HO_B :=  fifelse(HO_P == T & HO_B == T, T, F) ]

#

#Booleans for population HO_P_HE_B_to_NB
#
inputMatching <- inputMatching[, HO_P_HE_B :=  fifelse(HO_P == T & HE_B == T, T, F) ]
#


#

#Booleans for population ANY_P_with_prior_covid_B_to_NB
#
inputMatching <- inputMatching[, HE_P_B :=  fifelse(HE_P == T & B == T, T, F) ]
#

#
mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)

inputMatching <-  merge(x = inputMatching,  
                        y= as.data.table(dbReadTable(mydb, "FREE_COVID"))[NB == 1 & COVID_NB == 1,][, Date := as.Date(Date, "1970-01-01")][, .(person_id, Date)],
                        all.x = T,
                        by = "person_id")

dbDisconnect(mydb)

inputMatching <- inputMatching[, ANY_P_with_prior_covid :=  fifelse(Date < date_vax3 , T, F, na = F) ]
inputMatching <- inputMatching[, ANY_P_with_prior_covid_B :=  fifelse(Date < date_vax3 & B == T, T, F, na = F) ][, Date := NULL]

#

#Booleans for children
###
inputMatching <- inputMatching[, CHILD_P :=  fifelse(CHILD == T & !is.na(date_vax2) , T, F) ]
inputMatching <- inputMatching[, CHILD_NON :=  fifelse(CHILD == T & is.na(date_vax1), T, F) ]

###



#Delete subjects that are not in child cohort and do not have had any vaccination
inputMatching <- inputMatching[(CHILD == F & !is.na(type_vax2)) | CHILD == T ,]

###

#Save the person file and observation file separate,. This is used to create the matching database together with the time depended matching vartaibles  
# that are stored in spell files in the next step.
saveRDS(unique(inputMatching[, .(person_id, ST2, EN2)]), paste0(matchingDir,"OBS.rds"))
saveRDS(unique(inputMatching[, colnames(inputMatching)[!colnames(inputMatching) %in% c("ST2","EN2")], with = F]), paste0(matchingDir,"PER.rds"))





