 


##Aim

#Set up database connection
mydb <- dbConnect(RSQLite::SQLite(), db = dbConceptsFile)

#Get available coavariates and outcomes
outcomes <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[OUTCOME == TRUE ,][["STUDY_VARIABLES"]])
outcomes <- outcomes[outcomes %in% dbListTables(mydb)]
cov <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[COVARIATE == TRUE & TYPE == "TF",][["STUDY_VARIABLES"]])
cov <- cov[cov %in% dbListTables(mydb)]

#Load lookback values needed for time scope within the queries
lookback <- readRDS(file = paste0(dbDir, "LOOKBACK.rds" ))

##########

#Run the query on the database. For outcomes look forwards form T0, for coavariates look backwards.
#For coavariates I need to know the lookback periods per covariate. Now it is set to ever.

if(length(outcomes) > 0){
  
  COVARIATES <- lapply(cov, FUN =  function(i){ 
    
    lookbackTmp <- lookback[STUDY_VARIABLES == i ,][["DAYS"]]  
                  
                  GetDatesIR(
                            Concept = i, 
                            Start_date = "T0", 
                            FILE = "POPULATIONS", 
                            c.name = paste0(i,"_at_matching_date"),
                            lookback = lookbackTmp,
                            prior = T,
                            post = F,
                            between = F,
                            db = dbConceptsFile,
                            endpoint = NULL
                            
                        
                            )
    
        }
    
    )
  
  


}else{
  
  print("No covariates availble in data")
  
}


if(length(outcomes) > 0){
                        OUTCOMES <- lapply(outcomes, FUN =  function(i) GetDatesIR(
                                                            Concept = i, 
                                                            Start_date = "T0", 
                                                            FILE = "POPULATIONS", 
                                                            c.name = paste0("first_",i),
                                                            lookback = NULL,
                                                            prior = F,
                                                            post = T,
                                                            between = F,
                                                            db = dbConceptsFile,
                                                            endpoint = "study_exit_date"
                                                            
                                                            
                                                          ))
  
  
}else{
  
  print("No outcomes availble in data")
  
}





dbDisconnect(mydb)


#Append outputs from covariates and aesi's. This is done to add the vertically (append). The it can be transposed in 1 step forming hte output.
appendLists <- c(OUTCOMES, COVARIATES)
rm(OUTCOMES, COVARIATES)
gc()

#Hard coded query for  
###
betweenVar <- GetDatesIR(
  Concept = "FREE_COVID", 
  Start_date = "Date", 
  FILE = "POPULATIONS", 
  c.name = "covid_between_vax2_and_matching_date",
  lookback = NULL,
  prior = F,
  post = F,
  between = T,
  c.st = "date_vax2",
  c.en = "T0",
  db = dbConceptsFile,
  coll = "Date"
  
  
  
)$file3[date_vax2 != T0, ][,.(person_id, Date, pair_ID)][, OUTCOME := "covid_between_vax2_and_matching_date"][, Date := as.Date(Date, origin = "1970-01-01")]

setorder(betweenVar, "person_id", "pair_ID", "OUTCOME")
betweenVar <- betweenVar[, test := seq_len(.N), by = c("person_id", "pair_ID", "OUTCOME")][test == 1,][, test := NULL]

###


for(i in 1:length(appendLists)){

  var <- appendLists[[i]][["Concept"]]
  print(var)
  
  if(exists("file1", where = appendLists[[i]])){fName <- "file1"}
  if(exists("file2", where = appendLists[[i]])){fName <- "file2"}

  tmp <- as.data.table(appendLists[[i]][[fName]] )[, Date := as.Date(Date, origin = "1970-01-01")][, OUTCOME := var]
  
  if(i == 1) FILE <- tmp
  if(i > 1) FILE <- rbindlist(list(FILE, tmp)) 
  rm(var, tmp)
  
}

FILE <- rbindlist(list(FILE, betweenVar))

rm(appendLists, betweenVar)
gc()

#setorder(FILE, "person_id", "pair_ID", "OUTCOME")
#FILE[, test := seq_len(.N), by = c("person_id", "pair_ID", "OUTCOME")]

#Put it o a wide file
file <- data.table::dcast(FILE , person_id + pair_ID ~ OUTCOME, value.var = "Date")
rm(FILE)
gc()

#Add to the population file
file <- merge(x = readRDS(paste0(d3Dir,"POPULATIONS2.rds")), y =  file, by = c("person_id", "pair_ID"), all.x = T,)


#Create additional need variables for analytical file. 
###


original_cols <- c("FREE_COVID_at_matching_date", "first_FREE_COVID", "first_COVID_SEV2", "first_COVID_SEV3", "O_DEATHANY_AESI_at_matching_date")
new_cols <- c("date_last_covid_days", "first_covid_after_md_days", "first_SEVEREcovid_after_md_days", "DEATHcovid_days", "any_DEATH_days")
original_cols_product <- paste0(original_cols, "_days")

datesToT0 <- c("date_vax1" , "date_vax2", "date_vax3", original_cols)
datesToT0 <- datesToT0[datesToT0 %in% colnames(file)]
file <- GetDayDif(data = file, date.ref = "T0", dates = datesToT0, postfix = "_days")

#Make covariates binary
###
file <- MakeBooleanCovariates(data = file, cols = paste0(cov, "_at_matching_date"), type = "NATF")
###


setnames(file, original_cols_product[original_cols_product %in% colnames(file)], new_cols[original_cols_product %in% colnames(file)])

rm(datesToT0, original_cols, original_cols_product)
###

#aatest <- file[, datesToT0, with = F]





###


#Add country based on the DAP
###

daps <- toupper(c("TEST", "ARS", "PHARMO", "CPRD", "PEDIANET", "NHR", "HSD", "EPICHRON", "SIDIAP", "BIFAP")) 
countries <- toupper(c("netherlands", "italy", "uk", "netherlands", "italy", "norway", "italy", "spain", "spain",  "spain"))

countriesTable <- as.data.table(cbind(daps, countries))
country <- countriesTable[daps == toupper(DAP),  ][["countries"]]
if(length(country) == 0) country <- "UNK"

file <- file[, COUNTRY := country ]

rm(daps, countries, countriesTable)


###

if(file.exists(paste0(d3Dir,"TEMP_OUTCOMES.rds"))) file <- merge(x = file, y = readRDS(paste0(d3Dir,"TEMP_OUTCOMES.rds")), by = c("person_id", "pair_ID"), all.x = T)


#Set all tolower to allign column names for codebook
setnames(file, colnames(file), tolower(colnames(file)))

#Hard coded correction of some varaible names for Doriekes code
### 
old <- c("first_FREE_COVID", "ageband", "t0", "FREE_COVID_at_matching_date", "first_covid_sev2", "first_covid_sev3", "inf_at_matching_date")
new <- c("first_covid_after_md_date","ageband_at_matching_date", "date_start", "any_covid_at_matching_date", "first_SEVEREcovid_after_md_date", "DEATHcovid_date", "flu_vax_prev_5years_at_matching_date")
setCols <- tolower(old) %in% colnames(file)
setnames(file, tolower(old)[setCols], tolower(new)[setCols])
###


load(paste0(projectDir, "/data/i_simulated_analytic_datasets/D4_HO_P_to_HE_P_analytic_dataset.RData"))
needed_colls <- colnames(D4_HO_P_to_HE_P_analytic_dataset)
done <- needed_colls[tolower(needed_colls) %in% tolower(colnames(file))]
notdone <- needed_colls[!tolower(needed_colls) %in% tolower(colnames(file))]
#print(paste0("Columns missing: ", paste0(notdone, collapse = ", ")))

setcolorder(file, tolower(done))


saveRDS(file, paste0(d3Dir,"D3_matched_population_main_study_complete.rds"))

rm(file, outcomes, cov, done, notdone, D4_HO_P_to_HE_P_analytic_dataset, needed_colls)
gc()

