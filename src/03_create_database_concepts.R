#This needs to become a general variable to declare which coding systems are extracted with a wildcard.
start_with_colls <- c("ICD10CM","ICD10","ICD10DA","ICD9CM","MTHICD9", "ATC", "vx_atc")

#Since the tables are appending during the process the old database should be removed first.
if(file.exists(dbConceptsFile)) file.remove(dbConceptsFile)

#Create the database
mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)

#Put the concepts in the database in a standardized manner. This means that all the column names of the output tables in the database are equal
GetConceptsProcedure(
  t.interest = c("MEDICINES", "EVENTS", "SURVEY_ID"),
  additional = T,
  #expr = expression( person_id %in% PERSONS_OF_INTEREST1),
  concepts = ALL_CONCEPTS,
  dir.codes = dbDir,
  dir.data = pathCDM,
  standardized.cols = T
)

#Put concepts in the database with all the original columns in the table. This is because the ARS code for the studypopulation needs the original CDM names while the umcu
# scripts are relying on standardized column names for all the concepts independed of from which CDM tabel they originate.
GetConceptsProcedure(
  t.interest = c("VACCINES"),
  additional = F,
  concepts = "COV",
  dir.codes = dbDir,
  dir.data = pathCDM,
  standardized.cols = F
)

#Vaccines in database with standardized names.
tmpVar <- unique(readRDS(paste0(dbDir, "CODES_VACCINES.rds"))[["Outcome"]])
tmpVar <- tmpVar[!tmpVar %in% "COV"]
GetConceptsProcedure(
  t.interest = c("VACCINES"),
  additional = F,
  concepts = tmpVar,
  dir.codes = dbDir,
  dir.data = pathCDM,
  standardized.cols = T
)

rm(tmpVar)
###

#Make DEATH as a concept in the database so we can use that throughout the whole script. Maybe do this in CreateConcepts and not here.
dbWriteTable(mydb, "O_DEATHANY_AESI" , readRDS(paste0(d3Dir, "D3_PERSONS.rds"))[, .(person_id, date_death)], overwrite = T, append = F)

#Harmonize the column names. In the datebase always Date is used.
p <- dbSendStatement(mydb,"ALTER TABLE O_DEATHANY_AESI RENAME date_death TO Date")
dbClearResult(p)

dbListTables(mydb)

dbDisconnect(mydb)

rm(mydb)