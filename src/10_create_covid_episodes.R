#Aim: Clean covid
#Input: COVIDINFECTION
#Outputs: COVIDINFECTIONCLEAN

#Connect to the database
mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)

#Create COVID_INFECTION
covidCases <- as.data.table(dbReadTable(mydb, "COVIDINFECTION"))[, Date := as.Date(Date, origin = "1970-01-01")]


#Enrich the test data with a not double case within 60 days. !!!REMOVE
covidCases[13,5] <- covidCases[13,5] +10

#Then we want to apply a wash out period of 60 days. I use the function CleanOutcomes Albert extended this, we may need to have alook in that function.
covidCasesClean <- CleanOutcomes(
                                    Dataset = copy(covidCases)[,.(person_id, Date)][, Outcome := "COVIDINFECTION"], 
                                    Person_id = "person_id",
                                    Rec_period = 60,
                                    Outcomes = c("COVIDINFECTION"),
                                    Name_event = "Outcome",
                                    Date_event = "Date"
            
                                    
                                                  )

#Not clear yet if all dates are needed for further code so we merge back to have it all

#Add instance of the covid infection
setorder(covidCasesClean, person_id, Date)
covidCasesClean <- covidCasesClean[, COVID_NB := seq_len(.N), by = "person_id"]

#Merge back the file that is uncleaned and forward fill the empty's
covidCasesClean <- merge(x = copy(covidCases)[,.(person_id, Date)], y = covidCasesClean, by = c("person_id", "Date"), all.x = T, allow.cartesian = T)
covidCasesClean <- unique(covidCasesClean[,COVID_NB :=  nafill(COVID_NB, "locf") ][, Iteration := NULL][, Outcome := NULL])

#Add again an instance number so we can easy filter out the start of the episode by taking the first per covid infection.
setorder(covidCasesClean, person_id, Date, COVID_NB)
covidCasesClean <- covidCasesClean[, NB := seq_len(.N), by = c("person_id", "COVID_NB")]

#Write to the database as a new CONCEPT and add indexes
dbWriteTable(mydb, "FREE_COVID" ,covidCasesClean[NB == 1, ], overwrite = T, append = F)

rm(covidCases, covidCasesClean)
gc()
###


#dbListTables(mydb)

dbDisconnect(mydb)


AddIndexes(db.path = dbConceptsFile, needed.fields = NULL, remove.duplicated = T, index.all = T, tables =  "FREE_COVID" )
