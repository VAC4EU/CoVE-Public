
#Aim: Create outcomes covid severity
#Input: sev1 from database 
#Output: new concepts sev1-sev3 in conceptsdb


#Load function for covid sev1 and sev2. These queries are similar.
CovidSeverity <- function(or.cols, and.col, db.path , outcome.name, t.post, t.prior){ 
  
  #Open conncection with database
  db <- dbConnect(RSQLite::SQLite(), db.path)
  
  #Make a temp intermediate table for the query that needs to be joined
  dbWriteTable(db, "TEMP" , AppendConcepts(db, or.cols, outcome.name), overwrite = T, append = F)
  
  #Check if varaible is already in database. If so delete
  if(outcome.name %in% dbListTables(db)){
    p <- dbSendStatement(db ,paste0("DROP TABLE ",outcome.name ))
    dbClearResult(p)
  } 
  
  #Do query which is a inner join taking into account a time window from the and.col
  p <- dbSendStatement(db,paste0(
    "
                     CREATE TABLE ",outcome.name," AS
                     
                     SELECT t1.Date, t1.person_id, t1.COVID_NB, t1.NB FROM (SELECT * FROM ",and.col," WHERE NB = 1) t1
                     
                     INNER JOIN (SELECT DISTINCT person_id, Date FROM TEMP) t2 
                     
                     ON (
                          t1.person_id = t2.person_id AND
                          (t2.Date - t1.Date) < ",t.post," AND
                          (t2.Date - t1.Date) >= ",t.prior," 
                          )
                     
                     "
    
  )
  )
  
  dbClearResult(p)
  
  #Delete the temp table
  p <- dbSendStatement(db ,"DROP TABLE TEMP")
  dbClearResult(p)
  
  dbDisconnect(db)
  
  
}


#Creating sev2 using the function
CovidSeverity(
  or.cols = c("R_ARDS_AESI", "H_HOSPPOP_POP", "H_EMERG_COV", "H_HOSPADMCOVID_COV")  ,
    and.col = "FREE_COVID",
  db.path = dbConceptsFile,
  outcome.name = "COVID_SEV2",
  t.post = 30,
  t.prior = 0
  
)

#Creating sev3 using the function
CovidSeverity(
  or.cols = c("O_DEATHANY_AESI"),
  and.col = "FREE_COVID",
  db.path = dbConceptsFile,
  outcome.name = "COVID_SEV3",
  t.post = (7 * 8),
  t.prior = 0
  
)


#Create D3 output file
mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)

#Retreive all wanted infromation from the database for the output table. First merging so that combined variables can be created later. 
#If the MORE varaibles will not be relevant anymore convert this to a union instead of a join
covidOutcomes <- as.data.table(dbGetQuery(mydb, 
           "
           SELECT  
           t1.person_id,
           t1.Date,
           t1.NB as COVID_SEV1,
           t2.NB as COVID_SEV2,
           t3.NB as COVID_SEV3
           
           FROM (SELECT * FROM FREE_COVID WHERE NB = 1) t1
           
                LEFT JOIN (SELECT person_id, COVID_NB, NB   FROM   COVID_SEV2) t2 ON (t1.person_id = t2.person_id AND t1.COVID_NB = t2.COVID_NB) 
                
                  LEFT JOIN (SELECT person_id, COVID_NB, NB  FROM   COVID_SEV3) t3 ON (t1.person_id = t3.person_id AND t1.COVID_NB = t3.COVID_NB) 
                  
                  
           
           "
           
           
           
           )
)

#Create MORE variables
covidOutcomes <- covidOutcomes[, COVIDANY := COVID_SEV1]
covidOutcomes <- covidOutcomes[COVID_SEV2 | COVID_SEV3, `:=` (COVSEV2OR3 = 1, COVID_SEV1 = NA ) ]

#Define output variables for the output dataset
outputVars <- c("COVID_SEV1", "COVID_SEV2", "COVID_SEV3", "COVSEV2OR3", "COVIDANY")

#Melting is sensitive for the format of the columns that need to melted. Therfore set to integer all
lapply(outputVars , function(x) covidOutcomes <- covidOutcomes[, eval(x) :=  as.integer(get(x))])

#Make a long table
covidOutcomes <- data.table::melt(covidOutcomes, id.vars = c("person_id", "Date"), measure.vars = outputVars, na.rm = T, variable.name = "STUDY_VARIABLE")
covidOutcomes <- covidOutcomes[value == 1,][, value := NULL]

#Add category labels
covidOutcomes <- covidOutcomes[STUDY_VARIABLE == "COVID_SEV1", Type := "covid infection, no hospitalised nor worse"]
covidOutcomes <- covidOutcomes[STUDY_VARIABLE == "COVID_SEV2", Type := "covid infection hospitalised/ICU but not worse"]
covidOutcomes <- covidOutcomes[STUDY_VARIABLE == "COVID_SEV3", Type := "covid infection with death"]
covidOutcomes <- covidOutcomes[STUDY_VARIABLE == "COVSEV2OR3", Type := "covid infection severity 2 or 3"]
covidOutcomes <- covidOutcomes[STUDY_VARIABLE == "COVIDANY", Type := "covid infection irrespective of severity"]

covidOutcomes <- covidOutcomes[, Date := as.Date(Date, origin = "1970-01-01")] 

#I need the AESI's in the database.
for(i in outputVars[outputVars %in% c("COVID_SEV1",  "COVSEV2OR3")]){
  dbWriteTable(mydb, i, unique(covidOutcomes[STUDY_VARIABLE == i,][, .(person_id, Date)]),   overwrite = F, append =F)
}


#This set is not needed but I store it as it is specified
saveRDS(covidOutcomes, paste0(d3Dir, "D3_COVID_OUTCOMES.rds"))

dbDisconnect(mydb)

rm(covidOutcomes, mydb, outputVars)

AddIndexes(db.path = dbConceptsFile, needed.fields = NULL, remove.duplicated = T, index.all = T, tables =  c("COVID_SEV1", "COVID_SEV2", "COVID_SEV3",  "COVSEV2OR3") )





