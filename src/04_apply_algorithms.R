###
#Simple algorithms with only OR. These are specified in the meta file algorithms
###
ALG <- readRDS(paste0(dbDir, "ALG.rds"))[OR == T,]

Algoritms <- unique(ALG$NEW_STUDY_VARIABLES) 

mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)
#i <- Algoritms[1]

for(i in Algoritms){
  
  to_append <- unique(ALG[NEW_STUDY_VARIABLES == i,]$STUDY_VARIABLES)
  
  TEMP <- AppendConcepts(DB = mydb, CONCEPTS = to_append, NAME = i)
  
  if(!is.null(TEMP)){
    dbWriteTable(mydb, i, TEMP)
  }
  
  rm(TEMP, to_append)
  gc()
  
}
dbDisconnect(mydb)
###

#Add indexes and remove duplicate rows in concepts
AddIndexes(db.path = dbConceptsFile, needed.fields = c("Outcome", "Date", "person_id" ), remove.duplicated = T, index.all = T )