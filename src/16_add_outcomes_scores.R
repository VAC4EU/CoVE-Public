 

  
##Aim

#Set up database connection
mydb <- dbConnect(RSQLite::SQLite(), db = dbConceptsFile)

#set a test database to develop
testDir <- paste0(dbDir, "OUTCOMES.db")
mydb2 <- dbConnect(RSQLite::SQLite(), db = testDir)

#Get available coavariates and outcomes
cov <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "CAT" & COVARIATE == T & ORIGIN != "PERSONS" & (GREEDY != T | is.na(GREEDY)) ,][["STUDY_VARIABLES"]])
cov <- cov[cov %in% dbListTables(mydb)]

greedy <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "CAT" & COVARIATE == T & ORIGIN != "PERSONS" & GREEDY == T  ,][["STUDY_VARIABLES"]])
greedy <- greedy[greedy %in% dbListTables(mydb)]



##########

#Run the query on the database. For outcomes look forwards form T0, for coavariates look backwards.
#For coavariates I need to know the lookback periods per covariate. Now it is set to ever.



if(length(cov) > 0){
  
  if("COVARIATES_CAT" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "COVARIATES_CAT") 
  lapply(cov, FUN =  function(i){ tmp <- GetDatesIR(
                                                              Concept = i, 
                                                              Start_date = "T0", 
                                                              FILE = "POPULATIONS", 
                                                              c.name = paste0(i,"_at_matching_date"),
                                                              lookback = 999,
                                                              prior = T,
                                                              post = F,
                                                              between = F,
                                                              db = dbConceptsFile,
                                                              endpoint = NULL,
                                                              coll = "Value"
                                                              
                                                          
                                                              )
                                                  
                                                  concept <- tmp$Concept
                                                  tmp <- tmp$file1[, OUTCOME := concept]
                       
                       dbWriteTable(mydb2, "COVARIATES_CAT", tmp, append = T, overwrite = F)
  }
    
  )                                                              


}else{
  
  print("No covariates availble in data")
  
}


if(length(greedy) > 0){
  
  if("GREEDY_CAT" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "GREEDY_CAT") 
  lapply(greedy, FUN =  function(i){ tmp <- GetDatesIR(
    Concept = i, 
    Start_date = "T0", 
    FILE = "POPULATIONS", 
    c.name = paste0(i,"_at_matching_date"),
    lookback = 999,
    prior = T,
    post = T,
    between = F,
    db = dbConceptsFile,
    endpoint = "t1.T0 + (999 * 365.25)" ,
    coll = "Value"
    
    
  )
  
  concept <- tmp$Concept
  tmp <- tmp$file1[, OUTCOME := concept]
  
  dbWriteTable(mydb2, "GREEDY_CAT", tmp, append = T, overwrite = F)
  }
  
  )                                                              
  
  
}else{
  
  print("No greedy covariates availble in data")
  
}


#aatest <- dbReadTable(mydb2, "GREEDY_CAT")



dbDisconnect(mydb)



dbDisconnect(mydb2)
