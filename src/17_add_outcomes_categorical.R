


##Aim

#Set up database connection
mydb <- dbConnect(RSQLite::SQLite(), db = dbConceptsFile)

#set a test database to develop
testDir <- paste0(dbDir, "OUTCOMES.db")
if(file.exists(testDir)) file.remove(testDir)
mydb2 <- dbConnect(RSQLite::SQLite(), db = testDir)

#Load lookback values needed for time scope within the queries
lookback <- readRDS(file = paste0(dbDir, "LOOKBACK.rds" ))


###
#Create the needed vectors. For different types (CATEGORICAL, GREEDY_CATEGORICAL, SUM, SCORE/DISTICT_SUM) of covariates a separate vector is created as input for the function GetDatesIR

#Get available coavariates and outcomes
cov <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "CAT" & COVARIATE == T & ORIGIN != "PERSONS" & (GREEDY != T | is.na(GREEDY)) ,][["STUDY_VARIABLES"]])
cov <- cov[cov %in% dbListTables(mydb)]

bmi <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "BMI"  ,][["STUDY_VARIABLES"]])
bmi <- bmi[bmi %in% dbListTables(mydb)]

sum <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "SUM"  ,][["STUDY_VARIABLES"]])
sum <- sum[sum %in% dbListTables(mydb)]

score <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "SCORE"  ,][["STUDY_VARIABLES"]])
scoreFile <- ImportPattern(pat = "algorithms", dir = metaDir)[toupper(NEW_STUDY_VARIABLES) %in% toupper(score), ][,.(WEIGHT, STUDY_VARIABLES, NEW_STUDY_VARIABLES)][, NEW_STUDY_VARIABLES := toupper(NEW_STUDY_VARIABLES)]
scoreFile <- merge(x = scoreFile, y = lookback, by.x = "NEW_STUDY_VARIABLES", by.y = "STUDY_VARIABLES", all.x = T)
scoreFile <- scoreFile[STUDY_VARIABLES %in% dbListTables(mydb),] 
scoreOutcomes <- scoreFile[["STUDY_VARIABLES"]]
#scoreLookbacks <- scoreFile[["DAYS"]]

greedy <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[TYPE == "CAT" & COVARIATE == T & ORIGIN != "PERSONS" & GREEDY == T  ,][["STUDY_VARIABLES"]])
greedy <- greedy[greedy %in% dbListTables(mydb)]


###

##########
 
#Run the query on the database. For outcomes look forwards form T0, for coavariates look backwards.
#For coavariates I need to know the lookback periods per covariate. Now it is set to ever.



if(length(cov) > 0){
  
  if("COVARIATES_CAT" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "COVARIATES_CAT") 
  lapply(cov, FUN =  function(i){ 
    
                      lookbackTmp <- lookback[STUDY_VARIABLES == i ,][["DAYS"]]                                        
    
                                            tmp <- GetDatesIR(
                                                              Concept = i, 
                                                              Start_date = "T0", 
                                                              FILE = "POPULATIONS", 
                                                              c.name = paste0(i,"_at_matching_date"),
                                                              lookback.days = lookbackTmp,
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
  
  print("No categorical covariates availble in data")
  
}



if(length(greedy) > 0){
  
  if("GREEDY_CAT" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "GREEDY_CAT") 
  
  
  
  lapply(greedy, FUN =  function(i){ 
    
    lookbackTmp <- lookback[STUDY_VARIABLES == i ,][["DAYS"]]
    
    tmp <- GetDatesIR(
    Concept = i, 
    Start_date = "T0", 
    FILE = "POPULATIONS", 
    c.name = paste0(i,"_at_matching_date"),
    lookback = lookbackTmp,
    prior = T,
    post = T,
    between = F,
    db = dbConceptsFile,
    endpoint = "t1.T0 + (999 * 365.25)" ,
    coll = "Value"
    
    
  )
  
  concept <- tmp$Concept
  
  tmp <- merge(x = as.data.table(tmp$file1), y = as.data.table(tmp$file2), all = T, allow.cartesian = F, by = c("person_id", "pair_ID"))
  tmp <- tmp[, Value := fifelse(is.na(Value.x), Value.y, Value.x)  ]
  tmp <- tmp[, Date := fifelse(is.na(Value.x), Date.y, Date.x)  ][, .(person_id, pair_ID, Value, Date)][, OUTCOME := concept]
  
  dbWriteTable(mydb2, "GREEDY_CAT", tmp, append = T, overwrite = F)
  
  
  
  }
  
  )                                                              
  
  
}else{
  
  print("No greedy covariates availble in data")
  
}




if(length(bmi) > 0){
  
  if("BMI" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "BMI") 
  
  lapply(bmi, FUN =  function(i){ tmp <- GetDatesIR(
    Concept = i, 
    Start_date = "T0", 
    FILE = "POPULATIONS", 
    c.name = i,
    lookback = lookback[STUDY_VARIABLES == "BMI" ,][["DAYS"]],
    prior = T,
    post = F,
    between = F,
    db = dbConceptsFile,
    endpoint = NULL,
    coll = "Value, Voc"
    
    
  )
  
  concept <- tmp$Concept
  tmp <- tmp$file1[, OUTCOME := concept]
  
  dbWriteTable(mydb2, "BMI", tmp, append = T, overwrite = F)
  }
  
  )                                                              
  
  
}else{
  
  print("No bmi covariates availble in data")
  
}


if("BMI" %in% dbListTables(mydb2)){

        BMI <- as.data.table(dbReadTable(mydb2, "BMI"))[, Date := as.Date(Date, origin = "1970-01-01")][grepl("unit.", Voc),][, Value := as.numeric(Value)]
        
        
        BMI <- CalculateBMI(
          
          file = BMI[,.(person_id, pair_ID, Value, Date, OUTCOME)],
          filevoc = BMI[,.(person_id, pair_ID, Voc, OUTCOME)],
          Result = "Value",
          Voc = "Voc",
          Date = "Date",
          id = c("person_id", "pair_ID"),
          StudyVar = "OUTCOME",
          weight.var = "L_WEIGHT_COV",
          height.var = "L_HEIGHT_COV",
          bmi.var = "L_BMI_COV",
          fun = "max",
          unitprefix = "unit."
          
        )[, OUTCOME := "BMI_at_matching_date"]
        
        setnames(BMI, "Result", "Value" )

}


if(length(sum) > 0){
  
  if("SUM" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "SUM") 
  lapply(sum, FUN =  function(i){ 
    
    lookbackTmp <- lookback[STUDY_VARIABLES == i ,][["DAYS"]]
    
    tmp <- GetDatesIR(
    Concept = i, 
    Start_date = "T0", 
    FILE = "POPULATIONS", 
    c.name = paste0(i,"_at_matching_date"),
    lookback = lookbackTmp,
    prior = T,
    prior.sum = T,
    post = F,
    between = F,
    db = dbConceptsFile,
    endpoint = NULL,
    coll = "Value"
    
    
  )
  
  concept <- tmp$Concept
  tmp <- tmp$file1[, OUTCOME := concept]
  
  dbWriteTable(mydb2, "SUM", tmp, append = T, overwrite = F)
  }
  
  )                                                              
  
  
}else{
  
  print("No sum covariates availble in data")
  
}





if(nrow(scoreFile) > 0){
  
  if("SCORES" %in% dbListTables(mydb2)) dbRemoveTable(mydb2, "SCORES") 
  
  lapply(1:nrow(scoreFile) , FUN =  function(i){ 
    
    tmp <- GetDatesIR(
                      Concept = scoreFile[i ,][["STUDY_VARIABLES"]], 
                      Start_date = "T0", 
                      FILE = "POPULATIONS", 
                      c.name = scoreFile[i ,][["STUDY_VARIABLES"]],
                      lookback = scoreFile[i ,][["DAYS"]],
                      prior = T,
                      post = F,
                      between = F,
                      db = dbConceptsFile,
                      endpoint = NULL,
                      coll = "Value"
    
    
  )
  
  concept <- tmp$Concept
  tmp <- tmp$file1[, OUTCOME := concept][, BY := scoreFile[i ,][["NEW_STUDY_VARIABLES"]]]
  
  dbWriteTable(mydb2, "SCORES", tmp, append = T, overwrite = F)
  }
  
  )                                                              
  
  
}else{
  
  print("No score covariates availble in data")
  
}


if("SCORES" %in% dbListTables(mydb2)){

      SCORES <- as.data.table(dbReadTable(mydb2, "SCORES"))
      
      #Merge the weights to the sub concepts by an inner join. Because of the innner join also a filtereing is done
      SCORES <- merge(x = SCORES[, .(person_id, pair_ID, OUTCOME, BY, Value, Date)], 
                      y = scoreFile[, .(WEIGHT, NEW_STUDY_VARIABLES, STUDY_VARIABLES)], 
                      by.x = c("OUTCOME", "BY"), 
                      by.y = c("STUDY_VARIABLES", "NEW_STUDY_VARIABLES"), all = F
                      
                      )
      
      #Score by summing the weights that are joined to the results
      SCORES <- SCORES[, .(Value = sum(as.numeric(WEIGHT))  ), by = c("person_id", "pair_ID", "BY")]
        
      #Give the column name. This name is needed for the casting later on.
      SCORES <- SCORES[, OUTCOME := paste0(BY,"_at_matching_date")][, BY := NULL]
}  

file <- rbindlist(list(
  if("COVARIATES_CAT" %in% dbListTables(mydb2)) as.data.table(dbReadTable(mydb2, "COVARIATES_CAT"))[, Date := as.Date(Date, origin = "1970-01-01")] ,
  if("GREEDY_CAT" %in% dbListTables(mydb2)) as.data.table(dbReadTable(mydb2, "GREEDY_CAT"))[, Date := as.Date(Date, origin = "1970-01-01")],
  if(exists("BMI")) BMI,
  if("SUM" %in% dbListTables(mydb2)) as.data.table(dbReadTable(mydb2, "SUM"))[, Date := NULL],
  if(exists("SCORES")) SCORES
  ),
  fill = T, use.names = T)
file <- data.table::dcast(file , person_id + pair_ID ~ OUTCOME, value.var = "Value")

dbDisconnect(mydb)
dbDisconnect(mydb2)

#Set NA's to 0 if numerical
file <- MakeBooleanCovariates(data = file, cols = paste0(c(sum, score) , "_at_matching_date"), type = "NA0", date.ref = "T0")


saveRDS(file,paste0(d3Dir,"TEMP_OUTCOMES.rds"))



rm(file)
if(exists("SCORES")) rm(SCORES)
if(exists("BMI")) rm(BMI )
rm(cov, bmi, sum, score, scoreOutcomes,scoreFile,  mydb, mydb2, greedy, lookback)
gc()




