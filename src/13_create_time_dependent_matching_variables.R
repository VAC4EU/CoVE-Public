#Aim: Create spell files for the time dependent matching variables that can be used to create the matching database. 
#Inputs: Concept files 
#Outputs: spell files for every concept that is needed.

#remove all rds files in output folder
file.remove(list.files(matchingDir, pattern = "SPELLS", full.names = T))

#Start with retrieving the necessary parameters from the meta file
###

#Get file with the information 
matchingDefinitions <- ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T,][, .(STUDY_VARIABLES, TIMEDEP, TYPE, LOOKBACK, LOOKBACK_UNIT)]

#Divide the variables in it's 2 types. The binary stated as TF and the categorical stated as CAT. Therefore we do changes in the meta folder, not in the code.
matchingTF <- toupper(matchingDefinitions[TYPE == "TF" & TIMEDEP == T,][["STUDY_VARIABLES"]]) 
matchingCat <- toupper(matchingDefinitions[TYPE == "CAT" & TIMEDEP == T,][["STUDY_VARIABLES"]])

#Handle loockbak times by setting all to days
lookbackMatch <- matchingDefinitions[TYPE == "TF",][, .(STUDY_VARIABLES, LOOKBACK, LOOKBACK_UNIT)][, LOOKBACK := as.numeric(LOOKBACK)]
lookbackMatch <- lookbackMatch[LOOKBACK_UNIT == "year", DAYS := LOOKBACK * 365.25][LOOKBACK_UNIT == "day", DAYS := LOOKBACK][LOOKBACK_UNIT == "month", DAYS := LOOKBACK * 30][LOOKBACK_UNIT == "week", DAYS := LOOKBACK * 7]
lookbackMatch <- lookbackMatch[, STUDY_VARIABLES := toupper(STUDY_VARIABLES)]



###

#Get file with all needed persons (not sure what to do here so this code is temporary so I can continue)
###
PER <- unique(rbindlist(
  list(readRDS(paste0(d4Dir,"D4_STUDY_POPULATION_CHILD.rds"))[,.(person_id, entry_spell_category, exit_spell_category)],
  readRDS(paste0(d4Dir,"D4_STUDY_POPULATION.rds"))[,.(person_id, entry_spell_category, exit_spell_category)])
)[,entry_spell_category := NULL][,exit_spell_category := NULL])

persons_of_interest <- unique(PER[["person_id"]])
#Save this so we could reuse this 
saveRDS(persons_of_interest, paste0(d3Dir,"persons_of_interest.rds"))

###



#Create TF variables (this are binary variables). For all the time within the study period it need to be defined if a condition is TRUE or FALSE
###

#Open conncection to database
mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)

#i <- 4
for(i in 1:length(matchingTF)){
  
  varTemp <- matchingTF[i]
  print(varTemp)
  
  #Check if the needed concept is in the database
  if(varTemp %in% dbListTables(mydb)){
  
  #Determine lookback needed to set the spell  
  prior <- lookbackMatch[STUDY_VARIABLES == varTemp,][["DAYS"]]
  
  #Get the concepts from the database
  TEMP <- as.data.table(unique(dbGetQuery(mydb, paste0("SELECT person_id, Date FROM ",varTemp)) ))
  if(nrow(TEMP) > 0){
  
  #Create the spell by adding the loockbacktime to the date of the occurence. This generate the TRUE spell  
  TEMP <- TEMP[, ":=" (ST = Date + 1, EN = Date  + prior)][,.(person_id, ST, EN)]
  TEMP <-TEMP[, `:=` (ST = as.Date(ST, origin = "1970-01-01"), EN = as.Date(EN, origin = "1970-01-01"))][EN > studyEndDate2, EN := studyEndDate2 ]    
  TEMP <- TEMP[!(is.na(ST) | is.na(EN))]
    #Remove overlapping spells
    TEMP <- CreateSpells(TEMP, "person_id", "ST", "EN")
    setnames(TEMP, c("entry_spell_category","exit_spell_category"), c("ST", "EN"))
      
      #Add the FALSE spells. It is essential that every person that is in the matching as a control or exposed needs to have the whole studytime covered. 
      TEMP <- DevideTF(
        FILE = TEMP,
        FILEP = PER[person_id %in% persons_of_interest,],
        c.start = "ST",
        c.end = "EN" ,
        id = "person_id",
        start_study_date = studyStartDate2,
        end_study_date = studyEndDate2
      )
      
      setnames(TEMP, "Status", varTemp)
      
      if(any(is.na(TEMP))) warning("NA's in spells file")
      
      saveRDS(TEMP, paste0(matchingDir, varTemp,"_SPELLS.rds"))
      #dbWriteTable(mydb2, paste0(varTemp,"_SPELLS"), TEMP, overwrite = T)
      
    }else{print("0 rows found so so this varaible is excluded from matching")}
    
    rm(TEMP, varTemp, prior)
    gc()
    
  }else{print("Not founded in database so this varaible is excluded from matching")}
  } 






###

#Create CAT variables
###

#For the agebands the start dates per person per agebands needed to be retrieved.
birthDays <- readRDS(paste0(d3Dir,"D3_PERSONS.rds"))[,.(person_id, date_birth)][person_id %in% persons_of_interest,]
startBands <- c(5,12,18,30,50,60,70,80)

#To make script rerunnable first the to create table is removed
if(dbExistsTable(mydb, "AGEBAND")) dbRemoveTable(mydb, "AGEBAND")

#TEMP = data.table()

for(i in 1:length(startBands)){
        
  #i <- 1
        #Create label text
        if(i < length(startBands)) ageband <- paste0(startBands[i],"-", startBands[i + 1] - 1)else{
          ageband <- paste0(startBands[i],"+")
        }
        
        #calculate the date of which a shift of ageband occurs. Only start date per band is needed because the function MakeSpells does the rest
        band <- copy(birthDays)[, Date := add_with_rollback(date_birth, period(startBands[i], units = "year"), roll_to_first = T, preserve_hms = T)]
        
        #Add the label
        band <- band[, Value := ageband][, date_birth := NULL]
        
        #Write to database. The next loop is seeking in the database so it needs to be there
        dbWriteTable(mydb, "AGEBAND" , band, overwrite = F, append = T)
        #TEMP = rbind(TEMP, band) 
        rm(ageband, band)
        gc()

}

rm(birthDays, startBands)


for(i in 1:length(matchingCat)){
  
  #Create a temp varaible to use with the name of the varaible
  varTemp <- matchingCat[i]
  print(varTemp)
  
  #Check if the needed concept is in the database
  if(varTemp %in% dbListTables(mydb)){
    
    #Import from database only needed columns
    TEMP <- as.data.table(unique(dbGetQuery(mydb, paste0("SELECT person_id, Date, Value FROM ",varTemp)) ))
    if(nrow(TEMP) > 0){
    
      #In the database dates are stored as integer. So set the data.table date format back.
    TEMP <-TEMP[, `:=` (Date = as.Date(Date, origin = "1970-01-01"))]
    
    #Set the colnames so that it can be added to 1 file in the matching database in a join. 
    setnames(TEMP, "Value", varTemp)
    
    #Run the function that gives a start and end date for every condition. All the time within the study period needs to be covered
    TEMP <- MakeSpells(
      Data = TEMP,
      c.id = "person_id",
      c.value = varTemp,
      c.date = "Date",
      start_study_date = studyStartDate2,
      end_study_date = studyEndDate2
    )[, c("person_id", "ST", "EN", varTemp) , with = F]  
    
    #merge the persons with the aim to add the subject that had 0 information.
    TEMP <- merge(x = PER[person_id %in% persons_of_interest,], y = TEMP, all.x  = T, by = "person_id")
    
    #make spells with unk/0 so that all the subjects have a spell
    ###
    if(class(TEMP[[varTemp]]) == "character"){ 
      TEMP <- TEMP[is.na(ST),eval(varTemp)  := "UNK"][is.na(ST), `:=`  (ST = studyStartDate2, EN = studyEndDate2)]
    }
    
    if(class(TEMP[[varTemp]]) %in% c("numeric", "integer")){ 
      TEMP <- TEMP[is.na(ST), eval(varTemp) := 0][is.na(ST), `:=`  (ST = studyStartDate2, EN = studyEndDate2)]
    }
    ###
    
    if(any(is.na(TEMP))) warning("NA's in spells file")
    
    saveRDS(TEMP, paste0(matchingDir, varTemp,"_SPELLS.rds"))
    
    }else{print("0 rows found so so this varaible is excluded from matching")}
    
    rm(TEMP)
    gc()
    
  }else{print("Not founded in database so this varaible is excluded from matching")}
  rm(varTemp)
 
}

###


rm(PER)
gc()



dbDisconnect(mydb)
#dbDisconnect(mydb2)
rm(mydb)







