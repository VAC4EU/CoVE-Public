##in/output
#Input: persons, clean_spells.rds, spell files per matching criteria
#Output: CASES table in dbmatcing

#Matching parameters
minFu <- 1

#Get all the information that is needed fro the meta files
###
files <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TIMEDEP == T,][["STUDY_VARIABLES"]])
files <- files[sapply(files, function(x) file.exists(paste0(matchingDir, x, "_SPELLS.rds")))]
matchingCat <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TYPE == "CAT",][["STUDY_VARIABLES"]])
matchingPersons <- tolower(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & ORIGIN == "PERSONS" & TIMEDEP == F,][["STUDY_VARIABLES"]])
matchingBrand <- c("type_vax1", "type_vax2", "type_vax3", "type_vax4")
###

### Loading Persons03

PER <- readRDS(paste0(matchingDir, "PER.rds"))[person_id %in% readRDS(paste0(d3Dir,"persons_of_interest.rds")),]

#Setting the per person_id and sex as integer and saving it into a dictionary
PER <- SetToInteger(PER, c("person_id", matchingPersons, matchingBrand))
Dictionary <- PER$Dictionary
saveRDS(Dictionary, paste0(matchingDir, "DIC_PER.rds"))

PER <- PER$Data


### Setting the IDs of the Observation Periods (OBS) to integer, as done to Persons Table
#Steps:
# IDs edited based on the dictionary defined after setting to integer the 'person_id' from the PER (persons) table
OBS <- RenameId(readRDS(paste0(matchingDir, "OBS.rds"))[person_id %in% readRDS(paste0(d3Dir,"persons_of_interest.rds")),], Dictionary = Dictionary, colls = "person_id", AddId = T)

#To get a minimal follow up time of 1 day subtract min.Fu from the end date. This can only be applied when 1 spell per subject
if(any(duplicated(OBS$person_id))) stop("Multiple spells per person") 
OBS <- OBS[, EN2 :=  EN2 - 1]



if(file.exists(dbMatchingFile)) file.remove(dbMatchingFile)
#opening a SQLite database
mydb <- dbConnect(RSQLite::SQLite(), dbMatchingFile)


#i=x[4]

x <- c(files, "PER", "OBS")
for(i in x){
 # i = "AGEBAND"
  if(i %in% files) TEMP <- RenameId(readRDS(paste0(matchingDir,i,"_SPELLS.rds"))[person_id %in% readRDS(paste0(d3Dir,"persons_of_interest.rds")),], Dictionary = Dictionary, colls = "person_id", AddId = T)
  if(i == "PER"){
    TEMP <- PER
    rm(PER)
  }
  if(i == "OBS"){
    TEMP <- OBS
    rm(OBS)
  }
  
  if(i %in% matchingCat & class(TEMP[[i]]) == "character"){ 
    TEMP <- SetToInteger(TEMP, i)
    Dictionary_TEMP <- TEMP$Dictionary
    saveRDS(Dictionary_TEMP, paste0(matchingDir,"DIC_",i,".rds"))
    rm(Dictionary_TEMP)
    
    TEMP <- TEMP$Data
  }
  
  colls <-colnames(TEMP)
  f.types <- rep("INT", length(colls))
  names(f.types) <- colls
  dbWriteTable(mydb, i ,TEMP, overwrite = T, append = F, field.types = f.types) 
  
  rm(TEMP, colls, f.types)
  gc()
  
  
  
  
  
  
}

rm(Dictionary)
rm(x)


dbListTables(mydb)




seqnb <- 2:length(files) #Index kilst of time dependent matching variables

#Generating a query for selecting columns ST and EN from each of the tables
# Example:
# When 2, we are using file CDC then the query is
# t2.CDC, t2.ST AS ST2, t2.EN AS EN2
# THis will be used with a SELECT statement later on and it is needed so we give a unique name to each column of each variable
CODE_SELECT <- paste0(
  
  "
  t",seqnb,".",files[seqnb],",
  t",seqnb,".ST AS ST",seqnb,",
  t",seqnb,".EN AS EN",seqnb,"
  "
  , collapse = ","
)

x <- 1:length(files)
#CODE_MAX and CODE_MIN are strings withthe ST (start) and EN (end) dates for each of the time dependent matching variables
# Example of CODE_MIN
# "t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN"

CODE_MAX <- paste0("t",x,".ST", collapse = ",")
CODE_MIN <- paste0("t",x,".EN", collapse = ",")

CODE_JOIN <- paste0(
  
  "
            INNER JOIN ",files[seqnb]," t",seqnb," ON(
            
            t1.person_id = t",seqnb,".person_id AND
            
            (
            t",seqnb,".ST BETWEEN max(",CODE_MAX,") AND min(",CODE_MIN,") OR
            t",seqnb,".EN BETWEEN max(",CODE_MAX,") AND min(",CODE_MIN,") OR
            (t",seqnb,".ST  < max(",CODE_MAX,") AND t",seqnb,".EN  > min(",CODE_MIN,"))
            )
            
            
            )
  
  
  "
  , collapse = " "
)

#Example of CODE_JOIN
#   INNER JOIN CDC t2 
#   ON  t1.person_id = t2.person_id 
#   AND (t2.ST BETWEEN max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN) 
#   OR t2.EN BETWEEN max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN) 
#   OR (t2.ST < max(t1.ST,t2.ST,t3.ST,t4.ST,t5.ST,t6.ST) AND t2.EN > min(t1.EN,t2.EN,t3.EN,t4.EN,t5.EN,t6.EN))))"



#The following statement inner joins the Region rows with each of the time dependent matching variables where
# ID of the person is the same and:
# the start of the matching variable (e.g. CDC) is between the max of all the starts and the minimum of all the ends (including region t1)
# or the end of the matching variable is between the max of all the starts and between the minimum of all the ends
# or the start of the matching variable is before than the max of all the starts of all matching variables and the end bigger than the min of all ends of all thematching variables

#This is done for all the matching variables

p <- dbSendStatement(mydb,
                     
                     paste0(
                       " 
      CREATE TABLE TEMP1 AS
      SELECT
      t1.*,
      ",CODE_SELECT,",
      ROW_NUMBER() OVER() tmp
      
      
      FROM ",files[1]," t1
      
            ",CODE_JOIN,"
      
      
      
      "))
dbClearResult(p)

CODE_MAX2 <- paste("ST, ", paste0(" ST",seqnb, collapse = ","))
#Example "ST,   ST2, ST3, ST4, ST5, ST6"
CODE_MIN2 <- paste("EN, ", paste0(" EN",seqnb, collapse = ","))
#Example #"EN,   EN2, EN3, EN4, EN5, EN6"
CODE_GROUP <- paste0(files, collapse = ",")
#exampple #"REG,CDC,COV,INF,IMC,PREG"

#test <- as.data.table(test)[, tmp := seq_len(.N)]


#In the statement below, from all the starts and ends that are joined in the 
#previous statement (resulting in TEMP1) we are selecting the minimun start and 
#the max end between all the ST and EN columns. This results in ONE spell each
# status of a person. Every status is defined by a combination of time dependent variables
p <- dbSendStatement(mydb,
                     
                     paste0(
                       "       CREATE TABLE TEMP2 AS
             SELECT DISTINCT * FROM(
              SELECT DISTINCT
              person_id,
              max(",CODE_MAX2,") AS ST,
              min(",CODE_MIN2,") AS EN,
              ",CODE_GROUP,"
              
              FROM TEMP1
              
              GROUP BY tmp
             )
             WHERE EN - ST >= 0
              
            
              "
                       
                     ))

dbClearResult(p)

#test2 <- merge(as.data.table(test2), PER[, .(person_id, sex_at_instance_creation, YEAR_BIRTH, FIRST_COV_INF, FIRST_PFIZER, FIRST_OTHER)], by.x = "ID", by.y = "person_id", all.x = T )


#In the statement below we are including all the time-independent matching variables
# information into to the result of the TEMP2 (status spells)

selectT2 <- dbListFields(mydb, "PER")
selectT2 <- paste0("t2.",paste0(selectT2[!selectT2 %in% "person_id"], collapse = " ,t2."))

p <- dbSendStatement(mydb,
     paste0(                
                     "
  CREATE TABLE TEMP3 AS
  SELECT DISTINCT
  t1.*,
  ",selectT2,"
  
  
  FROM TEMP2 t1
  
  LEFT JOIN PER t2 ON(t1.person_id = t2.person_id)
  
  "
                     
                     
)
)
#Finally we divide the spells of the controls and exmposed into two different tables
#Creation of the exposed and control files for the matching 

dbClearResult(p)

p <- dbSendStatement(mydb,
                     
                     "
  CREATE TABLE CASES AS
  SELECT DISTINCT
  t1.*,
  t2.ST2, 
  t2.EN2,
  
  max(t2.ST2, t1.ST) AS ST3,
  min(t2.EN2, t1.EN) AS EN3
  
  
  FROM TEMP3 t1
  
  INNER JOIN OBS t2 ON(t1.person_id = t2.person_id AND 
  
            (
            t1.ST BETWEEN t2.ST2 AND t2.EN2 OR
            t1.EN BETWEEN t2.ST2 AND t2.EN2 OR
            (t1.ST  < t2.ST2 AND t1.EN  > t2.EN2)
            )
  
  
  )
  
  
  
  "
                     
                     
)

dbClearResult(p)


#Create indexes
p <- dbSendStatement(mydb, paste0("CREATE INDEX CASES_index ON CASES(",paste0(dbListFields(mydb, "CASES"), collapse = ","),")"))
dbClearResult(p)
#dbReadTable(mydb, "TEMP2")

dbDisconnect(mydb)




rm(mydb, p,  CODE_GROUP, CODE_JOIN, CODE_MAX, CODE_MAX2, CODE_MIN, CODE_MIN2, CODE_SELECT, files, seqnb, x)
gc()


### Create D3_study_population_with_matching_variables
#This is old code that I forgot to remove but because Davide is using it after all it needs to be kept in. I cannot remember anymore what the purpose was of this code?? 

study_pop <- readRDS(paste0(d3Dir, "Combine.rds"))
 
study_pop <- unique(study_pop[,.(person_id, sex_at_instance_creation, date_birth, type_vax1, type_vax2, date_vax2)])
study_pop[, age_at_time0 := floor(time_length(interval(date_birth, date_vax2),"year"))]
 
startBands <- c(-1, 4, 11, 17, 29, 49, 59, 69, 79, Inf)
test <- c("0-4", "5-11", "12-17", "18-29", "30-49", "50-59", "60-69", "70-79", "80+")
study_pop[, ageband_at_time0 := as.character(cut(age_at_time0,
                                                  breaks = startBands, labels = test))]

variables_at_time0 <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TIMEDEP == T ,][["STUDY_VARIABLES"]])
variables_at_time0 <- variables_at_time0[!variables_at_time0 %in% "AGEBAND"]

pop_vax2 <- study_pop[, .(person_id, date_vax2)]
 
for (variable in variables_at_time0) {
  
  if(file.exists(paste0(matchingDir, variable, "_SPELLS.rds"))){
    temp <- readRDS(paste0(matchingDir, variable, "_SPELLS.rds"))
    temp <- merge(temp, pop_vax2, all.y = T, by = "person_id")
    temp <- temp[ST <= date_vax2 & EN >= date_vax2, ]
    cols <- sapply(temp, is.logical)
    cols <- names(cols[cols == T])
    if (length(cols) > 0) {
      temp[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    }
    temp <- temp[, c("person_id", variable), with = FALSE]
    setnames(temp, variable, paste0(tolower(variable), "_at_time0"))
    study_pop <- merge(study_pop, temp, all.x = T, by = "person_id")
  }
}
  
# Check if columns do not exists

check_columns_do_not_exist <- function(start_df, columns) {
  columns[!grepl(paste(colnames(start_df), collapse = "|"), columns)]
}

variables_at_time0_in_df <- check_columns_do_not_exist(study_pop, paste0(tolower(variables_at_time0), "_at_time0"))
if (length(variables_at_time0_in_df) != 0) {
  study_pop[, (variables_at_time0_in_df) := NA]
}

study_pop[, (variables_at_time0_in_df) := NA]

study_pop <- study_pop[, c("person_id", "sex_at_instance_creation", "ageband_at_time0", "age_at_time0", "type_vax1",
                           "type_vax2", "l_georegion_cov_at_time0", "immunodeficiency_at_time0","cancer_at_time0",
                           "im_transplantrecipient_cov_at_time0","g_severerenaldisease_ch_at_time0",
                           "o_down_cov_at_time0", "free_covid_at_time0"), ]
 
 saveRDS(study_pop, paste0(d3Dir, "D3_study_population_with_matching_variables.rds"))
 rm(temp, study_pop, cols, variable, pop_vax2, variables_at_time0)

 # study_pop <- study_pop[,.(person_id, sex_at_instance_creation, age_at_time0, ageband_at_time0, type_vax1,
#                           type_vax2, l_georegion_cov_at_time0, immunodeficiency_at_time0,cancer_at_time0,
#                           im_transplantrecipient_cov_at_time0,g_severerenaldisease_ch_at_time0,
#                           o_down_cov_at_time0)]
