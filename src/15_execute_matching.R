
#Aim make matches for the different populations
#Inputs: dbMatchingFile (CASES)
#Outputs: Population in dbConceptsFile

nb_batch <- NULL

#Load the matching parameters that are needed as the input for the matching functions
files <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TIMEDEP == T,][["STUDY_VARIABLES"]])
files <- files[sapply(files, function(x) file.exists(paste0(matchingDir, x, "_SPELLS.rds")))]

#Distinct between different matching varaibles types
matchingCat <- toupper(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & TYPE == "CAT",][["STUDY_VARIABLES"]])
matchingPersons <- tolower(ImportPattern(pat = "study_variables", dir = metaDir)[MATCHING == T & ORIGIN == "PERSONS" & TIMEDEP == F,][["STUDY_VARIABLES"]])


ProcesMatches <- function(cohort = NULL, two.T0 = NULL){
  lapply(files[!files %in% matchingCat], function(x) MATCHED <- MATCHED[, eval(x) := as.logical(get(x))] )
  MATCHED <- MATCHED[is.na(T0_CONTROL), T0_CONTROL := T0]
  
  Dictionary <- readRDS(paste0(matchingDir, "DIC_PER.rds"))
  
  
  Dictionary$Exposed <- Dictionary$person_id
  colnames(Dictionary$Exposed) <- c("Exposed", "ID")
  Dictionary$Control <- Dictionary$person_id
  colnames(Dictionary$Control) <- c("Control", "ID")
  Dictionary$person_id <- NULL
  MATCHED <- RenameId(Data = MATCHED, Dictionary = Dictionary, colls = names(Dictionary)[names(Dictionary) %in% colnames(MATCHED)], AddId = F)
  
  
  for(i in matchingCat){
    
    if(file.exists(paste0(matchingDir,"DIC_",i,".rds"))){Dictionary_tmp <- readRDS((paste0(matchingDir,"DIC_",i,".rds")))
    MATCHED <- RenameId(Data = MATCHED, Dictionary = Dictionary_tmp, colls = names(Dictionary_tmp), AddId = F)
    rm(Dictionary_tmp)
    }
  }  
  
  setorder(MATCHED, T0, Exposed)
  MATCHED <- MATCHED[, pair_ID := row.names(MATCHED)]
  MATCHED[, T0 := as.Date(T0, origin = "1970-01-01")]
  MATCHED[, T0_CONTROL := as.Date(T0_CONTROL, origin = "1970-01-01")]
  
  setcolorder(MATCHED, c("Exposed","Control","T0","T0_CONTROL","pair_ID","nb_match"))
  
  saveRDS(MATCHED, paste0(d3Dir,"MATCHED_PAIRS",".rds" ))
  return(MATCHED)
  
  
  
  
}

#make scheme as the input for the function.
###
pop <- c("HO_P_TO_HE_P", "HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB", "ANY_P_with_prior_covid_B_to_NB", "children")
expGroup <- c("HO_P", "HO_P_HO_B", "HO_P_HE_B", "HE_P_B", "ANY_P_with_prior_covid_B" , "CHILD_P")
#conGroup <- c("HE_P", "NB", "NB", "NB", "NB", "CHILD")
#Fix wrong input
conGroup <- c("HE_P", "HO_P", "HO_P", "HE_P", "ANY_P_with_prior_covid", "CHILD")

expT0 <- c("date_vax2", "date_vax3", "date_vax3", "date_vax3", "date_vax3", "date_vax2")
conT0 <- c("date_vax2", "NULL", "NULL", "NULL" , "NULL", "NULL")

#It may be that additional criteria a formulated. These can be inputted via this section
##
optionalJoinCode <- c(
  "AND ABS(t1.date_vax1 - t2.date_vax1) < 7 AND ABS(t1.date_vax2 - t2.date_vax2) < 7 AND t1.type_vax1 = t2.type_vax1  ",
  "AND ABS(t1.date_vax1 - t2.date_vax1) < 7 AND ABS(t1.date_vax2 - t2.date_vax2) < 7  AND t1.type_vax1 = t2.type_vax1 ",
  "AND ABS(t1.date_vax1 - t2.date_vax1) < 7 AND ABS(t1.date_vax2 - t2.date_vax2) < 7 AND t1.type_vax1 = t2.type_vax1 AND t1.type_vax2 = t2.type_vax2 ",
  "AND ABS(t1.date_vax1 - t2.date_vax1) < 7 AND ABS(t1.date_vax2 - t2.date_vax2) < 7 AND t1.type_vax1 = t2.type_vax1  ",
  "AND ABS(t1.date_vax1 - t2.date_vax1) < 7 AND ABS(t1.date_vax2 - t2.date_vax2) < 7 ",
  "AND (t1.date_vax2  < t2.date_vax1 OR t2.date_vax1 IS NULL) "
)

optionalSelectCodeExp <- c(
  ", t1.date_vax1, t1.type_vax1 " ,
  ", t1.date_vax1, t1.date_vax2, t1.type_vax1 ",
  ", t1.date_vax1, t1.date_vax2, t1.type_vax1, t1.type_vax2  ",
  ", t1.date_vax1, t1.date_vax2, t1.type_vax1 ",
  ", t1.date_vax1, t1.date_vax2 ",
  ", t1.ST2, t1.EN2 "

)

optionalSelectCodeCon <- c(
  ", t2.date_vax1, t2.type_vax1 ",
  ", t2.date_vax1, t2.date_vax2, t2.type_vax1 ",
  ", t2.date_vax1, t2.date_vax2, t2.type_vax1, t2.type_vax2 ",
  ", t2.date_vax1, t2.date_vax2, t2.type_vax1 ",
  ", t2.date_vax1, t2.date_vax2",
  ", t2.date_vax1"
  
)

##
#

###

scheme <- as.data.table(cbind(pop, expGroup, conGroup, expT0, conT0, optionalJoinCode, optionalSelectCodeExp, optionalSelectCodeCon))
saveRDS(scheme, paste0(d3Dir, "matching_quries.rds"))

#Only the first and 4th row of scheme I have specified so I am working with that until I also did the others populations.
#scheme <- scheme[c(1,4), ]


for(i in 1:nrow(scheme)){

expGroup <- scheme[i,][["expGroup"]]
if(expGroup == "NULL") expGroup <- NULL
conGroup <- scheme[i,][["conGroup"]]
if(conGroup == "NULL") conGroup <- NULL
expT0 <- scheme[i,][["expT0"]]
if(expT0 == "NULL") expT0 <- NULL
conT0 <- scheme[i,][["conT0"]]
if(conT0 == "NULL") conT0 <- NULL
pop <- scheme[i,][["pop"]]
if(pop == "NULL") pop <- NULL
optionalJoinCode <- scheme[i,][["optionalJoinCode"]]
if(optionalJoinCode == "NULL") optionalJoinCode <- NULL
optionalSelectCodeExp <- scheme[i,][["optionalSelectCodeExp"]]
if(optionalSelectCodeExp == "NULL") optionalSelectCodeExp <- NULL
optionalSelectCodeCon <- scheme[i,][["optionalSelectCodeCon"]]
if(optionalSelectCodeCon == "NULL") optionalSelectCodeCon <- NULL

    print(pop)    

    if(is.null(nb_batch)){
    MATCHED_tmp <- MatchingSQLSpells(
    db = dbMatchingFile,
    colls = files,
    colls2 = matchingPersons,
    ids = NULL,  
    print = F,
    exposed.t0 = expT0,
    control.t0 = conT0, 
    exposed.group = expGroup, 
    control.group = conGroup,
    code.join = optionalJoinCode,
    select.exp =  optionalSelectCodeExp,
    select.con =  optionalSelectCodeCon
    
    )[, matched_cohort := pop]
    }else{MATCHED_tmp <- MatchingSQLSpellsBatched(batchSize = nb_batch)}

    
    
if(i==1) MATCHED <- MATCHED_tmp    
if(i>1) MATCHED <- rbindlist(list(MATCHED, MATCHED_tmp), fill = T, use.names = T)
        
#ProcesMatches(cohort = pop)

rm(MATCHED_tmp)
gc()
}

MATCHED <- ProcesMatches()

rm(matchingCat, matchingPersons  , files)
gc()


###From wide to long
Exposed <- copy(MATCHED)[, exposed_or_comparator := "exposed"][, T0_CONTROL := NULL][, matched := fifelse(is.na(Control), F, T)][, Control := NULL]
Control <- copy(MATCHED)[!is.na(Control)][, exposed_or_comparator := "comparator"][, T0 := NULL][, Exposed := NULL][, matched := T]
setnames(Exposed,"Exposed","person_id")
setnames(Control,"Control","person_id")
setnames(Control,"T0_CONTROL","T0")
rm(MATCHED)
gc()

POP <- rbindlist(list(Exposed,Control), use.names = T, fill = T)[, c("person_id", "pair_ID","T0","matched_cohort", "matched", "exposed_or_comparator","sex_at_instance_creation", "AGEBAND"), with = F]
if(nrow(POP) !=  (nrow(Control) + nrow(Exposed))) stop("Multiple spells overlap with an t0")
rm(Exposed, Control)
###


#Get needed information to compose the outfile for statistical analisis.
###
SPELLS <- readRDS(paste0(d3Dir, "D3_CLEAN_SPELLS.rds"))[, .(person_id, entry_spell_category_crude, exit_spell_category_crude )]
PER <- readRDS(paste0(d3Dir, "D3_PERSONS.rds"))[, .(person_id, date_birth, date_death  )]
VAC <- readRDS(paste0(matchingDir, "PER.rds"))[, .(person_id, type_vax1,date_vax1, type_vax2,date_vax2, type_vax3,date_vax3, type_vax4,date_vax4  )]

#mydb <- dbConnect(RSQLite::SQLite(), db = dbMatchingFile)
#VAC <- dbGetQuery(mydb, "SELECT person_id, type_vax1,date_vax1, type_vax2,date_vax2, type_vax3,date_vax3  FROM PER")
#dbDisconnect(mydb)
###

#Create file with needed columns. Rest of the studyvaraibles are added in step 13
###
POP <- as.data.table(sqldf(
  
  "
SELECT DISTINCT
t1.*,
t2.entry_spell_category_crude ,
t2.exit_spell_category_crude,
t3.date_birth, 
t3.date_death,
t4.type_vax1,
t4.date_vax1, 
t4.type_vax2,
t4.date_vax2, 
t4.type_vax3,
t4.date_vax3,
t4.type_vax4,
t4.date_vax4

FROM POP t1

left join SPELLS t2 on (t1.person_id = t2.person_id AND  t1.T0 BETWEEN t2.entry_spell_category_crude AND t2.exit_spell_category_crude)
left join PER t3 on (t1.person_id = t3.person_id)
left join VAC t4 on (t1.person_id = t4.person_id)

"
))   
rm(PER,SPELLS, VAC)

#Create additional column names as described for this study.
###
POP <- POP[, age_at_matching_date := floor(time_length(interval(date_birth, T0),"year"))]
POP <- POP[, monthyear_at_matching_date := paste0(formatC(month(T0), width = 2, flag = "0"), year(T0))]
POP <- POP[, birth_year := as.integer(year(date_birth))]
POP <- POP[, end_no_vaccine  := date_vax1 - 1]
POP <- POP[, end_no_booster  := date_vax3 - 1]
POP <- POP[, end_no_4dose  := date_vax4 - 1]
setnames(POP, c("exit_spell_category_crude"), c("study_exit_date"))

###


###

#Do checks on dates

if(any(is.na(POP[["entry_spell_category_crude"]]))) stop("NA in op_start_date")
if(any(is.na(POP[["study_exit_date"]]))) stop("NA in op_end_date")


setorder(POP, pair_ID, exposed_or_comparator)


saveRDS(POP, paste0(d3Dir,"POPULATIONS",".rds" ))

mydb <- dbConnect(RSQLite::SQLite(), dbConceptsFile)
dbWriteTable(mydb, "POPULATIONS", POP, overwrite = T, append = F)
dbDisconnect(mydb)


rm(POP, mydb)
gc()



AddIndexes(db.path = dbConceptsFile, needed.fields = NULL, remove.duplicated = T, index.all = T, tables =  c("POPULATIONS") )




















