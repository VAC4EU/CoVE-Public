

#Set up database connection
mydb <- dbConnect(RSQLite::SQLite(), db = dbConceptsFile)

#Get the output of the matching
#dbReadTable(mydb, "POPULATIONS")
file <- readRDS(paste0(d3Dir,"POPULATIONS.rds"))



#Load function
SetCensoringDate <- function(data, dates.file, dates.file.c = NULL, c.by, c.id, c.population = NULL, v.population = NULL, colname.date = "DATE_CENSORING", colname.reason = "REASON_CENSORING", refdate.c = NULL){
  
  if(!is.null(c.population)){data <- copy(data)[get(c.population) == v.population, c(dates.file, c.by, c.id, refdate.c, unname(dates.file.c)) , with = F]}else{
    data <- copy(data)[ , c(dates.file, c.by, c.id) , with = F]
  }
  
  expr2 <- paste("DT2 := min(",paste0(dates.file, collapse = ", "), ", na.rm=TRUE)")
  data <- data[, eval(parse(text = expr2 )), by = c(c.id, c.by)]
  data <- data[, eval(colname.reason) := as.character()] 
  
  for(i in 1:length(dates.file)){
    if(dates.file[i] %in% names(dates.file.c)){
      data <- data[DT2 == get(dates.file[i]) & is.na(get(colname.reason)) , eval(colname.reason) := get(dates.file.c[[dates.file[i]]])  ]
    }else{
      data <- data[DT2 == get(dates.file[i]) & is.na(get(colname.reason)) , eval(colname.reason) :=  dates.file[i] ]
    }
  }
  
  setnames(data, "DT2", colname.date)
  data <- data[, c(c.id, c.by, colname.date, colname.reason, refdate.c), with = F]
  
  return(data)
  
}


#Set paramters in file that can be added to meta later on
pop <- c("HO_P_TO_HE_P", "HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB", "ANY_P_with_prior_covid_B_to_NB", "children")
dExposed <- c("end_no_booster", "end_no_4dose", "end_no_4dose", "end_no_4dose", "end_no_4dose", "end_no_booster")
dComparator <- c("end_no_booster", "end_no_booster", "end_no_booster", "end_no_booster", "end_no_booster", "end_no_vaccine")

scheme <- as.data.table(cbind(pop, dExposed, dComparator))

#i = 5

#Apply the function for everey row of scheme

for(i in 1:nrow(scheme)){
  
  tmp <- copy(file)[, c("person_id", "pair_ID", "T0", "exposed_or_comparator", "matched_cohort", "study_exit_date", unique(c(scheme$dExposed, scheme$dComparator))), with = F][matched_cohort == scheme[i,][["pop"]], ]
  
  if(nrow(tmp) > 0){
    tmp <- tmp[, REF_DATE := fifelse(exposed_or_comparator == "comparator", get(scheme[i,][["dComparator"]]), get(scheme[i,][["dExposed"]]))]
    tmp <- tmp[, REF_DATE_LABEL := fifelse(exposed_or_comparator == "comparator", scheme[i,][["dComparator"]], scheme[i,][["dExposed"]])] 
    tmp <- SetCensoringDate(
      data = tmp, 
      dates.file = c("study_exit_date", "REF_DATE"),
      dates.file.c = c(REF_DATE = "REF_DATE_LABEL"),
      c.id = "person_id", 
      c.population = "matched_cohort", 
      v.population = scheme[i,][["pop"]], 
      colname.date = "date_tentative_censoring",
      colname.reason = "reason_for_tentative_censoring",
      c.by = "pair_ID",
      refdate.c = "T0"
    )
    
    
    
    if(i == 1) all <- tmp
    if(i > 1) all <- rbindlist(list(all, tmp))
    
    rm(tmp)
  }else{print(paste0("No matches for ", scheme[i,][["pop"]]))}
}

#data <- SetCensoringDate(data = file, dates.file = c("exit_spell_category_crude", "date_vax3"), pair = c(T, T), c.by = "pair_ID", c.id = "person_id", dates = c("studyEndDate2"), c.population = "POP", v.population = "HO_P_TO_HE_P", colname = "date_final_censoring")

#Complete censoring variables
###
datesToT0 <- c("date_tentative_censoring")
all <- GetDayDif(data = all, date.ref = "T0", dates = datesToT0, postfix = "_days")
rm(datesToT0)
all <- all[, date_final_censoring_days := min(date_tentative_censoring_days), by = "pair_ID" ]
all <- all[, date_final_censoring := T0 + date_final_censoring_days, by = "pair_ID" ][, T0 := NULL]
all <- all[, reason_for_final_censoring :=  ifelse(date_final_censoring_days != date_tentative_censoring_days, "censoring of the other member of the pair" , reason_for_tentative_censoring) , by = "pair_ID" ]
###




#For developing I parallel build the database and for the PI's I make rds files for now.
###
dbWriteTable(mydb, "CENSORING", all,  overwrite = T) 
dbDisconnect(mydb)
file <- merge(file, all, all.x = T, by = c("person_id", "pair_ID"))
saveRDS(file, paste0(d3Dir,"POPULATIONS2",".rds" ))
###

rm(all, file)
gc()






