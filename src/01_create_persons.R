# input: PERSONS, OBSERVATION_PERIODS
# output: D3_PERSONS, D3_events_DEATH

print('PRE-PROCESSING OF PERSONS')

# import input datasets
PERSONS <- ImportPattern(pat = "PERSONS", 
                          dir = pathCDM,
                          colls = c("person_id", "day_of_birth", "month_of_birth", "year_of_birth", "sex_at_instance_creation", "day_of_death", "month_of_death", "year_of_death")

                          )
OBSERVATION_PERIODS <- ImportPattern(pat = "OBSERVATION_PERIODS", 
                                      dir = pathCDM, 
                                      date.colls = c("op_start_date", "op_end_date"),
                                      colls = c("person_id", "op_start_date", "op_end_date")
                                                      )
saveRDS(OBSERVATION_PERIODS, file = paste0(d3Dir,"D3_OBSERVATION_PERIODS.rds"))



# decide if pre-processing is needed for birth date (Present year, missing day or month)
PERSONS_date_missing <- PERSONS[!is.na(year_of_birth) & (is.na(day_of_birth) | is.na(month_of_birth)),]
if(nrow(PERSONS_date_missing) != 0){
  
  print('SOME PERSONS HAVE DAYS OR MONTHS OF BIRTH MISSING')
  
  # Get the first start of observation periods for each person
  min_OBSERVATION_PERIODS <- OBSERVATION_PERIODS[!is.na(op_end_date), ]
  min_OBSERVATION_PERIODS <- min_OBSERVATION_PERIODS[, .(op_start_date = min(op_end_date)), by = person_id]
  min_OBSERVATION_PERIODS <- unique(min_OBSERVATION_PERIODS[,.(person_id, op_start_date)])
  
  # Merge persons with a missing date(as explained above) with observation periods
  CreateDateBirth <- merge(PERSONS_date_missing, min_OBSERVATION_PERIODS, all.x = T, by="person_id")
  
  # start of first observation periods assumed date of birth
  CreateDateBirth[, assumed_year_birth := year(op_start_date)]
  CreateDateBirth[, assumed_month_birth := month(op_start_date)]
  CreateDateBirth[, assumed_day_birth := day(op_start_date)]
  
  # The order is IMPORTANT: first recode just day then in case month + day
  CreateDateBirth <- CreateDateBirth[year_of_birth == assumed_year_birth & month_of_birth == assumed_month_birth & is.na(day_of_birth),
                                     day_of_birth := assumed_day_birth]
  CreateDateBirth <- CreateDateBirth[year_of_birth == assumed_year_birth & month_of_birth == assumed_month_birth & is.na(day_of_birth),
                                     birth_day_imputed := 1]
  
  CreateDateBirth <- CreateDateBirth[year_of_birth == assumed_year_birth & is.na(month_of_birth),
                                     c("month_of_birth", "day_of_birth") := list(assumed_month_birth, assumed_day_birth)]
  CreateDateBirth <- CreateDateBirth[year_of_birth != assumed_year_birth,
                                     c("month_of_birth", "day_of_birth") := list(6, 30)]
  CreateDateBirth <- CreateDateBirth[year_of_birth != assumed_year_birth | (year_of_birth == assumed_year_birth & is.na(month_of_birth)),
                                     c("birth_month_imputed", "birth_day_imputed") := list(1, 1)]
  
  # Clean the dataset
  CreateDateBirth <- CreateDateBirth[, c("op_start_date", "assumed_year_birth",
                                         "assumed_month_birth", "assumed_day_birth") := NULL]
  
  # Combine the persons which didn't need the correction for birth date to the one with the imputation
  PERSONS <- rbind(PERSONS[is.na(year_of_birth) | (!is.na(day_of_birth) & !is.na(month_of_birth)),],
                      CreateDateBirth, fill = T)
  
  rm(CreateDateBirth, min_OBSERVATION_PERIODS)
  
  print('DATE OF BIRTH IN PERSONS ADJUSTED')
}

rm(PERSONS_date_missing)


# RETRIEVE FROM PERSONS ALL DEATHS AND SAVE
print('TRANSFORM in COMPLETED DATE FOR BIRTH and DEATH')

missing_date <- "9999-12-31"

# Convert birth date and clean the dataset
PERSONS[is.na(year_of_birth) | is.na(month_of_birth) | is.na(day_of_birth), date_birth := missing_date]
PERSONS[!is.na(year_of_birth) & !is.na(month_of_birth) & !is.na(day_of_birth),
           date_birth := paste(year_of_birth, month_of_birth, day_of_birth, sep = "-")]
PERSONS[, date_birth := lubridate::ymd(date_birth)]
PERSONS[, c("year_of_birth", "month_of_birth", "day_of_birth") := NULL]

#### Convert death date and clean the dataset
cols <- c("year_of_death", "month_of_death", "day_of_death")
PERSONS[!is.na(year_of_death) & !is.na(month_of_death) & !is.na(day_of_death),
        date_death := paste(year_of_death, month_of_death, day_of_death, sep = "-")]
PERSONS[, (cols) := lapply(.SD, is.na), .SDcols = cols]
PERSONS[, flag := rowSums(.SD), .SDcols = cols]
PERSONS[flag %in% c(1, 2), date_death := missing_date]
PERSONS <- PERSONS[, date_death := lubridate::ymd(date_death)][, flag := NULL]

PERSONS <- PERSONS[, c("year_of_death", "month_of_death", "day_of_death") := NULL]

# Imputation of missing values
for (i in names(PERSONS)[names(PERSONS) != "date_death"]){
  PERSONS[is.na(get(i)), (i) := 0]
}

# Create and save D3_events_DEATH
events_DEATH <- PERSONS[!is.na(date_death),.(person_id, date_death)][, date := date_death][, -"date_death"]
saveRDS(events_DEATH, file = paste0(d3Dir,"D3_EVENTS_DEATH.rds"))
rm(events_DEATH)

# Save D3_PERSONS
saveRDS(PERSONS,file = paste0(d3Dir, "D3_PERSONS.rds"))

rm(PERSONS, OBSERVATION_PERIODS)