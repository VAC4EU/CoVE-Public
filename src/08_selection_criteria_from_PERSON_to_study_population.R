# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_sel_cri

print('CREATE EXCLUSION CRITERIA FOR STUDY POPULATION')



PERSONS <- readRDS(paste0(d3Dir, "D3_PERSONS.rds"))

### Create the criteria based on D3_PERSONS. They are the same for adults and children populations.
# Remove persons with sex or birth day missing (recoded to year 9999)
sel_cri <- PERSONS[, sex_or_date_birth_is_not_defined := fifelse(is.na(sex_at_instance_creation) |
                                                                   sex_at_instance_creation == "U" |
                                                                   year(date_birth) == 9999, 1, 0)]

# Remove persons with absurd date of birth
sel_cri[, birth_date_absurd := fifelse(year(date_birth) < 1905, 1, 0)]

# Remove persons with partial date of death
sel_cri[, no_details_on_death := fifelse(!is.na(date_death) & year(date_death) == 9999, 1, 0)]

# Clean dataset
sel_cri <- sel_cri[, .(person_id, sex_or_date_birth_is_not_defined, birth_date_absurd, no_details_on_death)]


### Create the criteria based on D3_clean_spells. The following criteria are the same for adults and children populations.
# Import D3_clean_spells

clean_spells <- readRDS(paste0(d3Dir, "D3_CLEAN_SPELLS.rds"))


clean_spells <- clean_spells[, .(person_id, entry_spell_category, exit_spell_category,
                                 starts_at_birth, starts_after_ending, date_birth)]

# Creation of no_spells criteria
sel_cri <- sel_cri[, no_spells := fifelse(person_id %in% unlist(unique(clean_spells[, .(person_id)])), 0, 1)]


# Creation of all_spells_start_after_ending criteria
clean_spells[, tot_spell_num := .N, by = person_id]
clean_spells[, tot_starts_after_ending := sum(starts_after_ending), by = person_id]
clean_spells[, all_spells_start_after_ending := fifelse(tot_starts_after_ending == tot_spell_num, 1, 0)]
clean_spells[, removed_row := starts_after_ending]
clean_spells[, c("starts_after_ending", "tot_starts_after_ending", "tot_spell_num") := NULL]

# Creation of no_spell_overlapping_the_study_period criteria
clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
clean_spells[removed_row == 0, spell_not_in_study_period := fifelse(entry_spell_category > as.Date(studyEndDate, format = "%Y%m%d") |
                                                                      exit_spell_category < as.Date(studyStartDate, format = "%Y%m%d"), 1, 0)]
clean_spells[removed_row == 0, tot_spell_not_in_study_period := sum(spell_not_in_study_period), by = person_id]
clean_spells[removed_row == 0, no_spell_overlapping_the_study_period := fifelse(tot_spell_not_in_study_period ==
                                                                                  tot_spell_num, 1, 0)]
clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "spell_not_in_study_period")]
clean_spells[, c("spell_not_in_study_period", "tot_spell_not_in_study_period", "tot_spell_num") := NULL]

# Creation of no_spell_longer_than_730_days. Keep other spells even if they are less than 730 days long if they start at birth
clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]

correct_difftime <- function(t1, t2, t_period = "days") {
  return(difftime(t1, t2, units = t_period) + 1)
}

clean_spells[removed_row == 0,
             spell_less_730_days := fifelse(correct_difftime(exit_spell_category, entry_spell_category) < 730 & starts_at_birth == 0, 1, 0)]
clean_spells[removed_row == 0, tot_spell_less_730_days := sum(spell_less_730_days), by = person_id]
clean_spells[removed_row == 0, no_spell_longer_than_730_days := fifelse(tot_spell_less_730_days ==
                                                                          tot_spell_num, 1, 0)]
clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "spell_less_730_days")]
clean_spells[, c("spell_less_730_days", "tot_spell_less_730_days", "tot_spell_num") := NULL]

### Other criteria based on D3_clean_spells. The following criteria different for adults and children populations.
# Creation of too_young criteria
clean_spells_adults <- copy(clean_spells)
clean_spells_adults[removed_row == 0, tot_spell_num := .N, by = person_id]

age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


clean_spells_adults[removed_row == 0,
                    too_young_single_spell := fifelse(age_fast(date_birth, pmin(studyEndDate2, exit_spell_category)) < 12, 1, 0)]
clean_spells_adults[removed_row == 0, tot_too_young := sum(too_young_single_spell), by = person_id]
clean_spells_adults[removed_row == 0, too_young := fifelse(tot_too_young == tot_spell_num, 1, 0)]
clean_spells_adults[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "too_young_single_spell")]
clean_spells_adults[, c("too_young_single_spell", "tot_too_young", "tot_spell_num") := NULL]

clean_spells_children <- copy(clean_spells)
clean_spells_children[removed_row == 0, tot_spell_num := .N, by = person_id]
clean_spells_children[removed_row == 0,
                      too_young_single_spell := fifelse(age_fast(date_birth, pmin(studyEndDate2, exit_spell_category)) < 5, 1, 0)]
clean_spells_children[removed_row == 0, tot_too_young := sum(too_young_single_spell), by = person_id]
clean_spells_children[removed_row == 0, too_young := fifelse(tot_too_young == tot_spell_num, 1, 0)]
clean_spells_children[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "too_young_single_spell")]
clean_spells_children[, c("too_young_single_spell", "tot_too_young") := NULL]

# Creation of too_old criteria
clean_spells_children[removed_row == 0,
                      too_old_single_spell := fifelse(age_fast(date_birth, pmax(studyStartDate2, entry_spell_category)) > 12, 1, 0)]
clean_spells_children[removed_row == 0, tot_too_old := sum(too_old_single_spell), by = person_id]
clean_spells_children[removed_row == 0, too_old := fifelse(tot_too_old == tot_spell_num, 1, 0)]
clean_spells_children[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "too_old_single_spell")]
clean_spells_children[, c("too_old_single_spell", "tot_too_old", "tot_spell_num") := NULL]

# Keep only study spells chosen as removed_row == 0 and the latest one (vax2 more almost sure if happened)
study_spells_adults <- clean_spells_adults[removed_row == 0, ][, .(person_id, entry_spell_category, exit_spell_category)]
study_spells_adults <- unique(study_spells_adults)[, lapply(.SD, max), by = person_id]

study_spells_children <- clean_spells_children[removed_row == 0, ][, .(person_id, entry_spell_category, exit_spell_category)]
study_spells_children <- unique(study_spells_children)[, lapply(.SD, max), by = person_id]


# Keep only one row for each spell which syntethize the previously defined exclusion criteria
clean_spells_adults <- unique(clean_spells_adults[, c("entry_spell_category", "exit_spell_category",
                                                      "date_birth", "removed_row") := NULL])
for (i in names(clean_spells_adults)) {clean_spells_adults[is.na(get(i)), (i):=0]}
clean_spells_adults <- clean_spells_adults[, lapply(.SD, max), by = person_id]

clean_spells_children <- unique(clean_spells_children[, c("entry_spell_category", "exit_spell_category",
                                                          "date_birth", "removed_row") := NULL])
for (i in names(clean_spells_children)) {clean_spells_children[is.na(get(i)), (i):=0]}
clean_spells_children <- clean_spells_children[, lapply(.SD, max), by = person_id]

# Add start_at_birth again
study_spells_adults <- merge(study_spells_adults,
                             clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_at_birth)],
                             all.x = T, by = c("person_id", "entry_spell_category", "exit_spell_category"))
study_spells_children <- merge(study_spells_children,
                               clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_at_birth)],
                               all.x = T, by = c("person_id", "entry_spell_category", "exit_spell_category"))



### Create the criteria based on vaccines_curated. The following criteria are the same for adults and children populations.
# Import doses dataset and create doses criteria
vaccines_curated <- readRDS(paste0(d3Dir,"D3_VACCINES_CURATED.rds"))

# Creation of higher_doses_included_but_lower_doses_missing criteria
vaccines_curated[, expected_doses_num := seq_len(.N), by = person_id]
vaccines_curated[, flag := fifelse(dose_curated != expected_doses_num, T, F)]
vaccines_curated[, higher_doses_included_but_lower_doses_missing := fifelse(any(flag), 1, 0), by = person_id]
vaccines_curated[, removed_row := higher_doses_included_but_lower_doses_missing]
vaccines_curated[, c("flag", "expected_doses_num") := NULL]

vaccines_curated_children <- copy(vaccines_curated)

### Other criteria based on vaccines_curated The following criteria are only for adults population.
# Creation of unvaccinated criteria
sel_cri <- sel_cri[, unvaccinated := fifelse(person_id %in% unlist(unique(vaccines_curated[, .(person_id)])), 0, 1)]

# Creation of one_of_the_vaccines_is_unknown
vaccines_curated[removed_row == 0, flag := fifelse(manufacturer_curated == "unk", T, F)]
vaccines_curated[removed_row == 0, one_of_the_vaccines_is_unknown := fifelse(any(flag), 1, 0), by = person_id]
vaccines_curated[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "one_of_the_vaccines_is_unknown")]
vaccines_curated[, flag := NULL]

# Creation of third_vax_is_janssen
vaccines_curated[removed_row == 0, flag := fifelse(dose_curated == 3 & manufacturer_curated == "JANSSEN", T, F)]
vaccines_curated[removed_row == 0, third_vax_is_janssen := fifelse(any(flag), 1, 0), by = person_id]
vaccines_curated[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "third_vax_is_janssen")]
vaccines_curated[, flag := NULL]



### Create the criteria based on D3_clean_spells merged with vaccines_curated.
### The following criteria are the different for adults and children populations.
# Merging D3_clean_spells and vaccines_curated. Clean and sort the dataset.

#Needed to add allow Cartesian, check if this is what is wanted???
spells_vaccines_adults <- merge(study_spells_adults, vaccines_curated, all.x = T, by = "person_id")
setorderv(spells_vaccines_adults, c("person_id", "date_curated"))

spells_vaccines_children <- merge(study_spells_children, vaccines_curated_children, all.x = T, by = "person_id")
setorderv(spells_vaccines_children, c("person_id", "date_curated"))

# Create important index dates and divide the dataset for the children and adult populations
spells_vaccines_adults[removed_row == 0, second_vax_date := date_curated[2], by = person_id]

spells_vaccines_children[removed_row == 0, tot_vaccination := length(unique(dose_curated)), by = person_id]

spells_vaccines_children[removed_row == 0, study_entry_date := pmax(entry_spell_category + 730, studyStartDate2)]

# Creation of no_two_vaccinations criteria. Only for adult population.
spells_vaccines_adults[removed_row == 0,
                       vax_in_spell := fifelse(entry_spell_category <= date_curated & exit_spell_category >= date_curated, 1, 0), by = person_id]
spells_vaccines_adults[removed_row == 0, tot_vaccination_in_spell := sum(vax_in_spell), by = person_id]
spells_vaccines_adults[removed_row == 0,
                       flag := fifelse(dose_curated %in% c(1, 2) &
                                         !(manufacturer_curated %in% c("PFIZER", "ASTRAZENECA", "MODERNA")), T, F),
                       by = person_id]
spells_vaccines_adults[removed_row == 0, no_two_vaccinations := fifelse(tot_vaccination_in_spell < 2 | any(flag), 1, 0),
                       by = person_id]
spells_vaccines_adults[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "no_two_vaccinations")]
spells_vaccines_adults[, c("vax_in_spell", "flag") := NULL]

# Creation of no_followup_after_two_vaccinations criteria. Only for adult population.
spells_vaccines_adults[removed_row == 0,
                       exit_spell_at_second_vax := fifelse(tot_vaccination_in_spell == 2 & exit_spell_category == max(date_curated), T, F), by = person_id]
spells_vaccines_adults[removed_row == 0, no_followup_after_two_vaccinations := fifelse(any(exit_spell_at_second_vax), 1, 0), by = person_id]
spells_vaccines_adults[removed_row == 0, removed_row := rowSums(.SD), .SDcols=c("removed_row", "no_followup_after_two_vaccinations")]
spells_vaccines_adults[, c("tot_vaccination_in_spell", "exit_spell_at_second_vax") := NULL]

# Creation of spell_including_second_vaccination_too_short criteria for adult population.
spells_vaccines_adults[removed_row == 0, spell_not_include_second_vax := fifelse(
  entry_spell_category > second_vax_date | exit_spell_category < second_vax_date, 1, 0)]
spells_vaccines_adults[removed_row == 0,
                       flag := fifelse(spell_not_include_second_vax == 0 & entry_spell_category > second_vax_date - 730 & starts_at_birth != 0, T, F)]
spells_vaccines_adults[removed_row == 0, removed_row := rowSums(.SD),
                       .SDcols=c("removed_row","spell_not_include_second_vax")]
spells_vaccines_adults[removed_row == 0, spell_including_second_vaccination_too_short := fifelse(any(flag), 1, 0), by = person_id]
spells_vaccines_adults[removed_row == 0, removed_row := rowSums(.SD),
                       .SDcols=c("removed_row","spell_including_second_vaccination_too_short")]
spells_vaccines_adults[, c("flag", "second_vax_date", "spell_not_include_second_vax") := NULL]

# Creation of any_vax_is_janssen criteria for children population.
spells_vaccines_children[removed_row == 0, vax_is_janssen := fifelse(tolower(manufacturer_curated) == "janssen", T, F)]
spells_vaccines_children[removed_row == 0, any_vax_is_janssen := as.integer(any(vax_is_janssen)), by = person_id]
spells_vaccines_children[removed_row == 0, removed_row := rowSums(.SD),
                         .SDcols=c("removed_row", "any_vax_is_janssen")]
spells_vaccines_children[, c("vax_is_janssen") := NULL]


# Creation of spell_including_studyStartDate_date_too_short criteria for children population.
spells_vaccines_children[removed_row == 0, spell_not_include_study_entry := fifelse(
  entry_spell_category > study_entry_date | exit_spell_category < study_entry_date, 1, 0)]
spells_vaccines_children[removed_row == 0,
                         flag := fifelse(spell_not_include_study_entry == 0 & entry_spell_category > study_entry_date - 730 & starts_at_birth != 0, T, F)]
spells_vaccines_children[removed_row == 0, removed_row := rowSums(.SD),
                         .SDcols=c("removed_row","spell_not_include_study_entry")]
spells_vaccines_children[removed_row == 0, spell_including_studyStartDate_date_too_short := fifelse(any(flag), 1, 0), by = person_id]
spells_vaccines_children[removed_row == 0, removed_row := rowSums(.SD),
                         .SDcols=c("removed_row","spell_including_studyStartDate_date_too_short")]
spells_vaccines_children[, c("flag", "tot_vaccination", "study_entry_date", "spell_not_include_study_entry") := NULL]



### Merge the selection criteria created on D3_PERSONS to spells and then vaccines. Clean them
sel_cri_adults <- merge(sel_cri, clean_spells_adults[, "starts_at_birth" := NULL], all.x = T, by = "person_id")
spells_vaccines_adults <- spells_vaccines_adults[, .(person_id, higher_doses_included_but_lower_doses_missing, 
                                                     one_of_the_vaccines_is_unknown,
                                                     no_two_vaccinations, third_vax_is_janssen,
                                                     no_followup_after_two_vaccinations,
                                                     spell_including_second_vaccination_too_short)]
for (i in names(spells_vaccines_adults)) {spells_vaccines_adults[is.na(get(i)), (i):=0]}
spells_vaccines_adults <- spells_vaccines_adults[, lapply(.SD, max), by = person_id]
sel_cri_adults <- merge(sel_cri_adults, spells_vaccines_adults, all.x = T, by = "person_id")

sel_cri_children <- merge(sel_cri, clean_spells_children[, "starts_at_birth" := NULL], all.x = T, by = "person_id")
spells_vaccines_children <- spells_vaccines_children[, .(person_id, higher_doses_included_but_lower_doses_missing, 
                                                         any_vax_is_janssen, 
                                                         spell_including_studyStartDate_date_too_short)]
for (i in names(spells_vaccines_children)) {spells_vaccines_children[is.na(get(i)), (i):=0]}
spells_vaccines_children <- spells_vaccines_children[, lapply(.SD, max), by = person_id]
sel_cri_children <- merge(sel_cri_children, spells_vaccines_children, all.x = T, by = "person_id")
for (i in names(spells_vaccines_children)) {spells_vaccines_children[is.na(get(i)), (i):=0]}

Population_additional_vars_adults <- copy(study_spells_adults)[, study_exit_date := pmin(studyEndDate2, exit_spell_category)]
Population_additional_vars_adults <- Population_additional_vars_adults[, .(person_id, entry_spell_category,
                                                                           study_exit_date, exit_spell_category)]

Population_additional_vars_children <- copy(study_spells_children)[, study_exit_date := pmin(studyEndDate2,
                                                                                             exit_spell_category)]
Population_additional_vars_children <- Population_additional_vars_children[, .(person_id, entry_spell_category,
                                                                               study_exit_date, exit_spell_category)]

sel_cri_adults = sel_cri_adults[, c("person_id", "sex_or_date_birth_is_not_defined", "birth_date_absurd",
                                    "no_details_on_death", "no_spells", "all_spells_start_after_ending",
                                    "no_spell_overlapping_the_study_period", "no_spell_longer_than_730_days",
                                    "too_young", "unvaccinated", "higher_doses_included_but_lower_doses_missing",
                                    "one_of_the_vaccines_is_unknown", "no_two_vaccinations",
                                    "third_vax_is_janssen", "no_followup_after_two_vaccinations",
                                    "spell_including_second_vaccination_too_short")]
sel_cri_children = sel_cri_children[, c("person_id", "sex_or_date_birth_is_not_defined", "birth_date_absurd",
                                        "no_details_on_death", "no_spells", "all_spells_start_after_ending",
                                        "no_spell_overlapping_the_study_period", "no_spell_longer_than_730_days",
                                        "too_young", "too_old", "higher_doses_included_but_lower_doses_missing",
                                        "any_vax_is_janssen", "spell_including_studyStartDate_date_too_short")]

sel_cri_adults <- merge(sel_cri_adults, Population_additional_vars_adults, all.x = T, by = "person_id")
sel_cri_children <- merge(sel_cri_children, Population_additional_vars_children, all.x = T, by = "person_id")

# Saving children and adults populations
saveRDS(sel_cri_adults, file = paste0(d3Dir, "D3_SELECTION_STUDYPOP", ".rds"))
saveRDS(sel_cri_children, file = paste0(d3Dir, "D3_SELECTION_CHILD", ".rds"))


rm(clean_spells, clean_spells_adults, clean_spells_children, sel_cri, sel_cri_adults, sel_cri_children, spells_vaccines_adults, spells_vaccines_children, vaccines_curated, vaccines_curated_children)
gc()


