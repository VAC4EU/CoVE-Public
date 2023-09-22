#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# input: D3_selection_criteria_from_PERSONS_to_study_population, D3_selection_criteria_from_PERSONS_to_children_study_population
# output: Flowchart_exclusion_criteria_children, Flowchart_exclusion_criteria, D4_study_population, D4_children_study_population

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION


  # Create flowchart for adults and save D4_study_population
selection_criteria <-readRDS(paste0(d3Dir,"D3_SELECTION_STUDYPOP.rds"))
  
selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("sex_or_date_birth_is_not_defined", "birth_date_absurd", "no_details_on_death", "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period",
                   "no_spell_longer_than_730_days", "too_young", "unvaccinated",
                   "higher_doses_included_but_lower_doses_missing", "one_of_the_vaccines_is_unknown",
                   "no_two_vaccinations", "third_vax_is_janssen", "no_followup_after_two_vaccinations",
                   "spell_including_second_vaccination_too_short"),
  flowchartname = paste0("Flowchart_exclusion_criteria"))

fwrite(get(paste0("Flowchart_exclusion_criteria")),
       paste0(exportDir, "/Flowchart_exclusion_criteria.csv"))

saveRDS(selected_population[, .(person_id, entry_spell_category, study_exit_date, exit_spell_category)],
        paste0(d4Dir, "D4_STUDY_POPULATION.rds"))
rm(selected_population)
  
# Create flowchart for children and save D4_children_study_population

selection_criteria <-readRDS(paste0(d3Dir,"D3_SELECTION_CHILD.rds"))

selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("person_id", "sex_or_date_birth_is_not_defined", "birth_date_absurd", "no_details_on_death",
                   "no_spells", "all_spells_start_after_ending", "no_spell_overlapping_the_study_period",
                   "no_spell_longer_than_730_days", "too_young", "too_old",
                   "higher_doses_included_but_lower_doses_missing", "any_vax_is_janssen",
                   "spell_including_studyStartDate_date_too_short"),
  flowchartname = paste0("Flowchart_exclusion_criteria_children"))


fwrite(get(paste0("Flowchart_exclusion_criteria_children")),
       paste0(exportDir, "/Flowchart_exclusion_criteria_children.csv"))

saveRDS(selected_population[, .(person_id, entry_spell_category, study_exit_date, exit_spell_category)],
        paste0(d4Dir, "D4_STUDY_POPULATION_CHILD.rds"))
rm(selected_population)
