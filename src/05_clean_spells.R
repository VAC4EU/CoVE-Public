# input: output_spells_category
# output: clean_spells

PERSONS <- readRDS(paste0(d3Dir,"D3_PERSONS.rds"))

person_spell <- merge(readRDS(paste0(d3Dir,"D3_OUTPUT_SPELLS_CATEGORY.rds")), PERSONS, all.x = T, by = "person_id")
person_spell <- person_spell[, .(person_id, date_birth, date_death, entry_spell_category_crude = entry_spell_category,
                                 exit_spell_category_crude = exit_spell_category, num_spell)]
person_spell[, entry_spell_category := data.table::fifelse(date_birth < entry_spell_category_crude - 60,
                                                           entry_spell_category_crude,
                                                           date_birth)]
person_spell[, exit_spell_category := pmin(exit_spell_category_crude, date_death, na.rm = T)]
person_spell[, op_start_date_cleaned := data.table::fifelse(entry_spell_category != entry_spell_category_crude, 0, 1)]
person_spell[, op_end_date_cleaned := data.table::fifelse(exit_spell_category <= exit_spell_category_crude, 0, 1)]
person_spell[, starts_at_birth := data.table::fifelse(entry_spell_category == date_birth, 1, 0)]
person_spell[, starts_after_ending := data.table::fifelse(entry_spell_category <= exit_spell_category, 0, 1)]

saveRDS(person_spell, file = paste0(d3Dir, "D3_CLEAN_SPELLS.rds"))
rm(person_spell)
