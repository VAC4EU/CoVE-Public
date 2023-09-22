# OBSERVATION PER IODS -----------------------------------------------------
#COMPUTE SPELLS

# input: OBSERVATION_PERIODS
# output: D3_output_spells_category


print("COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS")

# import input datasets

#studyEndDate <- min(as.Date(as.character(CDM_SOURCE[1,"date_creation"]), date_format),
#                 as.Date(as.character(CDM_SOURCE[1,"recommended_end_date"]), date_format), na.rm = T)


days <- 1

OBSERVATION_PERIODS <- readRDS(paste0(d3Dir,"D3_OBSERVATION_PERIODS.rds"))

  OBSERVATION_PERIODS <- OBSERVATION_PERIODS[,op_meaning:="all"]
  output_spells_category <- CreateSpells(
    dataset = OBSERVATION_PERIODS,
    id = "person_id" ,
    start_date = "op_start_date",
    end_date = "op_end_date",
    category ="op_meaning",
    replace_missing_end_date = studyEndDate,
    gap_allowed = days
  )
  
  output_spells_category <- as.data.table(output_spells_category)
  setkeyv(
    output_spells_category,
    c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning")
  )
  
  saveRDS(output_spells_category,file = paste0(d3Dir,"D3_OUTPUT_SPELLS_CATEGORY.rds"))


#To do clarify
###
#empty_spells <- OBSERVATION_PERIODS[1,.(person_id)]
#empty_spells <- empty_spells[,op_meaning := "test"]
#empty_spells <- empty_spells[,entry_spell_category := as.Date('20010101',"Y%d%m")]
#empty_spells <- empty_spells[,exit_spell_category := as.Date('20010101',"Y%d%m")]
#empty_spells <- empty_spells[,num_spell := 1]
#empty_spells <- empty_spells[op_meaning!="test",]
###

rm(OBSERVATION_PERIODS)
