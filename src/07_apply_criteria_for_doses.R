# input: D3_clean_vaccines
# output: Flowchart_QC_criteria, D3_vaccines_curated
clean_vaccines <- readRDS(paste0(d3Dir, "D3_CLEAN_VACCINES.rds"))
# Crate the flowchart and filter the record of the doses
vaccines_curated <- CreateFlowChart(
  dataset = clean_vaccines,
  listcriteria = c("duplicated_records", "manufacturer_not_in_study", "missing_date", "date_before_start_vax",
                   "distance_btw_1_2_doses", "distance_btw_2_3_doses", "distance_btw_3_4_doses", "dose_after_4"),
  flowchartname = "Flowchart_criteria_for_doses")

# Save the flowchart
fwrite(Flowchart_criteria_for_doses, paste0(exportDir, "/Flowchart_criteria_for_doses.csv"))

# Clean and save the final dataset for thye doses
vaccines_curated <- vaccines_curated[, .(person_id, date_curated, dose_curated, manufacturer_curated)]
saveRDS(vaccines_curated, file = paste0(d3Dir, "D3_VACCINES_CURATED.rds"))
rm(vaccines_curated, clean_vaccines, Flowchart_criteria_for_doses)
