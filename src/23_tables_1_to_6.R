#--------------------------------------------------------------
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

inputD3_matched_pop <- paste0(projectDir,"/data/D3/D3_matched_population_main_study.rds")

inputD3_studypop <- paste0(projectDir,"/data/D3/D3_study_population_with_matching_variables.rds")

diroutput <- paste0(projectDir,"/output/export/")

### Create to show how many repetead pairs for each combination of T0_CONTROL, Control and matched_cohort we have
inputD3_matched <- readRDS(paste0(d3Dir, "MATCHED_PAIRS.rds"))
inputD3_matched <- inputD3_matched[!is.na(Exposed) & !is.na(Control), ]
keys <- c("Control", "T0_CONTROL", "matched_cohort")
setkeyv(inputD3_matched, keys)
df_control_matched_cohort <- copy(inputD3_matched)[, n_exposed_each_control := .N, by = keys]
df_control_matched_cohort <- df_control_matched_cohort[n_exposed_each_control != 1, ]

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
# FROM HERE: TO BE USED AS IS (after revising the points marked 'To CHECK')

if (!require("gtsummary")) install.packages("gtsummary")
library(gtsummary)

if (!require("labelled")) install.packages("labelled")
library(labelled)

#--------------------------------------------------------------
# from D3_matched_population_main_study_variables (all tables except 1.1)
# Filter information from D3_matched_population_main_study_variables
D3_matched_population_main_study_variables <- readRDS(inputD3_matched_pop)

# add variables that will be used in the table

input_matched <- D3_matched_population_main_study_variables[,isF := fifelse(sex_at_instance_creation == "F", 1, 0)][,ageband_at_matching_date := fifelse(ageband_at_matching_date == "5-11","05-11", ageband_at_matching_date)][,group:= fifelse(age_at_matching_date >= 18,"adults",fifelse(age_at_matching_date >= 12,"adolescents","children"))]

# exposure_and_unmatched_sorted 1 = exposed matched, 2 = comparator 3 = exposed unmatched
input_matched <- input_matched[,exposure_sorted := fifelse(exposed_or_comparator == "exposed",1,2) ][,exposure_and_unmatched_sorted := fifelse(matched == F,3,exposure_sorted)]

input_matched <- input_matched %>% set_value_labels(exposure_and_unmatched_sorted = c( "Homologous matched" = 1, "Heterologous matched" = 2, "Homologous unmatched" = 3))

var_label(input_matched) <- list(exposure_and_unmatched_sorted = "Matched and unmatched exposed, and controls (controls are repeated)")

# type_HO_P is type_vax1 only in homologous (missing in heterologous)
input_matched <- as.data.table(input_matched)[type_vax1 == type_vax2 & !is.na(type_vax1), type_HO_P := type_vax1]

# type_HE_P is type_vax1 only in heterologous (missing in homologous)
input_matched <- input_matched[type_vax1 != type_vax2 & !is.na(type_vax1), type_HE_P := type_vax1]

# homologous_P is like HO_P, just adding is since i am not sure it was included in the D3
input_matched <- input_matched[!is.na(type_vax1), homologous_P := fifelse( type_vax1 == type_vax2,1,0)]

# homologous_P_matched: 1 = unmatched, homologous, 2 = matched, homologous, 3 = unmatched, heterologous,4 = matched, heterologous
input_matched <- input_matched[!is.na(homologous_P), homologous_P_matched := fifelse( homologous_P == 1,matched + 1,matched + 3)]

check_columns_do_not_exist <- function(start_df, columns) {
  columns[!grepl(paste(colnames(start_df), collapse = "|"), columns)]
}

variables_not_in_df <- check_columns_do_not_exist(input_matched, c("number_drugs_at_matching_date",
                                                                   "flu_vax_prev_5years_at_matching_date"))
if (length(variables_not_in_df) != 0) {
  input_matched[, (variables_not_in_df) := NA]
}

input_matched <- input_matched[,more_than_5_drugs := fifelse( number_drugs_at_matching_date > 5 & !is.na(number_drugs_at_matching_date),1,0)]
input_matched <- input_matched[,any_flu_vax_prev5years := fifelse( flu_vax_prev_5years_at_matching_date >= 1 & !is.na(flu_vax_prev_5years_at_matching_date),1,0)]
input_matched <- input_matched %>% set_value_labels(homologous_P_matched = c( "Homologous unmatched" = 1, "Homologous matched" = 2, "Heterologous unmatched" = 3, "Heterologous matched" = 4))

var_label(input_matched) <- list(homologous_P_matched = "Matched and unmatched exposed, and controls (controls are repeated)")
input_matched <- as.data.table(input_matched)[matched_cohort == "children" , children_HO_HE_and_controls := fifelse(exposed_or_comparator == "exposed",fifelse( homologous_P == 1,matched + 1,matched + 3),5)]
input_matched <- input_matched %>% set_value_labels(children_HO_HE_and_controls = c( "Homologous unmatched" = 1, "Homologous matched" = 2, "Heterologous unmatched" = 3, "Heterologous matched" = 4, "Unvaccinated matched" = 5))

var_label(input_matched) <- list(children_HO_HE_and_controls = "Children exposed to heterologous or homologous vaccination, and unvaccinated controls (controls may be repeated)")

input_matched <- as.data.table(input_matched)

if (tolower(thisDataSource) %in% tolower(c("CPRD", "PEDIANET", "TEST", "PHARMO", "CASERTA", "BIFAP", "SIDIAP"))) {
  input_matched <- input_matched[, any_covid_at_matching_date := fifelse(any_covid_at_matching_date == 0, 1, 0)]
}

rm(D3_matched_population_main_study_variables)

#--------------------------------------------------------------
# from D3_study_population_with_matching_variables (only table 1.1)
# add variables that will be used in the table

D3_study_population_with_matching_variables <- readRDS(inputD3_studypop)

input_studypop <- D3_study_population_with_matching_variables[,isF := fifelse(sex_at_instance_creation == "F", 1, 0)][,ageband_at_time0 := fifelse(ageband_at_time0 == "5-11","05-11", ageband_at_time0)][,group:= fifelse(age_at_time0 >= 18,"adults",fifelse(age_at_time0 >= 12,"adolescents","children"))]

input_studypop <- input_studypop[type_vax1 == type_vax2 & !is.na(type_vax1), type_HO_P := type_vax1]

input_studypop <- input_studypop[type_vax1 != type_vax2 & !is.na(type_vax1), type_HE_P := type_vax1]

input_studypop <- input_studypop[!is.na(type_vax1), homologous_P := fifelse( type_vax1 == type_vax2,1,0)]


rm(D3_study_population_with_matching_variables)

#--------------------------------------------------------------
# ASSIGN TABLE-INDEPENDENT PARAMETERS
#--------------------------------------------------------------

categorical_TI_matching_var <- tolower(c("monthyear_at_matching_date","L_GEOREGION_COV_at_matching_date"))

dichotomous_TI_matching_var <- tolower(c("any_covid_at_matching_date","immunodeficiency_at_matching_date","cancer_at_matching_date","Im_TRANSPLANTRECIPIENT_COV_at_matching_date","G_SEVERERENALDISEASE_CH_at_matching_date","O_DOWN_COV_at_matching_date"))

categorical_TI_matching_var_time0 <- tolower(c("L_GEOREGION_COV_at_time0"))

dichotomous_TI_matching_var_time0 <- tolower(c("immunodeficiency_at_time0","cancer_at_time0","Im_TRANSPLANTRECIPIENT_COV_at_time0","G_SEVERERENALDISEASE_CH_at_time0","O_DOWN_COV_at_time0", "free_covid_at_time0"))

# dichotomous_comorb_and_comed <- c("B_COAGDIS_AESI_at_matching_date", "C_HF_AESI_at_matching_date", "E_DM12_COV_at_matching_date", "E_DM12ALGORITHM_COV_at_matching_date", "G_BLADINCONT_COV_at_matching_date", "G_KDCHRONIC_COV_at_matching_date", "I_SEPSIS_COV_at_matching_date", "Im_AUTOIMM_COV_at_matching_date", "M_ARTHR_COV_at_matching_date", "M_OSTEOARTHRITIS_COV_at_matching_date", "Ment_ALCABUSE_COV_at_matching_date", "N_CEREBROVASC_AESI_at_matching_date", "N_DEMENTIA_COV_at_matching_date", "N_PARKINSON_COV_at_matching_date", "Onc_CANCER_CH_at_matching_date", "Onc_MALIGNANTTUMOR_CH_at_matching_date", "R_RESPCHRONICALGORITHM_COV_at_matching_date", "V_HYPERTENSION_COV_at_matching_date", "C_CAD_AESI_at_matching_date", "D_LIVERCHRONIC_COV_at_matching_date", "DP_ANALGESIC_at_matching_date", " DP_CORTICOST_at_matching_date", "DP_NSAID_at_matching_date", "DP_PSYCH_at_matching_date", "DP_STAT_at_matching_date", "DP_VACCINES_at_matching_date", "DP_ANTIBIO_at_matching_date", "DP_ANTIVIR_at_matching_date")

dichotomous_comorb <- tolower(c("B_COAGDIS_AESI_at_matching_date", "C_HF_AESI_at_matching_date", "E_DM12_COV_at_matching_date", "E_DM12ALGORITHM_COV_at_matching_date", "G_BLADINCONT_COV_at_matching_date", "G_KDCHRONIC_COV_at_matching_date", "I_SEPSIS_COV_at_matching_date", "Im_AUTOIMM_COV_at_matching_date", "M_ARTHR_COV_at_matching_date", "M_OSTEOARTHRITIS_COV_at_matching_date", "Ment_ALCABUSE_COV_at_matching_date", "N_CEREBROVASC_AESI_at_matching_date", "N_DEMENTIA_COV_at_matching_date", "N_PARKINSON_COV_at_matching_date", "Onc_CANCER_CH_at_matching_date", "Onc_MALIGNANTTUMOR_CH_at_matching_date", "R_RESPCHRONICALGORITHM_COV_at_matching_date", "V_HYPERTENSION_COV_at_matching_date", "C_CAD_AESI_at_matching_date", "D_LIVERCHRONIC_COV_at_matching_date"))

dichotomous_comed <- tolower(c("DP_ANALGESIC_at_matching_date", "DP_CORTICOST_at_matching_date", "DP_NSAID_at_matching_date", "DP_PSYCH_at_matching_date", "DP_STAT_at_matching_date", "DP_VACCINES_at_matching_date", "DP_ANTIBIO_at_matching_date", "DP_ANTIVIR_at_matching_date","more_than_5_drugs"))

lifestyle_and_other_factors <- tolower(c("H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date"))

# des_healthcare_utilization <- tolower(c("H_PRIMARYHCUSE_COV_at_matching_date",  "aggregated_number_of_contacts","any_flu_vax_prev5years","any_other_vax_prev5years", "tp_covid19test_cov_at_matching_date"))
des_healthcare_utilization <- tolower(c("H_PRIMARYHCUSE_COV_at_matching_date",  "aggregated_number_of_contacts","any_flu_vax_prev5years", "tp_covid19test_cov_at_matching_date"))

#--------------------------------------------------------------
# ASSIGN TABLE-DEPENDENT PARAMETERS
#--------------------------------------------------------------
parameter_inputtable <- vector(mode="list")
parameter_select <- vector(mode="list")
parameter_include <- vector(mode="list")
parameter_by <- vector(mode="list")
parameter_statistic <- vector(mode="list")
parameter_type <- vector(mode="list")

# table 1.1

parameter_inputtable[['1.1']] <- 'input_studypop'
parameter_select[['1.1']] <- '!is.na(person_id) & !(ageband_at_time0 %in% c("0-4", "05-11")) & !is.na(homologous_P)'
parameter_include[['1.1']] <- c("type_HO_P","type_HE_P","isF","age_at_time0","ageband_at_time0",categorical_TI_matching_var_time0,dichotomous_TI_matching_var_time0)
parameter_by[['1.1']] <- "homologous_P"
parameter_statistic[['1.1']] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[['1.1']] <- list(c(age_at_time0) ~ "continuous2")

# table 1.2
parameter_inputtable[['1.2']] <- 'input_matched'
parameter_select[['1.2']] <- 'matched_cohort == "children" & exposed_or_comparator == "exposed"'
parameter_include[['1.2']] <- c("type_HO_P","type_HE_P","date_final_censoring_days","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var)
parameter_by[['1.2']] <- "homologous_P"
parameter_statistic[['1.2']] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[['1.2']] <- list(c(age_at_matching_date) ~ "continuous2")

needed_colls <- setdiff(parameter_include[['1.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 2.1

tablenum <- '2.1'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_TO_HE_P" & (exposed_or_comparator == "exposed" | matched == T) & ageband_at_matching_date != "05-11"'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var)
parameter_by[[tablenum]] <- "exposure_and_unmatched_sorted"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days) ~ "continuous2")

needed_colls <- setdiff(parameter_include[['2.1']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 2.2

tablenum <- '2.2'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "children" & exposed_or_comparator == "exposed"' 
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var)
parameter_by[[tablenum]] <- "homologous_P_matched"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(date_final_censoring_days) ~ "continuous2", age_at_matching_date ~ "categorical" )

needed_colls <- setdiff(parameter_include[['2.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 3.1

tablenum <- '3.1'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_TO_HE_P" & matched == T'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P")
parameter_by[[tablenum]] <- "monthyear_at_matching_date"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- NULL

needed_colls <- setdiff(parameter_include[['3.1']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 3.2

tablenum <- '3.2'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "children" & matched == T'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P")
parameter_by[[tablenum]] <- "monthyear_at_matching_date"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- NULL

needed_colls <- setdiff(parameter_include[['3.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 4.1

tablenum <- '4.1'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_HO_B_to_NB" & matched == T & exposed_or_comparator == "exposed"'
parameter_include[[tablenum]] <- c("type_vax3")
parameter_by[[tablenum]] <- "monthyear_at_matching_date"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- NULL

needed_colls <- setdiff(parameter_include[['4.1']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 4.2

tablenum <- '4.2'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HE_P_B_to_NB" & matched == T & exposed_or_comparator == "exposed"'
parameter_include[[tablenum]] <- c("type_vax3")
parameter_by[[tablenum]] <- "monthyear_at_matching_date"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- NULL

needed_colls <- setdiff(parameter_include[['4.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}



# table 5.1

tablenum <- '5.1'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_TO_HE_P" & matched == T & group == "adults" & any_covid_at_matching_date == 1'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['5.1']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 5.2

tablenum <- '5.2'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_TO_HE_P" & matched == T & group == "adolescents" & any_covid_at_matching_date == 1'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['5.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

# table 5.3

tablenum <- '5.3'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "children" & matched == T & any_covid_at_matching_date == 1'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "children_HO_HE_and_controls"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['5.3']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}
tablenum <- '6.1'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_HO_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & !(ageband_at_matching_date %in% c("05-11", "12-17"))'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.1']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

tablenum <- '6.2'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HE_P_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & !(ageband_at_matching_date %in% c("05-11", "12-17"))'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","type_vax3","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.2']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

tablenum <- '6.3'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_HE_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & !(ageband_at_matching_date %in% c("05-11", "12-17"))'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","type_vax3","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.3']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

tablenum <- '6.4'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_HO_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & ageband_at_matching_date == "12-17"'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.4']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

tablenum <- '6.5'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HE_P_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & ageband_at_matching_date == "12-17"'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","type_vax3","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.5']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

tablenum <- '6.6'

parameter_inputtable[[tablenum]] <- 'input_matched'
parameter_select[[tablenum]] <- 'matched_cohort == "HO_P_HE_B_to_NB" & matched == T  & any_covid_at_matching_date == 1 & ageband_at_matching_date == "12-17"'
parameter_include[[tablenum]] <- c("type_HO_P","type_HE_P","type_vax3","date_final_censoring_days","reason_for_final_censoring","isF","age_at_matching_date","ageband_at_matching_date",categorical_TI_matching_var,dichotomous_TI_matching_var,dichotomous_comorb,dichotomous_comed,"H_HCW_COV_at_matching_date", "H_NURSING_COV_at_matching_date", "H_RLTCF_COV_at_matching_date", "H_PRIMARYHCUSE_COV_at_matching_date", "L_BMI_COV_at_matching_date", "L_SMOKESTATUS_COV_at_matching_date", "number_drugs_at_matching_date", "num_contacts_yearbefore_at_matching_date", "flu_vax_prev_5years_at_matching_date", "tp_covid19test_cov_at_matching_date", "pair_w_prior_test")
parameter_by[[tablenum]] <- "exposed_or_comparator"
parameter_statistic[[tablenum]] <- list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)",all_continuous2() ~ c("{mean} ({sd})","{min}","{p25}","{median}","{p75}","{max}") )
parameter_type[[tablenum]] <- list(c(age_at_matching_date,date_final_censoring_days,L_BMI_COV_at_matching_date,tp_covid19test_cov_at_matching_date) ~ "continuous2", c(number_drugs_at_matching_date, num_contacts_yearbefore_at_matching_date, flu_vax_prev_5years_at_matching_date) ~ "categorical")

needed_colls <- setdiff(parameter_include[['6.6']], colnames(input_matched))
if (length(needed_colls) != 0) {input_matched[, (needed_colls) := NA]}

#--------------------------------------------------------------
# GENERATE THE TABLES
#--------------------------------------------------------------
all_tables <- c('1.1', '1.2', '2.1', '2.2', '3.1', '3.2', '4.1', '4.2', '5.1', '5.2', '5.3',
                '6.1', '6.2', '6.3', '6.4', '6.5', '6.6')

for (tablenum in all_tables){
  input_table <- get(parameter_inputtable[[tablenum]])[, (parameter_by[[tablenum]]) := factor(get(parameter_by[[tablenum]]))]
  input_table <- input_table[eval(parse(text = parameter_select[[tablenum]])),]
  if (nrow(input_table) > 0) {
    print(tablenum)
    output <- suppressWarnings(tbl_summary(
      data = input_table,
      by = parameter_by[[tablenum]],
      label = NULL,
      statistic = parameter_statistic[[tablenum]],
      digits = list(all_continuous() ~ c(1, 1)),
      type = parameter_type[[tablenum]],
      value = NULL,
      missing = NULL,
      missing_text = NULL,
      sort = list(everything() ~ "alphanumeric"),
      percent = "column",
      include = parameter_include[[tablenum]]
    )
    )
    
    output$inputs <- NULL
    gtsave(output %>% as_gt(), paste0('table_', tablenum, ".html"), path = diroutput)
    
    nametable <- paste0('table_',tablenum)
    assign(nametable, output)
    
    
    saveRDS(get(nametable), file = paste0(diroutput ,nametable, ".rds"))
  } else {
    print(paste("No matches for table:", tablenum))
  }
}