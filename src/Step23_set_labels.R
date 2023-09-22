label(df$person_id)    <- "Id" 
label(df$pair_id)    <- "Pair Id" 
label(df$exposed_or_comparator)    <- "Exposed or Comparator" 
label(df$sex_at_instance_creation)    <- "Sex" 
label(df$age_at_matching_date)    <- "Age at matching date" 
label(df$ageband_at_matching_date)    <- "Age Band at matching date" 
label(df$monthyear_at_matching_date)    <- "Month Year at matching Date" 
label(df$type_vax1)    <- "1st Dose" 
label(df$type_vax2)    <- "2nd Dose" 
label(df$type_vax3)    <- "3th Dose" 
label(df$type_vax4)    <- "4th Dose" 

label(df$date_last_covid_days)    <- "Days since last covid infection" 
label(df$date_vax1_days)    <- "date_vax1_days" 
label(df$date_vax2_days)    <- "date_vax2_days"
label(df$date_vax3_days)    <- "date_vax3_days" 
label(df$birth_year)    <- "Year of birth" 
label(df$country)    <- "Country" 
label(df$l_georegion_cov_at_matching_date)    <- "GeoRegion" 
label(df$c_hf_aesi_at_matching_date)    <- "hf" 
label(df$e_dm12_cov_at_matching_date)    <- "dm12" 
label(df$g_bladincont_cov_at_matching_date)    <- "bladincont" 
label(df$g_kdchronic_cov_at_matching_date)    <- "kdchronic" 
label(df$i_sepsis_cov_at_matching_date)    <- "sepsis" 
label(df$m_arthr_cov_at_matching_date)    <- "arthr" 
label(df$m_osteoarthritis_cov_at_matching_date)    <- "osteoarthritis" 
label(df$ment_alcabuse_cov_at_matching_date)    <- "alcabuse" 
label(df$n_cerebrovasc_aesi_at_matching_date)    <- "cerebrovasc" 
label(df$n_dementia_cov_at_matching_date)    <- "dementia" 
label(df$n_parkinson_cov_at_matching_date)    <- "parkinson" 
label(df$onc_malignanttumor_ch_at_matching_date)    <- "malignanttumor" 
label(df$v_hypertension_cov_at_matching_date)    <- "hypertension" 
label(df$c_cad_aesi_at_matching_date)    <- "cad_aesi" 
label(df$dp_analgesic_at_matching_date)    <- "analgesic" 
label(df$dp_corticost_at_matching_date)    <- "corticost" 
label(df$dp_nsaid_at_matching_date)    <- "nsaid" 
label(df$dp_psych_at_matching_date)    <- "psych" 
label(df$dp_stat_at_matching_date)    <- "stat" 
label(df$dp_antibio_at_matching_date)    <- "antibio"
label(df$dp_antivir_at_matching_date)    <- "antivir" 
label(df$l_smokestatus_cov_at_matching_date)    <- "smoke" 
label(df$number_drugs_at_matching_date)    <- "number_drugs" 
label(df$num_contacts_yearbefore_at_matching_date)    <- "num_contacts"
label(df$flu_vax_prev_5years_at_matching_date)    <- "vax_prev_5years" 
label(df$reason_for_final_censoring)    <- "reason_for_final_censoring"
label(df$date_final_censoring_days)    <- "Name" 
label(df$first_covid_after_md_days)    <- "Name" 
label(df$first_covid_after_md_date)    <- "Name" 
label(df$first_severecovid_after_md_days)    <- "Name" 
label(df$first_severecovid_after_md_date)    <- "Name" 
label(df$date_start)    <- "Name" 
label(df$matched)    <- "Matched" 
label(df$entry_spell_category_crude)    <- "Name" 
label(df$study_exit_date)    <- "Name" 
label(df$date_birth)    <- "Name" 
label(df$date_death)    <- "Name" 
label(df$date_vax1)    <- "Name" 
label(df$date_vax2)    <- "Name" 
label(df$date_vax3)    <- "Name" 
label(df$type_vax4)    <- "Name" 
label(df$date_vax4)    <- "Name" 
label(df$end_no_vaccine)    <- "Name" 
label(df$end_no_booster)    <- "Name" 
label(df$end_no_4dose)    <- "Name" 
#label(df$date_tentative_censoring)    <- "Name" 
label(df$reason_for_tentative_censoring)    <- "Name"
#label(df$date_tentative_censoring_days)    <- "Name"
#label(df$date_final_censoring)    <- "Name" 
label(df$covid_between_vax2_and_matching_date)    <- "Name" 
label(df$first_covid_sev1)    <- "Name" 
label(df$first_o_deathany_aesi)    <- "Name" 
label(df$h_hosppop_pop_at_matching_date)    <- "Hosppop" 
label(df$dp_immunosuppr_at_matching_date)    <- "Name" 
label(df$im_immunodef_cov_at_matching_date)    <- "Name" 
label(df$onc_anymalignancy_cov_at_matching_date)    <- "Name" 
label(df$any_covid_at_matching_date)    <- "Name"
label(df$r_ards_aesi_at_matching_date)    <- "Name" 
label(df$dp_proxy_at_matching_date)    <- "Name"
label(df$comorbidity_at_matching_date)    <- "Name" 
label(df$bmi_at_matching_date)    <- "Name" 
#label(df$tp_covid19test_cov_at_matching_date)    <- "Name" 
label(df$date_final_censoring_days)    <- "Final censoring days" 

units(df$date_last_covid_days)       <- "days"


df$immunodeficiency_at_matching_date <- as.logical(df$immunodeficiency_at_matching_date)
label(df$immunodeficiency_at_matching_date) <- "Immunodeficiency at matching date"
df$cancer_at_matching_date <- as.logical(df$cancer_at_matching_date)
label(df$cancer_at_matching_date) <- "Cancer at matching date"
df$im_transplantrecipient_cov_at_matching_date <- as.logical(df$im_transplantrecipient_cov_at_matching_date)
label(df$im_transplantrecipient_cov_at_matching_date) <- "Transplant at matching date"
df$g_severerenaldisease_ch_at_matching_date <- as.logical(df$g_severerenaldisease_ch_at_matching_date)
label(df$g_severerenaldisease_ch_at_matching_date) <- "Renal Disease at matching date"
df$o_down_cov_at_matching_date <- as.logical(df$o_down_cov_at_matching_date)
label(df$o_down_cov_at_matching_date) <- "Down at matching date"

df$dp_analgesic_at_matching_date <- as.logical(df$dp_analgesic_at_matching_date)
label(df$dp_analgesic_at_matching_date) <- "Analgesic at matching date"
df$dp_corticost_at_matching_date <- as.logical(df$dp_corticost_at_matching_date)
label(df$dp_corticost_at_matching_date) <- "Corticost at matching date"
df$dp_nsaid_at_matching_date <- as.logical(df$dp_nsaid_at_matching_date)
label(df$dp_nsaid_at_matching_date) <- "Nsaid at matching date"
df$dp_psych_at_matching_date <- as.logical(df$dp_psych_at_matching_date)
label(df$dp_psych_at_matching_date) <- "Psych at matching date"
df$dp_stat_at_matching_date <- as.logical(df$dp_stat_at_matching_date)
label(df$dp_stat_at_matching_date) <- "Stat at matching date"
df$dp_antibio_at_matching_date <- as.logical(df$dp_antibio_at_matching_date)
label(df$dp_antibio_at_matching_date) <- "Antibio at matching date"
df$dp_antivir_at_matching_date <- as.logical(df$dp_antivir_at_matching_date)
label(df$dp_antivir_at_matching_date) <- "Antivir at matching date"

df$dp_immunosuppr_at_matching_date <- as.logical(df$dp_immunosuppr_at_matching_date)
label(df$dp_immunosuppr_at_matching_date) <- "Immunosuppr at matching date"

df$h_hosppop_pop_at_matching_date <- as.logical(df$h_hosppop_pop_at_matching_date)
label(df$h_hosppop_pop_at_matching_date) <- "Hospop at matching date"
df$im_immunodef_cov_at_matching_date <- as.logical(df$im_immunodef_cov_at_matching_date)
label(df$im_immunodef_cov_at_matching_date) <- "Immunodef at matching date "
df$onc_anymalignancy_cov_at_matching_date <- as.logical(df$onc_anymalignancy_cov_at_matching_date)
label(df$onc_anymalignancy_cov_at_matching_date) <- "Anymalignancy at matching date "
df$any_covid_at_matching_date <- as.logical(df$any_covid_at_matching_date)
label(df$any_covid_at_matching_date) <- "Covid at matching date "
df$dp_immunosuppr_at_matching_date <- as.logical(df$dp_immunosuppr_at_matching_date)
label(df$dp_immunosuppr_at_matching_date) <- "Immunosuppr at matching date "
df$r_ards_aesi_at_matching_date <- as.logical(df$r_ards_aesi_at_matching_date)
label(df$r_ards_aesi_at_matching_date) <- "Ards at matching date "


df$m_arthr_cov_at_matching_date <- as.logical(df$m_arthr_cov_at_matching_date)
label(df$m_arthr_cov_at_matching_date) <- "arthr at matching date "
df$m_osteoarthritis_cov_at_matching_date <- as.logical(df$m_osteoarthritis_cov_at_matching_date)
label(df$m_osteoarthritis_cov_at_matching_date) <- "osteoarthritis at matching date "
df$ment_alcabuse_cov_at_matching_date <- as.logical(df$ment_alcabuse_cov_at_matching_date)
label(df$ment_alcabuse_cov_at_matching_date) <- "alcabuse at matching date "
df$c_hf_aesi_at_matching_date <- as.logical(df$c_hf_aesi_at_matching_date)
label(df$c_hf_aesi_at_matching_date) <- "hf at matching date "
df$e_dm12_cov_at_matching_date <- as.logical(df$e_dm12_cov_at_matching_date)
label(df$e_dm12_cov_at_matching_date) <- "dm12 at matching date "
df$g_bladincont_cov_at_matching_date <- as.logical(df$g_bladincont_cov_at_matching_date)
label(df$g_bladincont_cov_at_matching_date) <- "bladincont at matching date "
df$g_kdchronic_cov_at_matching_date <- as.logical(df$g_kdchronic_cov_at_matching_date)
label(df$g_kdchronic_cov_at_matching_date) <- "kd at matching date "
df$i_sepsis_cov_at_matching_date <- as.logical(df$i_sepsis_cov_at_matching_date)
label(df$i_sepsis_cov_at_matching_date) <- "sepsis at matching date "

df$n_cerebrovasc_aesi_at_matching_date <- as.logical(df$n_cerebrovasc_aesi_at_matching_date)
label(df$n_cerebrovasc_aesi_at_matching_date) <- "Cerebrovasc at matching date "

df$n_dementia_cov_at_matching_date <- as.logical(df$n_dementia_cov_at_matching_date)
label(df$n_dementia_cov_at_matching_date) <- "Dementia at matching date "

df$n_parkinson_cov_at_matching_date <- as.logical(df$n_parkinson_cov_at_matching_date)
label(df$n_parkinson_cov_at_matching_date) <- "Parkinson at matching date "

df$onc_malignanttumor_ch_at_matching_date <- as.logical(df$onc_malignanttumor_ch_at_matching_date)
label(df$onc_malignanttumor_ch_at_matching_date) <- "Cancer at matching date "

df$v_hypertension_cov_at_matching_date <- as.logical(df$v_hypertension_cov_at_matching_date)
label(df$v_hypertension_cov_at_matching_date) <- "Hypertension at matching date "

df$c_cad_aesi_at_matching_date <- as.logical(df$c_cad_aesi_at_matching_date)
label(df$c_cad_aesi_at_matching_date) <- "Cad at matching date "



#other variables not printed in the table
#+ first_covid_after_md_date
#+ first_severecovid_after_md_days
#+ first_severecovid_after_md_date
#+ date_start
# + entry_spell_category_crude+ study_exit_date+ date_birth+ date_death
#+ date_vax1+ date_vax2
# + date_vax3
# + date_vax4
#+ end_no_vaccine+ end_no_booster+ end_no_4dose
#+ date_tentative_censoring
#+ reason_for_tentative_censoring
#+ date_tentative_censoring_days
#+ date_final_censoring
# + covid_between_vax2_and_matching_date
#+ first_covid_sev1
#+ first_o_deathany_aesi
#+ reason_for_final_censoring+ date_final_censoring_days
#+ first_covid_after_md_days
#+ date_last_covid_days+ date_vax1_days+ date_vax2_days+ date_vax3_days
#+ dp_proxy_at_matching_date+ comorbidity_at_matching_date+ bmi_at_matching_date+ tp_covid19test_cov_at_matching_date


