

scheme <- readRDS(paste0(d3Dir, "matching_quries.rds"))

mydb <- dbConnect(RSQLite::SQLite(), db = dbMatchingFile)
mydb2 <- dbConnect(RSQLite::SQLite(), db = dbConceptsFile)


result <- lapply(1:nrow(scheme), function(i){
  
  tmp <- as.data.table(dbGetQuery(mydb, paste0(" SELECT sum(",scheme[i,][["expGroup"]],") AS INCLUDED_SPELLS FROM CASES WHERE ",scheme[i,][["expT0"]]," BETWEEN ST3 AND EN3 ") ))
  tmp[, group := scheme[i,][["expGroup"]]]
  tmp[, cohort := scheme[i,][["pop"]]]
  tmp[, TOTAL_SPELLS := as.numeric(dbGetQuery(mydb, paste0(" SELECT sum(",scheme[i,][["expGroup"]],")  FROM CASES  "))) ]
  tmp[, SUBJECTS := as.numeric(dbGetQuery(mydb, paste0(" SELECT COUNT(DISTINCT person_id) FROM CASES WHERE ",scheme[i,][["expGroup"]], " = 1") )) ]
}
)

result <- do.call(rbind, result)

PAIRS <- readRDS(paste0(d3Dir, "MATCHED_PAIRS.rds"))
D3_output <- readRDS(paste0(d3Dir, "D3_matched_population_main_study.rds"))
persons1 <- length(unique(readRDS(paste0(d3Dir, "persons_of_interest.rds"))))
persons2 <- as.numeric(dbGetQuery(mydb, "SELECT DISTINCT count(DISTINCT person_id) from CASES"))
if(persons1 != persons2) warning("Subjects inequal ")

result <- merge(x = result, y = PAIRS[, .(PAIRS = sum(!is.na(Control))), by = matched_cohort], by.y = "matched_cohort", by.x = "cohort", all = T)
result <- merge(x = result, y = D3_output[, .(D3 = .N), by = matched_cohort], by.y = "matched_cohort", by.x = "cohort", all = T)

colorder <- c("cohort", "SUBJECTS", "TOTAL_SPELLS", "INCLUDED_SPELLS", "PAIRS", "D3")
setcolorder(result, colorder)[, group := NULL]
rm(colorder)

result <- result[, check1 := fifelse(D3 == (PAIRS + INCLUDED_SPELLS), T, F)]

matchCrit <- c("L_GEOREGION_COV", 
               "IM_TRANSPLANTRECIPIENT_COV", 
               "G_SEVERERENALDISEASE_CH" ,
               "O_DOWN_COV" , 
               "AGEBAND", 
               "IMMUNODEFICIENCY", 
               "CANCER", 
               "FREE_COVID", 
               "sex_at_instance_creation"
) 




COMB <- PAIRS[,c(matchCrit, "matched_cohort", "nb_match", "Exposed"), with = F]

COMB <- merge(
              x = COMB,
              y = as.data.table(dbGetQuery(mydb2, "SELECT DISTINCT person_id, type_vax1, type_vax2, date_vax1, date_vax2, date_vax3 FROM POPULATIONS")),
              by.x = "Exposed",
              by.y = "person_id",
              all.x = T
            
            )[, Exposed := NULL]

#Maybe it is possible to determine per cohort which factor is hampering the cahnge of being matched
###
#cohort <- "HO_P_TO_HE_P"
#indep <- c("type_vax1" , matchCrit)
#COMB2 <- copy(COMB)[matched_cohort == cohort ,][,c("matched_cohort", "nb_match", indep), with = F]
#COMB2[,nb_match := fifelse(nb_match == 0, 0, 1)]
#lapply(c("matched_cohort", indep), function(x) COMB2[, eval(x) := as.character(get(x))])
#COMB2 <- SetToInteger(Data = COMB2, colls =  c("matched_cohort", indep))
#formula <- paste0("  nb_match ~ " ,paste0(indep, collapse = " + "))
#model <- glm(formula = formula, data = COMB2$Data, family = "binomial")
#summary(model)

#install.packages("car")
#library("car")
#vif(model)

###


saveRDS(result, paste0(d3Dir, "MATCH_FLOWCHART.rds"))
saveRDS(COMB, paste0(d3Dir, "MATCH_COMBINATIONS.rds"))

dbDisconnect(mydb)

rm(mydb, scheme, result, PAIRS, D3_output, persons1, persons2, matchCrit, COMB)



cohorts <- c(
  "D3_ANY_P_with_prior_covid_B_to_NB_analytic_dataset",
  "D3_HE_P_B_to_NB_analytic_dataset",
  "D3_children_analytic_dataset",
  "D3_HO_P_HO_B_to_NB_analytic_dataset",
  "D3_HO_P_TO_HE_P_analytic_dataset",
  "D3_HO_P_HE_B_to_NB_analytic_dataset"
  
) 

matchVars <- tolower(c(#"L_GEOREGION_COV_at_matching_date",
                       "sex_at_instance_creation",
                       "Im_TRANSPLANTRECIPIENT_COV_at_matching_date",
                       "G_SEVERERENALDISEASE_CH_at_matching_date",
                       "O_DOWN_COV_at_matching_date",
                       "ageband_at_matching_date",
                       "immunodeficiency_at_matching_date",
                       "CANCER_at_matching_date")
)



for(j in cohorts){
  
  
  for(i in matchVars){
    tmp <- readRDS(paste0(d3Dir ,j,".rds"))[, c("pair_id","person_id","date_start", i), with = F]
    if(!length(unique(tmp$pair_id)) == nrow(unique(tmp[, c("pair_id", i), with = F]))){
      print(j)
      print(i)
      tmp2 <- unique(tmp[,c(i, "pair_id"), with = F])
      print(tmp2[["pair_id"]][duplicated(tmp2[["pair_id"]])])
      rm(tmp2)
    }
    
    rm(tmp)
  }
}

rm(cohorts, matchVars)

if(DAP == "TEST"){

IR <- readRDS(paste0(d3Dir, "D3_HO_P_TO_HE_P_analytic_dataset.rds"))
IR <- IR[, .(person_id, pair_id, exposed_or_comparator , ageband_at_matching_date,  testcovariate_at_matching_date, first_testoutcome, date_start, date_final_censoring, date_final_censoring_days)]
IR <- IR[pair_id %in% IR$pair_id[duplicated(IR$pair_id)],]
IR <- IR[, N := fifelse(first_testoutcome >= date_start & first_testoutcome <= date_final_censoring, 1, 0, na = 0)]

resultIr <- IR[, .(N =sum(N), PT = sum(date_final_censoring_days), COV = sum(testcovariate_at_matching_date)), by = c("exposed_or_comparator", "ageband_at_matching_date")][, IR := N/PT]

resultIr <- dcast(resultIr, ageband_at_matching_date ~ exposed_or_comparator, value.var = c("COV","N", "PT", "IR") )
print(resultIr)

#Make graph
###
GRAPH <- IR[N == 1, day := first_testoutcome - date_start ][, .(person_id, pair_id, exposed_or_comparator , day, date_final_censoring_days, N)][N==1]
setorder(GRAPH, day)
GRAPH <- GRAPH[, CUMIR := cumsum(N), by = "exposed_or_comparator"]
GRAPH <- GRAPH[, `:=` (last = .N, order = seq_len(.N)) , by = c("exposed_or_comparator", "day")][last == order,]

tmp <- plot(NA,NA,xlim = c(0,max(GRAPH$day)), ylim = c(0,max(GRAPH$CUMIR)), xlab = "days", ylab = "sum occurences")
lines(GRAPH[exposed_or_comparator == "exposed",][["day"]], GRAPH[exposed_or_comparator == "exposed",][["CUMIR"]], col = "blue" )
lines(GRAPH[exposed_or_comparator == "comparator",][["day"]], GRAPH[exposed_or_comparator == "comparator",][["CUMIR"]], col = "red" )

rm(tmp, GRAPH)
###



rm(IR, resultIr)
}

D3_matched_population_main_study <- readRDS(paste0(d3Dir, "D3_matched_population_main_study.rds"))
colnames(D3_matched_population_main_study) <- toupper(colnames(D3_matched_population_main_study))
D3_matched_population_main_study <- D3_matched_population_main_study[MATCHED_COHORT != "children",]



#First tests on the matching to no boosters which requires that T0 is eaqual for exposed and comparator 
###

#Checks HO and HE for P and B
##
aatest3 <- D3_matched_population_main_study[substr(MATCHED_COHORT, nchar(MATCHED_COHORT) - 1, nchar(MATCHED_COHORT) ) == "NB", ]
aatest3 <- aatest3[, .(PERSON_ID, PAIR_ID, DATE_VAX3, EXPOSED_OR_COMPARATOR, TYPE_VAX1, TYPE_VAX2, TYPE_VAX3, MATCHED_COHORT)]

aatest3[substr(MATCHED_COHORT, 1, 2 ) == "HO"  & TYPE_VAX1 == TYPE_VAX2, test := T]
aatest3[substr(MATCHED_COHORT, 1, 2 ) == "HE"  & TYPE_VAX1 != TYPE_VAX2, test := T]
aatest3[substr(MATCHED_COHORT, 6, 7 ) == "HO"  & (TYPE_VAX1 == TYPE_VAX3 & TYPE_VAX2 == TYPE_VAX3) & EXPOSED_OR_COMPARATOR == "exposed", test2 := T]
aatest3[substr(MATCHED_COHORT, 6, 7 ) == "HO"  &  EXPOSED_OR_COMPARATOR == "comparator", test2 := T]
aatest3[substr(MATCHED_COHORT, 6, 7 ) == "HE"  & (TYPE_VAX1 != TYPE_VAX3 | TYPE_VAX2 != TYPE_VAX3) & EXPOSED_OR_COMPARATOR == "exposed", test2 := T]
aatest3[substr(MATCHED_COHORT, 6, 7 ) == "HE"  &  EXPOSED_OR_COMPARATOR == "comparator", test2 := T]
aatest3[MATCHED_COHORT == "HE_P_B_to_NB", test2 := T]
aatest3[substr(MATCHED_COHORT, 1, 3 ) == "ANY", `:=` (test = T, test2 = T)]
if(!sum(aatest3$test) == nrow(aatest3)) warning("HO_P or HE_P not correct")
if(!sum(aatest3$test2) == nrow(aatest3)) warning("HO_B or HE_B not correct")

##

#Test if all control booster vaccinations are after booster of exposed
##
id <-  aatest3[duplicated(aatest3$PAIR_ID),][["PAIR_ID"]]

aatest3 <- merge(
  aatest3[PAIR_ID %in% id,][EXPOSED_OR_COMPARATOR == "exposed",],
  aatest3[PAIR_ID %in% id,][EXPOSED_OR_COMPARATOR == "comparator",],
  by = "PAIR_ID"
)[, test := DATE_VAX3.y - DATE_VAX3.x]

if(any(aatest3$test < 0, na.rm = T)) warning("Matching not correct on vacination dates")
rm(aatest3)
gc()

##


#Check HO_P_TO_HE_P in which t0 starts at the second vaccination for both.
###
aatest3 <- D3_matched_population_main_study[MATCHED_COHORT == "HO_P_TO_HE_P", ]
aatest3 <- aatest3[, .(DATE_START, DATE_VAX2, TYPE_VAX1, TYPE_VAX2, EXPOSED_OR_COMPARATOR)]

if(!sum(aatest3$DATE_START == aatest3$DATE_VAX2) == nrow(aatest3)) warning("T0 not equal to date of second vacination")

aatest3[EXPOSED_OR_COMPARATOR == "exposed" & TYPE_VAX1 == TYPE_VAX2, test := T]
aatest3[EXPOSED_OR_COMPARATOR == "comparator" & TYPE_VAX1 != TYPE_VAX2, test := T]

if(!sum(aatest3$test) == nrow(aatest3)) warning("HO_P or HE_P not correct")

###

rm(D3_matched_population_main_study, aatest3)



