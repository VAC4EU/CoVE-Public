#  RRRRRRRRRRRRRRRRR   WWWWWWWW                           WWWWWWWUEEEEEEEEEEEEEEEEEEEEE
#  R::::::::::::::::R  W::::::W                           W::::::M::::::::::::::::::::E
#  R::::::RRRRRR:::::R W::::::W                           W::::::C::::::::::::::::::::E
#  RR:::::R     R:::::UU::::::W                           W::::::UE::::::EEEEEEEEE::::E
#    R::::R     R:::::R W:::::W           WWWW            W:::::W E:::::E       EEEEEE
#    R::::R     R:::::R  W:::::W         W:::::W         W:::::W  E:::::E             
#    R::::RRRRRR:::::R    W:::::W       W:::::::W       W:::::W   E::::::EEEEEEEEEE   
#    R:::::::::::::RR      W:::::W     W:::::::::W     W:::::W    E:::::::::::::::E   
#    R::::RRRRRR:::::R      W:::::W   W:::::W:::::W   W:::::W     E:::::::::::::::E   
#    R::::R     R:::::R      W:::::W W:::::W W:::::W W:::::W      E::::::EEEEEEEEEE   
#    R::::R     R:::::R       W:::::W:::::W   W:::::W:::::W       E:::::E             
#    R::::R     R:::::R        W:::::::::W     W:::::::::W        E:::::E       EEEEEE
#  RR:::::R     R:::::R         W:::::::W       W:::::::W       EE::::::EEEEEEEE:::::E
#  R::::::R     R:::::R          W:::::W         W:::::W        E::::::::::::::::::::E
#  R::::::R     R:::::R           W:::W           W:::W         E::::::::::::::::::::E
#  RRRRRRRR     RRRRRRR            WWW             WWW          EEEEEEEEEEEEEEEEEEEEEE
#                                                                                                
#      ****************       REAL WORLD EVIDENCE PIPELINE      *******Ver. 2.1*******                                                                                           
#                                                                                                
# Project: CoVE 
# based on ConcePTION project: https://www.imi-conception.eu
# using VAC4EU definitions and tools: https://vac4eu.org 
# Creation date: 22/07/2022
# See authors, license and other details at: https://github.com/VAC4EU/CoVE/
#
# Release notes (Ver.2.1 - The Bridge):
#    Including RWE functions to connect (bridge) PI definitions into a new metadata database
#    allows investigators to select study specific variables, outcomes and other without change .R code
# -------------------------------------------------------------------------------------------------

# Start/reset the environment
rm(list=ls())

# Load packages
studyName <- "COVE"
if(!require(rstudioapi)){install.packages("rstudioapi")}
suppressPackageStartupMessages(library(rstudioapi))
if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))
if(!require(dplyr)){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))
if(!require(lubridate)){install.packages("lubridate")}
suppressPackageStartupMessages(library(lubridate))
if(!require(tidyverse)){install.packages("tidyverse")}
suppressPackageStartupMessages(library(tidyverse))
if(!require(tidyselect)){install.packages("tidyselect")}
suppressPackageStartupMessages(library(tidyselect))
if(!require(reshape)){install.packages("reshape")}
suppressPackageStartupMessages(library(reshape))
if(!require(rlist)){install.packages("rlist")}
suppressPackageStartupMessages(library(rlist))
if(!require("DBI")){install.packages("DBI")}
suppressPackageStartupMessages(library(DBI))
if(!require("Rcpp")){install.packages("Rcpp")}
suppressPackageStartupMessages(library("Rcpp"))
if(!require("sqldf")){install.packages("sqldf")}
suppressPackageStartupMessages(library("sqldf"))
if(!require("RSQLite")){install.packages("RSQLite")}
suppressPackageStartupMessages(library(RSQLite))
if(!require("WeightIt")){install.packages("WeightIt")}
suppressPackageStartupMessages(library(WeightIt))
if(!require("fmsb")){install.packages("fmsb")}
suppressPackageStartupMessages(library(fmsb))
if(!require("survival")){install.packages("survival")}
suppressPackageStartupMessages(library(survival))
if(!require("cobalt")){install.packages("cobalt")}
suppressPackageStartupMessages(library(cobalt))
if(!require("survminer")){install.packages("survminer")}
suppressPackageStartupMessages(library(survminer))
if(!require("data.table")){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))
if(!require("dplyr")){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))
if (!require("gtsummary")) install.packages("gtsummary")
suppressPackageStartupMessages(library(gtsummary))
if (!require("gt")) install.packages("gt")
suppressPackageStartupMessages(library(gt))
if (!require("scales")) install.packages("scales")
suppressPackageStartupMessages(library(scales))

#Set pipeline project directory with path to source code folder (to be used by all other folder definition)
projectDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectDir)

#Set source folder for .R scripts based on projectDir
sourceDir<-paste0(projectDir,"/src/")  
if(dir.exists(sourceDir)==F){dir.create(sourceDir)}

#Set source Data path
pathCDM<-paste0(projectDir,"/data/CDMInstances/CDM_arq/")
invisible(if(dir.exists(pathCDM)==F)
{dir.create(pathCDM)})

#Set functions path
functionsDir <- paste0(projectDir,"/functions/")
invisible(if(dir.exists(functionsDir)==F)
{dir.create(functionsDir)})

#Paths to the database where the concepts and matchings are stored.
dbDir <- paste0(projectDir,"/data/db/")
invisible(if(dir.exists(dbDir)==F)
{dir.create(dbDir)})
dbConceptsFile <- paste0(dbDir,"CONCEPTS.db") 

#Set folder for matching results
matchingDir <- paste0(projectDir,"/data/db/matching/")
invisible(if(dir.exists(matchingDir)==F)
{dir.create(matchingDir)})
dbMatchingFile <- paste0(matchingDir,"MATCHING.db") 

#Set meta path
metaDir <- paste0(projectDir,"/data/meta/")
invisible(if(dir.exists(metaDir)==F)
{dir.create(metaDir)})

#Create structure inside output folder 
#D3 individual level datasets
d3Dir<-paste0(projectDir,"/data/D3/")
invisible(if(dir.exists(d3Dir)==F)
{dir.create(d3Dir)})

#D4 analytical datasets
d4Dir<-paste0(projectDir,"/data/D4/")
invisible(if(dir.exists(d4Dir)==F)
{dir.create(d4Dir)})

#Output
outputDir<-paste0(projectDir,"/output/")
invisible(if(dir.exists(outputDir)==F)
{dir.create(outputDir)})

#Export
exportDir<-paste0(projectDir,"/output/export")
invisible(if(dir.exists(exportDir)==F)
{dir.create(exportDir)})

#Export Source Scripts
originDir<-paste0(projectDir,"/output/origin")
invisible(if(dir.exists(originDir)==F)
{dir.create(originDir)})


#ARS functions
source(paste0(functionsDir, "CreateSpells_v15.R"))
source(paste0(functionsDir, "CreateFlowChart.R"))

#UMCU functions
source(paste0(functionsDir, "ImportPattern.R"))
source(paste0(functionsDir, "GetConceptsProcedure.R"))
source(paste0(functionsDir, "CreateConceptDatasets.R"))
source(paste0(functionsDir, "CreateConceptDatasetsMultipleVars.R"))
source(paste0(functionsDir, "AppendConcepts.R"))
source(paste0(functionsDir, "CleanOutcomes.R"))
source(paste0(functionsDir, "DevideTF.R"))
source(paste0(functionsDir, "MakeSpells.R"))
source(paste0(functionsDir, "SetToInteger.R"))
source(paste0(functionsDir, "MatchingSQLSpells.R"))
source(paste0(functionsDir, "GetDatesIR.R"))
source(paste0(functionsDir, "GetDayDif.R"))
source(paste0(functionsDir, "CalculateBMI.R"))
source(paste0(functionsDir, "AddIndexes.R"))
source(paste0(functionsDir, "MakeBooleanCovariates.R"))
source(paste0(functionsDir, "GetStatisticalOutcomes.R"))
source(paste0(functionsDir, "MatchingSQLSpellsBatched.R"))

# Environment Loaded 

#  Now running ...
#   ______                   _                  
#  |_____ \                 | |                
#   _____| |_  ____   _____ | |  _  ____   _____ 
#  |  ____/| ||  _ \ | ___ || | | ||  _ \ | ___ |
#  | |     | || |_| || ____|| | | || | | || ____|
#  |_|     |_||  __/ |_____) \_)|_||_| |_||_____)
#             |_|                                
#  DESIGNED STEPS TO PROCESS REAL WORLD EVIDENCES  

#Retrieves the  name of the datasource where the script is running and assign it to DAP
CDM_SOURCE<- fread(paste0(pathCDM,"CDM_SOURCE.csv"))
thisDataSource <- as.character(CDM_SOURCE[1,3])
DAP <- thisDataSource
#Parameter for histogram: Divide the number of repeated comparators by the number below. 
#X axis of histogram
resize_parameter_x <- 100
#Y axis of histogram
resize_parameter_y <- 10

#Define study specific start and end dates
studyStartDate <- "20201220"
studyStartDate2 <- as.Date(studyStartDate, format = "%Y%m%d")
studyEndDate <- "20220101"
studyEndDate2 <- as.Date(studyEndDate, format = "%Y%m%d")

#Sampling
#Default settings are sampleSize = NULL and useSample = F
sampleSize <- NULL
useSample <- F

#benchmarking time 
startTime <- Sys.time()

#Pipeline steps for: 
# - read meta-data (./data/meta) 
# - create study population
# - covariates, outcomes and matching variables 
# - matching
# - statistics
# - reporting

if(!is.null(sampleSize)) system.time(source(paste0(sourceDir, "00_TakeSample.R")))
if(useSample) pathCDM <- paste0(dbDir,"sample")

system.time(source(paste0(sourceDir, "00_set_codesheets.R")))
system.time(source(paste0(sourceDir, "01_create_persons.R")))
system.time(source(paste0(sourceDir, "02_apply_create_spells.R")))
system.time(source(paste0(sourceDir, "03_create_database_concepts.R")))  
system.time(source(paste0(sourceDir, "04_apply_algorithms.R")))
system.time(source(paste0(sourceDir, "05_clean_spells.R")))
system.time(source(paste0(sourceDir, "06_clean_vaccines.R")))
system.time(source(paste0(sourceDir, "07_apply_criteria_for_doses.R")))
system.time(source(paste0(sourceDir, "08_selection_criteria_from_PERSON_to_study_population.R")))
system.time(source(paste0(sourceDir, "09_create_study_population.R")))
system.time(source(paste0(sourceDir, "10_create_covid_episodes.R")))
system.time(source(paste0(sourceDir, "11_create_covid_outcomes.R")))
system.time(source(paste0(sourceDir, "12_create_time_independent_matching_file.R")))
system.time(source(paste0(sourceDir, "13_create_time_dependent_matching_variables.R")))
system.time(source(paste0(sourceDir, "14_prepare_matching_database.R")))
system.time(source(paste0(sourceDir, "15_execute_matching.R")))
system.time(source(paste0(sourceDir, "16_apply_cencoring.R")))
system.time(source(paste0(sourceDir, "17_add_outcomes_categorical.R")))
system.time(source(paste0(sourceDir, "18_add_outcomes.R"))) 
system.time(source(paste0(sourceDir, "19_sample_and_devide_populations.R")))
system.time(source(paste0(sourceDir, "20_statistics.R")))
system.time(source(paste0(sourceDir, "21_export_code.R")))
#This step is commented because it is used during development
#system.time(source(paste0(sourceDir, "22_evaluate_matching.R")))
system.time(source(paste0(sourceDir, "23_tables_1_to_6.R")))

#compute total running time
endTime <- Sys.time()
endTime - startTime
#FINISHED WITH SUCCESS
