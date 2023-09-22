**Principal Investigator**
- Elisa Martin Merino

**Core Scientific Group**
- Elisa Martin Merino
- Fabio Riefolo
- Tiago Andres Vaz
- Rosa Gini
- Lamiae Grimaldi
- Olaf Klungel

**List of authors of the programming code:**
- Tiago Andres Vaz
- Davide Messina
- Roel Elbers
- Albert Cid Royo
- Vjola Hoxhaj
- Belén Castillo Cano
- Dorieke Brink-Kwakkel
- Rosa Gini

**based on ConcePTION common data model** 

**using VAC4EU definitions and tools:** https://vac4eu.org 

**How it works**

this pipeline implements the "BRIDGE" concept. 
It works reading a list of codes provided by VAC4EU:
../date/meta/20220930.csv
../date/meta/ALL_drugs_full_codelist.csv
../date/meta/vaccines_codelist.csv

codes later are selected and combined using algorithms. The selection
and combination of codes into study variables are defined by metadata:

../date/meta/study_variables.csv
../date/meta/algorithms.csv
../date/meta/specific_additional_concepts.csv

Example: For the study variable CANCER (algorithms.csv) is assigned by the combination of
Onc_ANYMALIGNANCY_COV or Onc_MALIGNANTTUMOR_CH and then these two are linked by study_variables.csv 
looking for STUDY_VARIABLE and ORIGIN joined with the codelist in 20220930.csv by SYSTEM, EVENT_ABBREVIATION and TYPE.

**Instructions**

1) Connected to GitHub, clone repository or download the .ZIP code 
2) Save your files to your Project Folder (ex: /home/cove)
3) Unzip DemonstrationData.zip. Copy and paste data files inside  ../CDMInstances/
5) Inside RStudio:

    - clean your environment
    
    - then, run "main.R" 
 
**Results**

statistical results and aggregated tables will be generated inside folders "outcome" 
results to be exported are in "outcome/exports" 