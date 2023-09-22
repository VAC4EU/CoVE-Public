
MatchingSQLSpells <- function(
                              db,
                              colls, #Time dependent matching criteria (columns in CASES table with format 1/0) 
                              colls2, #Time independent matching criteria (columns in CASES table with format categorical string)
                              ids = NULL , #Possibility to match a subset of the exposed. Input is a vector of person_id's
                              print = F, 
                              exposed.t0, #Date that defines T0 for the exposed and if control.t0 is NULL also for the control.
                              control.t0 = NULL, #Date that defines T0 for control
                              exposed.group = NULL, #Defines the population from which the exposed are picked. (columns in CASES table with format 1/0)  
                              control.group = NULL, #Defines the population from which the controls are picked. (columns in CASES table with format 1/0 
                              code.join = NULL, #Optional sql code to add to the join. So can be interpedated as additional matching criteria  
                              select.exp, 
                              select.con 
                              
                              ){
  
  library("data.table")
  library("RSQLite")
  library("DBI")
  
  #Create new temporary database
  dbtmp <- dbConnect(RSQLite::SQLite(), "")
  
  #Attach new temporary database with standardized matching database. By using attach it is possible to work in parallel if needed.
  p <- dbSendStatement(dbtmp, paste0("ATTACH DATABASE '",db,"' AS tempdb"))
  dbClearResult(p)
  
  #Code uses 3 steps.
  # 1 Create a table for the exposed and a table for the controls. These tables are created by 2 queries on the CASES table in the matching database.
  # 2 Join the 2 tables from step 1 to generate a table with all the possible matches
  # 3 Randomly select 1 match per exposed
  
  #STEP 1
  ####
  
  #SELECT SQL
  ###
  #Write SQL code for the SELECT part of the code. First create the code for the variables that are needed in all situation
  selectControls <- paste0(c("person_id" , colls, colls2), collapse = ", ")
  selectExposed <- paste0(c("person_id", exposed.t0, colls, colls2), collapse = ", ")
  
  #If no control.t0 is specified, then the t0 depends on the t0 from the exposed. In that situation it is part of the join. Therefore the ST3 and EN3 need to be added to the SELECT  
  if(is.null(control.t0)){selectControls <- paste0(c(selectControls,"ST3", "EN3", exposed.t0), collapse = ", ")}else{selectControls <- paste0(c(selectControls,control.t0), collapse = ", ")}
  ###
  
  #WHERE SQL
  ###
  #Write the SQL code for the WHERE statements. The CASES table from the matching database needs to contain a boolean column for every grouping factor. 
  #If the controls have no group the whole CASES table is used 
  if(!is.null(control.group)){whereControls1 <- paste0(control.group, " = 1")}else{whereControls1 <- NULL}
  #If exposed and controls have an separate t0 date then both groups need the select based on T0. If the controls have now T0 the T0 of the exposed is used and this is done in the JOIN part  
  if(!is.null(control.t0)){whereControls2 <- paste0(control.t0, " BETWEEN ST3 AND EN3 ")}else{whereControls2 <- NULL}
  whereControls <- paste0(c(whereControls1, whereControls2), collapse =  " AND " )
  #Dummy expression in the case that there is no group defined
  if(whereControls == "") whereControls <- " 1=1  "
  
  #Make the where statement for the exposed. 
  if(!is.null(exposed.group)){whereExposed1 <- paste0(exposed.group, " = 1")}else{whereExposed1 <- NULL}
  if(!is.null(exposed.t0)){whereExposed2 <- paste0(exposed.t0, " BETWEEN ST3 AND EN3 ")}else{whereExposed2 <- NULL}
  if(!is.null(ids)){whereExposed3 <- paste0("person_id IN(",ids,")")}else{whereExposed3 <- NULL}
  whereExposed <- paste0(c(whereExposed1, whereExposed2, whereExposed3), collapse =  " AND " )
  ###
  
  #If print is TRUE a message is printed if started with a batch. This is only usefull when working with batches
  if(print & !is.null(ids)) print(paste0("Matching of ",length(gregexpr(",", ids, fixed = TRUE)[[1]]) , " subject(s) is started"))
  
  #Execute the SQL code
  if(is.null(ids)) p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Exposed AS SELECT ",selectExposed," ",select.exp," FROM tempdb.CASES t1 WHERE ",whereExposed))
  ###
  if(!is.null(ids)) p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Exposed AS SELECT ",selectExposed," ",select.exp," FROM tempdb.CASES t1 WHERE person_id IN(",ids,") AND ",whereExposed))
  ###
  dbClearResult(p)
  
  p <- dbSendStatement(dbtmp, paste0("CREATE TABLE Controls AS SELECT ",selectControls," ",select.con," FROM tempdb.CASES t2 WHERE ",whereControls))
  dbClearResult(p)
  
  #Add indexes to optimize the join. The join is the most time consuming step
  p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Exposed_index ON Exposed(",paste0(dbListFields(dbtmp, "Exposed"), collapse = ","),")"))
  dbClearResult(p)
  
  p <- dbSendStatement(dbtmp, paste0("CREATE INDEX Controls_index ON Controls(",paste0(dbListFields(dbtmp, "Controls"), collapse = ","),")"))
  dbClearResult(p)
  
  ####
  
  
  #STEP 2
  ####
  
  #Generate the generic part of the SQL join code. This is a simple form t1.VAR = t2.VAR
  colls3 <- c(colls, colls2)
  CODE_JOIN2 <- paste0("t1.",colls3," = t2.",colls3, collapse =  " AND ")
  
  CODE_SELECT1 <- paste0("t1.",c(colls3, paste0(exposed.t0, " AS T0 ")), collapse = ",")
  if(!is.null(control.t0)){CODE_SELECT2 <- paste0("t2.", control.t0, " AS T0_CONTROL ")}else{CODE_SELECT2 <- NULL}
  CODE_SELECT <- paste0(c(CODE_SELECT1, CODE_SELECT2), collapse = ", ")
  
  if(is.null(control.t0)){
    
    CODE_JOIN3 <- paste0(
      paste0("(t1.",exposed.t0, " BETWEEN t2.ST3 AND t2.EN3 )"),
      " AND ",
      paste0("(t1.",exposed.t0, " < t2.",exposed.t0, " OR t2.",exposed.t0, " IS NULL)")
    )
    
    
  }else{CODE_JOIN3 <- NULL}  
  
  CODE_JOIN <- paste0(c(CODE_JOIN2, CODE_JOIN3), collapse = " AND ")
  
  p <- dbSendStatement(dbtmp, paste0(
    "         
              CREATE TABLE POS_MATCH AS
              SELECT DISTINCT
              t1.person_id as Exposed,
              t2.person_id as Control,
              ",CODE_SELECT,"
              
              
              FROM Exposed t1 
              
              left join Controls t2
              
              on(
                    ",CODE_JOIN,"
                    
                    AND t1.person_id IS NOT t2.person_id
                    
                    ",code.join,"
              )
            
            
           "
  ))
  
  #",select.exp,"
  #",select.con,"
  
  dbClearResult(p)
  
  ####
  
  
  #STEP 3
  ####
  
  
  TEMP1 <- dbGetQuery(dbtmp ,
                      
                      "
          SELECT * FROM(
          SELECT * , ROW_NUMBER () OVER ( 
                  PARTITION BY Exposed
                  ORDER BY Exposed, random()
          		
              ) NB  
          
          FROM POS_MATCH
          )
          
          WHERE NB = 1
          
          "                    
                      
  )
  
  TEMP2 <- dbGetQuery(dbtmp , "SELECT Exposed, COUNT(Control) as nb_match FROM POS_MATCH GROUP BY Exposed")
  
  TEMP <- as.data.table(merge(TEMP1, TEMP2, by = "Exposed"))[, NB := NULL] 
  
  ####
  
  p <- dbSendStatement(dbtmp, "DROP TABLE POS_MATCH")
  dbClearResult(p)
  
  dbDisconnect(dbtmp)
  
  return(TEMP)
  
  
  rm(TEMP,TEMP1,TEMP2)
  gc()
  
  
}



#To add later the option to include a minimum lookback period

# SELECT * , CASE WHEN test = 1 AND test2 = 0 THEN EN3 - 1 ELSE EN3 END EN3_new
# 
# 
# 
# FROM(
#   SELECT *, lag(test,-1,0)OVER(PARTITION BY person_id) AS test2
#   
#   
#   
#   FROM
#   (
#     SELECT
#     person_id,
#     ST3,
#     EN3,
#     CASE
#     WHEN
#     1 = ST3 - lag(EN3, 1,0) OVER(
#       PARTITION BY person_id
#       ORDER BY person_id, EN3
#     )
#     THEN  1
#     END AS test
#     
#     
#     
#     
#     FROM CASES
#   )
# )





