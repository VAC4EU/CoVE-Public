GetDatesIR <- function(
                        
                        Concept,
                        Start_date, 
                        FILE, 
                        c.name, 
                        
                        prior = T, 
                        prior.order = "DESC",
                        prior.col = "Date",
                        prior.sum = F,
                        
                        post = T, 
                        post.order = "ASC",
                        post.col = "Date",
                        
                        between = F,
                        c.st, 
                        c.en,   
                        
                        lookback.days, 
                        endpoint = "t1.op_end_date", 
                        coll = "", 
                        db, 
                        
                        keep_start_date = F,
                        
                        pathrds = NULL,
                        
                        id1 = "person_id",
                        id2 = "pair_ID"
                        
                        ){
  
  library(data.table)
  library(RSQLite)
  library(DBI)
  #########
  
    if(between & (prior | post)){stop("If between is TRUE, prior and post can only be FALSE")}  
  
    mydb <- dbConnect(RSQLite::SQLite(), db)
        
    
    if(nchar(coll) > 0){
      coll <- unlist(strsplit(coll, ",")[[1]])
      coll0 <- paste0(",",coll, collapse = " ")
      coll2 <- paste0(",t2.",coll, collapse = " ")  
      
      }else{
        coll0 <- ""
        coll2 <- ""
      }
    
    #coll0 <- if(nchar(coll) > 0){paste0(",",coll, collapse = " ")}else{coll0 <- ""}
    #coll2 <- if(nchar(coll) > 0){paste0(",t2.",coll, collapse = " ")}else{coll0 <- ""}
    
    if(keep_start_date){
      start0 <- paste0(",", Start_date, " as REFDT")
      start2 <- paste0(",t1.", Start_date)
    }else{
      start0 <- ""
      start2 <- "" 
      }
    
    if(post){
    temp2 <- as.data.table(dbGetQuery(mydb,paste0(
     "

     SELECT ",id1,", Date, ",id2," ",coll0, start0,"  FROM(
     SELECT
     * ,
     ROW_NUMBER () OVER (
                     PARTITION BY ",id1,", ",id2,"
                     ORDER BY ",post.col," ",post.order,"
                     )  NB


     FROM(

     SELECT DISTINCT
     t1.",id1,",
     t1.",id2,",
     t2.Date
     ",coll2,"
     ",start2,"

     FROM ",FILE," t1

     inner join ",Concept," t2

     ON (t1.",id1," = t2.",id1," AND (t2.Date BETWEEN (t1.",Start_date," - 0) AND (",endpoint,")))

 	  )
    )
   WHERE NB = 1

     "

   )
   )
   )
    }  
  
  #browser() 
    
  if(prior){
  
      if(prior.sum == F){
         temp1 <- as.data.table(dbGetQuery(mydb,paste0(
           "
      
           SELECT ",id1,", Date, ",id2," ",coll0, start0,"  FROM(
           SELECT
           * ,
           ROW_NUMBER () OVER (
                           PARTITION BY ",id1,", ",id2,"
                           ORDER BY ",prior.col," ",prior.order,"
                           )  NB
      
           FROM(
      
           SELECT DISTINCT
           t1.",id1,",
           t1.",id2,",
           t2.Date
           ",coll2,"
           ",start2,"
      
           FROM ",FILE,"  t1
      
           inner join ",Concept," t2
           
     
            ON (t1.",id1," = t2.",id1," AND (t2.Date BETWEEN (t1.",Start_date," - (",lookback.days,")) AND (t1.",Start_date," - 1 )))
       	  )
         )
         WHERE NB = 1
      
          "
      
         )
         )
         )
      }
    
    if(prior.sum == T){
      temp1 <- as.data.table(dbGetQuery(mydb,paste0(
        "
        
             SELECT ",id1,", cast(NULL as INT) AS Date, ",id2," , COUNT(*) AS ",coll, start0,"  
        
             FROM(
        
             SELECT DISTINCT
             t1.",id1,",
             t1.",id2,",
             t2.Date,
             t2.Value
             ",start2,"
        
             FROM ",FILE,"  t1
        
             inner join ",Concept," t2
        
             ON (t1.",id1," = t2.",id1," AND (t2.Date BETWEEN (t1.",Start_date," - (",lookback.days,")) AND (t1.",Start_date," - 1)))
        
         	  )
           
           GROUP BY ",id1,", ",id2,"
            "
        
      )
      )
      )
    }    
    
    
  }

  
    
  if(between){
    
    temp3 <- as.data.table(dbGetQuery(mydb,paste0(
      "

     
     SELECT DISTINCT
     t1.",id1,",
     ",id2,",
     ",c.st,",
     ",c.en,"
     ",coll0,"
     ",start0,"
    
     FROM ",FILE," t1

     inner join ",Concept," t2

     ON (t1.",id1," = t2.",id1," AND ",Start_date," BETWEEN ",c.st," AND ",c.en,")
    
     "
    )
    )
    )
  }      
      
      
    
    
    
  if(is.null(pathrds)){
    if(post & prior) temp <- list(file1 = temp1, file2 = temp2, Concept = c.name)
    if(post & !prior) temp <- list(file2 = temp2, Concept = c.name) 
    if(!post & prior) temp <- list(file1 = temp1, Concept = c.name)
    if(between) temp <- list(file3 = temp3, Concept = c.name)
    return(temp)
  }
  
  if(!is.null(pathrds)){
    if(prior){
      if("Date" %in% colnames(temp1)) temp1 <- temp1[, Date := as.Date(Date, origin = "1970-01-01")]
      setnames(temp1, colnames(temp1)[!colnames(temp1) %in% c(id1, id2)], paste0(colnames(temp1)[!colnames(temp1) %in% c(id1, id2)], "_HIST"))
      }
    
    if(post){
      if("Date" %in% colnames(temp2)) temp2 <- temp2[, Date := as.Date(Date, origin = "1970-01-01")]
      setnames(temp2, colnames(temp2)[!colnames(temp2) %in% c(id1, id2)], paste0(colnames(temp2)[!colnames(temp2) %in% c(id1, id2)], "_COUNT"))
    }
    
    if(between){
      if(c.st %in% colnames(temp3)) temp3 <- temp3[, eval(c.st) := as.Date(get(c.st), origin = "1970-01-01")]
      if(c.en %in% colnames(temp3)) temp3 <- temp3[, eval(c.en) := as.Date(get(c.en), origin = "1970-01-01")]
      if("REFDT" %in% colnames(temp3)) temp3 <- temp3[, REFDT := as.Date(REFDT, origin = "1970-01-01")]
    }
      
    if(post & prior) temp <- merge(x = temp1, y = temp2, by = c(id1, id2), all = T)
    if(post & !prior) temp <- temp2
    if(!post & prior) temp <- temp1
    if(between) temp <- temp3
    
    if(nrow(temp) > 0) saveRDS(temp ,paste0(pathrds,"/",c.name,".rds"))
    return(c.name)
    
  }
      
  print(paste0("Dates for column ",c.name," are collected"))
  
  dbDisconnect(mydb)

 
}
  
