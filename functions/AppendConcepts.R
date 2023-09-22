

AppendConcepts <- function(DB, CONCEPTS, NAME, DT.coll = "Date", colls = "*"){
  
  MISSING <-  CONCEPTS[!CONCEPTS %in% dbListTables(DB)]  
  if(length(MISSING) > 0) print(paste0(paste0(MISSING, collapse = " "), " not in database"))  
  
  if(any(CONCEPTS %in% dbListTables(DB))){
    
    CONCEPTS <- CONCEPTS[CONCEPTS %in% dbListTables(DB)]
    
    if(length(CONCEPTS) > 1) Query <- paste0("SELECT DISTINCT ",colls," FROM ", CONCEPTS[1],paste0(paste0(" UNION SELECT ",colls," FROM ",  CONCEPTS[2:length(CONCEPTS)]), collapse = " "))
    if(length(CONCEPTS) == 1) Query <- paste0("SELECT DISTINCT ",colls," FROM ", CONCEPTS[1])
    
    for(i in CONCEPTS) if(dbGetQuery(DB, paste0("SELECT count(*) FROM ", i)) == 0) warning(paste0("0 cases in ",NAME," for ",i))
    
    return(as.data.table(dbGetQuery(DB, Query))[, eval(DT.coll) := as.Date(get(DT.coll), origin="1970-01-01")])
  }else{
    warning(paste0("0 tables for ",NAME," in database"))
    return(NULL)
  }
}
