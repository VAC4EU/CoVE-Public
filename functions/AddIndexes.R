

AddIndexes <- function(
    db.path,
    needed.fields = NULL, 
    remove.duplicated = F,
    index.all = F,
    tables = NULL
    ){

mydb <- dbConnect(RSQLite::SQLite(), db = db.path)


tablesList <- dbListTables(mydb)
if(!is.null(tables)) tablesList <- tablesList[tablesList %in% tables]

if(length(tablesList) > 0){
  
      for(i in tablesList){
        
        if(sum(needed.fields %in% dbListFields(mydb, i)) == length(needed.fields)){
        
        #Remove duplicates
        ###
        if(remove.duplicated){
        p <- dbSendStatement(mydb,
                             paste0(
                               "DELETE FROM ",i,"
                                WHERE rowid NOT IN (select min(rowid)
                                FROM ",i,"
                                group by ",paste0(dbListFields(mydb, i), collapse = " , "),")
                                "                    
                             )
                             
        )
        
        dbClearResult(p)
        }
        ###
        
        #Add indexes
        ###
        indexes_available <- dbGetQuery(mydb,"SELECT name  FROM sqlite_master WHERE type = 'index'")[["name"]]
        
        cols <- dbListFields(mydb, i)
        if(!index.all & !is.null(needed.fields)) cols <- cols[ cols %in% needed.fields]
        
        if(!paste0(i,"_index") %in% indexes_available){
          p <- dbSendStatement(mydb, paste0("CREATE INDEX ",i,"_index ON ",i,"(",paste0(cols, collapse = ","),")"))
          dbClearResult(p)
        }
        ###
       
        rm(indexes_available, cols)
        
        }
      }
}


dbDisconnect(mydb)

}

