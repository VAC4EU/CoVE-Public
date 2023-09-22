

MatchingSQLSpellsBatched <- function(db = dbMatchingFile, batchSize = 1000){

      mydb <- dbConnect(RSQLite::SQLite(), dbMatchingFile)
      
      persons <- dbGetQuery(mydb,  "SELECT DISTINCT PERSON_ID FROM CASES")$person_id
      dbDisconnect(mydb)
      
      x <- split(persons, sort(seq_along(persons))%%(1 * ceiling(length(persons)/batchSize)))
      
      #cnt <- 1
      
      MATCHED <- lapply(x, function(x){ 
              
            MATCHED_tmp <- MatchingSQLSpells(
                db = dbMatchingFile,
                colls = files,
                colls2 = matchingPersons,
                ids = paste0(x, collapse = ","),  
                print = F,
                exposed.t0 = expT0,
                control.t0 = conT0, 
                exposed.group = expGroup, 
                control.group = conGroup,
                code.join = optionalJoinCode,
                select.exp =  optionalSelectCodeExp,
                select.con =  optionalSelectCodeCon
                
            )[, matched_cohort := pop]
            
            #print(paste0("batch ",cnt, " out of ",length(x)))
            #cnt <- cnt + 1
            
            }
            
            
      
      )
      
      MATCHED <-  do.call(rbind, MATCHED)
  return(MATCHED)

}


