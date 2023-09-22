


DevideTF <- function(FILE, FILEP, c.start, c.end, id, start_study_date, end_study_date){
  
  FILE <- copy(FILE)
  setnames(FILE, c(id, c.start, c.end), c("ID", "STDT", "ENDT")  )
  
  setorder(FILE, "ID", STDT)
  
  FILE2 <- FILE[, EN2 := shift(ENDT), by = "ID"][EN2 < STDT, ":=" (ST = EN2 + 1, EN = STDT - 1)]
  
  FILE2 <- FILE2[!is.na(EN2),][,.(ID, ST, EN)][, Status := F]
  FILE <- FILE[,.(ID, STDT, ENDT)][, Status := T]
  setnames(FILE , c("STDT","ENDT"), c("ST","EN"))
  
  FILE <- rbind(FILE, FILE2)
  rm(FILE2)
  gc()
  
  
  FILE3 <- FILE[, .(ST2 = min(ST), EN2 = max(EN)), by = "ID"]
  FILE4 <- copy(FILE3)[start_study_date < ST2, ":=" (ST = start_study_date, EN = ST2 - 1)][!is.na(ST),][,.(ID, ST, EN)][, Status := F]
  FILE5 <- copy(FILE3)[end_study_date > EN2, ":=" (ST = EN2 + 1, EN = end_study_date)][!is.na(ST),][,.(ID, ST, EN)][, Status := F]
  
  FILE <- rbind(FILE, FILE4, FILE5)
  
  rm(FILE3, FILE4, FILE5)
  gc()
  
  FILEP <- copy(FILEP)
  setnames(FILEP, c(id), "ID")
  
  FILEP <- FILEP[!ID %in% unique(FILE$ID),]
  FILE6 <- FILEP[, ":=" (ST = start_study_date, EN = end_study_date, Status = F)][, .(ID, ST, EN, Status)]
  FILE <- rbind(FILE, FILE6)
  
  setnames(FILE, "ID", id  )
  
  return(FILE)
  
  
  
}
