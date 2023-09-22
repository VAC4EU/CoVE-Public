
#TEMP2 = TEMP
# codesheet = FILE_TEMP
# file = TEMP2
# f.id = "person_id"
# c.columns = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "col"])
# c.values = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "val"])
# c.date = "date_column"
# c.outcome = "Outcome"
# c.keep = "keep"
# c.keepnames = "Outcome"
# db = tmpdb

CreateConceptDatasetsMultipleVars <- function(codesheet,file ,f.id, c.keep = NULL, c.keepnames = NULL, c.columns,c.values, c.date, c.outcome, db = NULL ){
  
  codesheet <- copy(codesheet)
  
  
  #Get data. If not use copy input dataset may be affected (see data.table properties)
  file <- copy(file) 
  file <- unique(file)
  
  colls <- unique(c(c.columns, c.values, c.outcome, c.date, c.keep, c.keepnames))
  #colls2 <- c(f.id, c.keep, "Outcome", "Voc", "Date" )
  codesheet <- codesheet[,..colls]
  
  for(i in 1:length(c.columns)){
    if(i == 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), var := paste0(get(c.columns[i])," == '", get(c.values[i]),"'")]
    if(i > 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), var := paste0(var ," & ",paste0(get(c.columns[i])," == '", get(c.values[i]),"'"))]
    
    if(i == 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), Voc := paste0(get(c.columns[i]),".", get(c.values[i]))]
    if(i > 1) codesheet <- codesheet[!is.na(get(c.columns[i])) & !is.na(get(c.values[i])), Voc := paste0(Voc ," | ",paste0(get(c.columns[i]),".", get(c.values[i])))]
    
    
  }
  
  keepcolls <- unique(c(c.outcome, c.date, c.keep, c.keepnames, "var", "Voc"))
  codesheet <- codesheet[, ..keepcolls]
  codesheet <- codesheet[, Val := fifelse(grepl("[|]", Voc), trimws(substr(Voc, max(unlist(gregexpr("[|]", Voc)))+ 1, nchar(Voc)), "b"), Voc), by = list(row.names(codesheet))]
  codesheet <- codesheet[, Voc2 := fifelse(grepl("[|]", Voc), trimws(substr(Voc,1 , max(unlist(gregexpr("[|]", Voc))) - 1), "b"), "NO_VOC"), by = list(row.names(codesheet))]
  
  
  for(j in 1:nrow(codesheet)){
    
    x <- codesheet[j,][["var"]]
    TEMP <- file[eval(parse(text = x)),]
    
    if(nrow(TEMP) > 0){
    setnames(TEMP, codesheet[j,][[c.date]], "Date" )
    if(!is.na(codesheet[j,][[c.keep]]))setnames(TEMP, codesheet[j,][[c.keep]], codesheet[j,][[c.keepnames]] )  
    TEMP <- TEMP[, Voc := codesheet[j,][["Voc2"]]]
    TEMP <- TEMP[, Outcome := codesheet[j,][["Outcome"]]]
    TEMP <- TEMP[, ':=' (Value = codesheet[j,][["Val"]])] 
    
    
    
    if(!is.na(codesheet[j,][[c.keep]])){
      
      TEMP <- TEMP[, Voc := paste0(Voc,"|", Value)]
      setnames(TEMP, codesheet[j,][[c.keepnames]], "Value" )
      }
      #colls2 <- c(f.id, codesheet[j,][[c.keepnames]], "Outcome", "Value", "Voc", "Date" )
      #}else{
      colls2 <- c(f.id, "Outcome", "Value", "Voc", "Date" )
    #}
    
    TEMP <- TEMP[, ..colls2]
     
    
    dbWriteTable(db, codesheet[j,][["Outcome"]] ,TEMP, overwrite = F, append = T)
    
    rm(TEMP, x, colls2)
    gc()
    }else{print(paste0("0 rows found for ", codesheet[j,][["Voc"]]))}
  
    
  }
  #dbReadTable(tmpdb, "COVID19DX")
  #dbReadTable(tmpdb, "REGION")
  
  
  
}
