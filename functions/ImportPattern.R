ImportPattern <- function(pat,dir, colls = NULL, colls.new = NULL, exprss = NULL, date.colls = NULL, append = T){
  
  #Find all files that contain the pat 
  obs_files<-list.files(dir, pattern = pat)
  
  #If more than 1 file is found while append is F then stop the function
  if(!append & length(obs_files) > 1){stop("Several files meet pattern while append is FALSE")}
  
  #Import every file and append loop wise. If no files found return NUL
  if(length(obs_files) > 0){
    
    for(i in 1:length(obs_files)){
      
      #Import csv
      if(!is.null(colls)){TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, select = colls, na.strings = c("", NA), colClasses=c("character"))}else{
        TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, na.strings = c("" , NA), colClasses=c("character"))
      }
      
      #if(!is.null(colls)){ TEMP <- TEMP[, ..colls] }
      
      #Rename
      if(!is.null(colls) & !is.null(colls.new)){setnames(TEMP, eval(colls),eval(colls.new))}
      
      #Add correction for spaces to prevent misinterpretation of NA's and avoiding leading and tailing spaced. 
      invisible(lapply(colnames(TEMP), function(x) TEMP <- TEMP[, eval(x) := trimws(get(x), "b", whitespace = "[\\h\\v]")]))
      TEMP[TEMP == ""] <- NA
      
      
      #Set specified columns in data.colls to date format
      if(!is.null(date.colls)){lapply(date.colls, function (x)  TEMP[, eval(x) := as.Date(get(x),"%Y%m%d") ])}
      
      #Apply the subsetting with the specified exprss
      if(!is.null(exprss)){
        
        rem1 <- nrow(TEMP) 
        TEMP <- TEMP[eval(exprss),]
        rem2 <- nrow(TEMP)
        print(paste0(rem2," selected rows from ",rem1," rows after evaluation of exprss"))
      }
      
      #Append the new file to the files imported prior
      if(i == 1) {FILE <- TEMP}
      if(i > 1) {FILE <- rbindlist(list(TEMP,FILE),fill = T, use.names = T)}
      rm(TEMP)
      gc()
      
      #test integer performence when all person id's are in integer
      #, set_to_integer = "person_id", path = tmp
      #########
      #if(!is.null(set_to_integer)){
      #   if(set_to_integer %in%  colnames(FILE)){
      #     Dic <- paste0(path,"/dic.rds")
      #     
      #     if(file.exists(Dic)){
      #       FILE <- RenameId(FILE, Dictionary = readRDS(Dic), colls = set_to_integer, AddId = T)
      #     } 
      #     
      #     if(!file.exists(Dic)){
      #     TEMP <- SetToInteger(FILE, colls = set_to_integer)
      #     FILE <- TEMP$Data
      #     saveRDS(TEMP$Dictionary, Dic)
      #     rm(TEMP)
      #     gc()
      #     }
      # } 
      # }
      #########
      
    }
  }else FILE <- NULL 
  
  return(FILE)
  rm(FILE,obs_files)
  gc()
}
