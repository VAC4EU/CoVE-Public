

GetConceptsProcedure <- function(t.interest, additional, expr = NULL, concepts, dir.codes, dir.data, standardized.cols = T){

      scheme <- list(
        
        "VACCINES" = list(
          col1 = c("person_id","vx_record_date","vx_admin_date","vx_atc","vx_type", "vx_dose","vx_manufacturer"),
          col2 = c("person_id","vx_record_date","vx_admin_date","vx_atc","vx_type", "vx_dose","vx_manufacturer"),
          Date = c("vx_record_date","vx_admin_date"),
          Filter = expr
        )
        
        ,
        
  
                
        "MEDICINES" = list(
          col1 = c("person_id",	"medicinal_product_atc_code","date_dispensing","date_prescription"),
          col2 = c("person_id","Code","date_dispensing","date_prescription"),
          Date = c("date_dispensing","date_prescription"),
          Filter = expr
        )
        
        ,
        "EVENTS" = list(
          col1 = c("person_id", "start_date_record","event_code","event_record_vocabulary", "event_free_text"),
          col2 = c("person_id","Date","Code","CodingSystem", "Free_text"),
          Date = c("Date"),
          Filter = expr
        
        )
      )
      
      scheme <- scheme[names(scheme) %in% t.interest]
      
      for(j in names(scheme)){
        
        if(j == "EVENTS"){
          FILE <- readRDS(file = paste0(dir.codes, "CODES_EVENTS.rds"))[toupper(Outcome) %in% concepts, ]
          setnames(FILE,c("coding_system","code"),c("CodeSystem","Code"))
        }
        
        if(j == "MEDICINES"){
          FILE <- readRDS(file = paste0(dir.codes, "CODES_MEDICINES.rds"))[toupper(Outcome) %in% concepts, ]
        }
        
        if(j == "VACCINES"){
          FILE <- readRDS(file = paste0(dir.codes, "CODES_VACCINES.rds"))[toupper(Outcome) %in% concepts, ]
        }
        
        files <- list.files(dir.data, pattern = j)
        
        for(i in files){
          #browser()
          
          print(paste0("File ",i," is imported"))
          TEMP <- ImportPattern(
            append = F,
            dir = dir.data, 
            pat = i,
            colls = scheme[[j]][["col1"]],
            colls.new = scheme[[j]][["col2"]], 
            date.colls = scheme[[j]][["Date"]],
            if(!is.null(scheme[[j]][["Filter"]])) exprss = scheme[[j]][["Filter"]]
          )
          
          if(j == "VACCINES" & standardized.cols) TEMP <- TEMP[,Date := fifelse(is.na(vx_admin_date),vx_record_date,vx_admin_date)][,vx_admin_date := NULL][,vx_record_date := NULL]
          if(j == "VACCINES") TEMP <- TEMP[, Code := fifelse(is.na(vx_atc) | vx_atc == "", vx_type, vx_atc)]
          if(j == "VACCINES") TEMP <- TEMP[, CodingSystem := fifelse(is.na(vx_atc) | vx_atc == "", "vx_type", "vx_atc")]
          
          if(j == "MEDICINES"){TEMP <- TEMP[, CodingSystem := "ATC"]}
          if(j == "MEDICINES" & standardized.cols){TEMP <- TEMP[,Date := fifelse(is.na(date_dispensing),date_prescription,date_dispensing)][,date_dispensing := NULL][,date_prescription := NULL]}
          
          if(j == "EVENTS" & DAP == "PHARMO"){TEMP <- TEMP[CodingSystem == "Free_text", Code := Free_text ]}
          if(j == "EVENTS") TEMP <- TEMP[, Free_text := NULL]
          
          #Clean rows with no valid date
          ###
          if(standardized.cols){
          check1 <- nrow(TEMP)
          TEMP <- TEMP[!is.na(Date),]
          check2 <- nrow(TEMP)
          
          if(check1 != check2) print(paste0("In file ", i," ", check1 - check2, " row(s) with a NA for date are found. These rows are excluded"))
          rm(check1, check2)
          ###
          }
          
          CreateConceptDatasets(
            codesheet = FILE,
            c.voc = "CodeSystem",
            c.concept = "Outcome",
            c.codes = "Code",
            file = TEMP,
            f.code = "Code",
            f.voc =  "CodingSystem",
            c.startwith = start_with_colls,
            method = "loop",
            db = mydb,
            group = T,
            f.name = i,
            reduce_size = standardized.cols
          )
          
          rm(TEMP)
          gc()
          
        }
        rm(FILE, files)
        
      }
      
      
      #Importing DAP specific concepts.
      ###
      
      if(additional){
                  
                  FILE <- readRDS(file = paste0(dir.codes, "CODES_ADDITIONAL.rds"))[Outcome %in% concepts,]
                  
                  if(nrow(FILE) > 0){
                    scheme <- unique(FILE[["table"]])
                    
                    needed_colls <- unique(c(colnames(FILE)[substr(colnames(FILE),1,3) == "col"], "date_column", "keep"))
                  
                    for(j in 1:length(scheme)){
                      
                      files <- list.files(dir.data, pattern = scheme[j])
                      
                      #if(length(files) > 0){
                      FILE_TEMP <- FILE[table == scheme[j] ,]
                      
                      for(i in files){
                        print(paste0("File ",i," is imported"))
                        TEMP <- ImportPattern(
                          dir = dir.data, 
                          pat = i,
                          colls = na.omit(unique(c("person_id", unique(do.call(list.append, lapply(needed_colls, function(x) unique(FILE_TEMP[!is.na(x)][[x]]) )))))),
                          date.colls = unique(FILE_TEMP[["date_column"]]),
                          if(!is.null(expr)) exprss = expr,
                          append = F
                        )
                        
                        #Clean rows with no valid date
                        ###
                        check1 <- nrow(TEMP)
                        TEMP <- TEMP[!is.na(get(unique(FILE_TEMP[["date_column"]]))),]
                        check2 <- nrow(TEMP)
                        
                        if(check1 != check2) print(paste0("In file ", i," ", check1 - check2, " row(s) with a NA for date are found. These rows are excluded"))
                        rm(check1, check2)
                        
                        ###
                        
                        
                        CreateConceptDatasetsMultipleVars(
                          codesheet = FILE_TEMP,
                          file = TEMP,
                          f.id = "person_id",
                          c.columns = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "col"]),
                          c.values = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "val"]),
                          c.date = "date_column",
                          c.outcome = "Outcome",
                          c.keep = "keep",
                          c.keepnames = "Outcome",
                          db = mydb
                        ) 
                        
                        
                        rm(TEMP)
                        gc()
                        
                      }
                      
                      rm(files, FILE_TEMP)
                      gc()
                      }
                    }              
                  #}            
                  rm(FILE)   
                  gc()
      
      
      }

}
