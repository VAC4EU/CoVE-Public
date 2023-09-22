
# REG <- unique(readRDS(SCRIPT[["INPUT2"]][["path"]])[,.(person_id, Date, REGION)])
# 
# Data = copy(REG)
# c.id = "person_id"
# c.value = "REGION"
# c.date = "Date"
# start_study_date = start_study_date
# end_study_date = end_study_date




##ID-00000118#



MakeSpells <- function(Data, c.id, c.value, c.date, start_study_date, end_study_date){
        
        check1 <- length(unique(Data[[c.id]]))
        id1 <- unique(Data[[c.id]])
        
        setnames(Data, c(c.id, c.value, c.date), c("id", "value", "date"))
        
        #Work only with start dates by removal of repeating values in time 
        #setorder(Data, id, date)
        #Data[, seq := seq_along(.N), by = c("id", "value")]
        #Data <- Data[seq == 1,]
        

        #Remove double dates with different region
        setorder(Data, id, -date)
        Data[, seq := seq_len(.N), by = c("id", "date")]
        invalid <- unique(Data[seq > 1,][["id"]])
        Data <- Data[seq == 1,]
        if(length(invalid) > 0) print(paste0(paste0(invalid, collapse = ","), " have more values on 1 date. 1 observations for this date are removed"))
        #check1 <- check1 - length(invalid)
        rm(invalid)
        
        #Determine start and end point
        setorder(Data, id, date)
        Data[, Date1 := shift(date, -1), by =  "id"]
        Data[, seq := seq_len(.N), by = "id"]
        Data[, N := .N, by = "id"]
        Data[, ST := date]
        Data[, EN := Date1 - 1]
        Data[is.na(EN), EN := end_study_date]
        
        #Extend/inpute range
        Data[seq == 1 & ST > start_study_date, ST := start_study_date]
        Data[seq == .N & EN < end_study_date, EN := end_study_date ]
        
        
        #Removal of information out off scope of study time
        Data <- Data[!(EN < start_study_date |  ST > end_study_date) ,]
        
        
        #Do checks
        Data[, test := EN - ST]
        check2 <- length(unique(Data$id))
        id2 <- unique(Data[["id"]])
        
        if(check1 != check2) warning(paste0(i,": Unequal subjects probably because NA in Date. Missing ", paste0(id1[!id1 %in% id2], collapse = ", ")))
        if(any(Data$test < 0)) warning(paste0(i, ": Spells not correct. Check spells for: ", paste0(Data[test < 0,][["id"]], collapse = ", ")))
        
      
        setnames(Data, c("id", "value", "date"), c(c.id, c.value, c.date))
        

        return(Data[, c(c.id, "ST", "EN", "seq", c.value), with = F])


}



  