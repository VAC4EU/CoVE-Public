

#type = "lookback", "NATF", "NA0"

MakeBooleanCovariates <- function(data, cols, type = "lookback", lookback = NULL, date.ref) {
  
  if(type == "lookback"){ 
  if(length(cols) != length(lookback)) stop("check input vectors are of the same length")
  }
    
  for(i in 1:length(cols)){
    data <- copy(data)
    col <- cols[i]
    
    if(col %in% colnames(data)){
      
      if(type == "lookback"){ 
        data <- data[, tmp := fifelse((get(date.ref) - get(col)) < lookback[i], 1, 0, na = 0 )]
      }
      
      if(type == "NATF"){ 
        data <- data[, tmp := fifelse(is.na(get(col)), 0, 1)]
      }
      
      if(type == "NA0"){ 
        data <- data[, tmp := fifelse(is.na(get(col)), 0, as.numeric(get(col)))]
      }
      
      data <- data[, eval(col) := NULL]
      
      setnames(data, "tmp", col)
      
    }else{print(paste0(col, " not in table"))}
  }
  return(data)
  
}







