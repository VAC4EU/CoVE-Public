GetDayDif <- function(data, date.ref, dates, suffix = "", postfix = "", keep = T){
  
  data <- copy(data)
  
  for(i in 1:length(dates)){
    
    
    
    var <- dates[i]
    if(var %in% colnames(data)){
      
      data <- data[ , paste0(suffix, eval(var), postfix) := as.integer(get(var) - get(date.ref))  ]
      if(!keep) data <- data[ , eval(var) := NULL  ]
    }else{print(paste0(col, " not in database"))}
  }
  
  return(data)
  
}