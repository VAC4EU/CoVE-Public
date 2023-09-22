


CalculateBMI <- function(
  file,
  Result = "Result",
  Date = "Date",
  
  filevoc,
  Voc = "Voc",
  
  #colls both files
  id = c("person_id", "id"),
  StudyVar = "col",
  
  weight.var,
  height.var,
  bmi.var,
  fun = "max",
  unitprefix = "unit."
  
  
  
){
  
  #browser()
  
  #Standardize weight component to kg. 
  #WEIGHT1 <- copy(file)[ grep(paste0(c(weight.var), collapse = "|"), get(StudyVar)),]
  WEIGHT1 <- copy(file)[get(StudyVar) == weight.var,]
  #WEIGHT2 <- copy(filevoc)[ grep(paste0(c(weight.var), collapse = "|"), get(StudyVar)),]
  WEIGHT2 <- copy(filevoc)[get(StudyVar) == weight.var,]
  
  WEIGHT <- merge(x = WEIGHT1, y = WEIGHT2, by = c(id, StudyVar))[, Voc := toupper(gsub(" ","",eval(Voc)))]
  rm(WEIGHT1, WEIGHT2)
  gc()
  
  WEIGHT <- WEIGHT[ grep(toupper(paste0(unitprefix , c("kg", "kilogram","kilograms", "kilogramme", "kilo"), collapse = "|")), Voc), unit := "kg"]
  WEIGHT <- WEIGHT[ grep(toupper(paste0(unitprefix , c("g", "gram","grams", "gramme"), collapse = "|")), Voc), unit := "g"]
  
  if(any(is.na(WEIGHT[["unit"]]))) print("Missing units for WEIGHT")
  WEIGHT <- WEIGHT[!is.na(unit),]
  
  WEIGHT <- WEIGHT[unit == "kg", WEIGHT := as.numeric(get(Result))]
  WEIGHT <- WEIGHT[unit == "g", WEIGHT := as.numeric(get(Result))/1000][, WEIGHT_DT := eval(Date)][, c(id, "WEIGHT", "WEIGHT_DT", StudyVar), with = F]
  
  #Standardize Height component to m. 
  #HEIGHT1 <- copy(file)[ grep(paste0(c(height.var), collapse = "|"), get(StudyVar)),]
  HEIGHT1 <- copy(file)[get(StudyVar) == height.var,]
  #HEIGHT2 <- copy(filevoc)[ grep(paste0(c(height.var), collapse = "|"), get(StudyVar)),]
  HEIGHT2 <- copy(filevoc)[get(StudyVar) == height.var,]
  
  HEIGHT <- merge(x = HEIGHT1, y = HEIGHT2, by = c(id, StudyVar))[, Voc := toupper(gsub(" ","",eval(Voc)))]
  rm(HEIGHT1, HEIGHT2)
  gc()
  
  HEIGHT <- HEIGHT[ grep(toupper(paste0(unitprefix , c("m", "meter","meters", "metre"), collapse = "|")), Voc), unit := "m"]
  HEIGHT <- HEIGHT[ grep(toupper(paste0(unitprefix , c("cm", "centimeter","centimetre"), collapse = "|")), Voc), unit := "cm"]
  if(any(is.na(HEIGHT[["unit"]]))) print("Missing units for HEIGHT")
  HEIGHT <- HEIGHT[!is.na(unit),]
  
  HEIGHT <- HEIGHT[unit == "m", HEIGHT := as.numeric(get(Result))]
  HEIGHT <- HEIGHT[unit == "cm", HEIGHT := as.numeric(get(Result))/100][, HEIGHT_DT := eval(Date)][, c(id, "HEIGHT", "HEIGHT_DT", StudyVar), with = F]
  
  #Calculate BMI
  BMI2 <- merge(x = WEIGHT, y = HEIGHT, by = c(id), all = F)
  rm(WEIGHT, HEIGHT)
  gc()
  
  #Make calculation and get the latest date in time as date for the calculated BMI
  BMI2 <- BMI2[, Result := WEIGHT/(HEIGHT^2)][, Date := pmax(WEIGHT_DT, HEIGHT_DT)][, c(id, "Result", "Date" ), with = F]
  
  #Add directly available BMI
  #BMI11 <- copy(file)[ grep(paste0(c(bmi.var), collapse = "|"), get(StudyVar)),]
  BMI11 <- copy(file)[get(StudyVar) == bmi.var,]
  #BMI12 <- copy(filevoc)[ grep(paste0(c(bmi.var), collapse = "|"), get(StudyVar)),]
  BMI12 <- copy(filevoc)[get(StudyVar) == bmi.var,]
  
  BMI1 <- merge(x = BMI11, y = BMI12, by = c(id, StudyVar))[, Voc := toupper(gsub(" ","",eval(Voc)))]
  rm(BMI11, BMI12)
  gc()
  
  BMI1 <- BMI1[ grep(toupper(paste0(unitprefix , c("kg/m2", "kgperm2", "kg/m\\^2"), collapse = "|")), Voc), unit := "kg/m2"]
  if(any(is.na(BMI1[["unit"]]))) print("Missing units for BMI")
  BMI1 <- BMI1[!is.na(unit),]
  
  BMI1 <- BMI1[unit == "kg/m2",  Result := round(as.numeric(get(Result)), digits = 0)][, Date := eval(Date)][, c(id, "Result", "Date" ), with = F]
  
  BMI <- rbindlist(list(BMI1, BMI2), use.names = T, fill = F)
  
  #Take the bmi latest in time
  setorderv(BMI, c(id, "Date"), order = c(rep(1, length(id)),-1))
  BMI <- BMI[, order := seq_len(.N), by = c(id)][order == 1,][, order := NULL]
  
  return(BMI)
  
  
}
