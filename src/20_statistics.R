
#if(file.exists(paste0(d3Dir, "statistics.db"))) file.remove(paste0(d3Dir, "statistics.db"))



for(populationName in c("HO_P_TO_HE_P", "HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB", "ANY_P_with_prior_covid_B_to_NB", "children")){
  
  if(file.exists(paste0(d3Dir,"D3_",populationName,"_analytic_dataset.rds"))){ 
    
    #if(nrow(readRDS(paste0(d3Dir,"D3_",populationName,"_analytic_dataset.rds"))) > 0){
    
    #mydb <- dbConnect(RSQLite::SQLite(), paste0(d3Dir, "statistics.db"))
    
    print(populationName)
    system.time(source(paste0(sourceDir, "statistical_procedure.R")))
    
    
    if(file.exists(paste0(outputDir,paste0("df_",populationName,"_new.test.csv")))){
    tmp <- fread(paste0(outputDir,paste0("df_",populationName,"_new.test.csv")), colClasses = "character")
    #if(!dir.exists(paste0(outputDir, "masked_results"))) dir.create(paste0(outputDir, "masked_results"))
    lapply(c("Control cases of covid", "Exposed cases of covid"), function(x) tmp[as.numeric(get(x)) < 5, eval(x) := "<5" ] )
    
      for(j in unique(tmp[["agegroup"]])){
        fwrite(tmp[agegroup == j,], paste0(outputDir,"/export/df_",populationName,"_",j,"_masked_results.csv"), sep = ";")
      }
    }
    
    rm(populationName)
    
    #dbDisconnect(mydb)
    #}
  }
}
