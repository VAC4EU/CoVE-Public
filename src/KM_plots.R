
#populationName <- "HO_P_HE_B_to_NB"
for(populationName in c("HO_P_TO_HE_P", "HO_P_HO_B_to_NB", "HO_P_HE_B_to_NB", "HE_P_B_to_NB", "ANY_P_with_prior_covid_B_to_NB", "children")){
  if(file.exists(paste0(outputDir, populationName,"_graph.rds"))){
    graph.object <- readRDS(paste0(outputDir, populationName,"_graph.rds"))
    
    new.graph.objects <-  list()
    
    for(object in graph.object){
      
      if(!is.null(object[["graph.object"]])){
        
        if(object[["graph.name"]] == "Overall" |
           object[["graph.name"]] == "1st dose PFIZER" |
           object[["graph.name"]] == "1st dose MODERNA" |
           object[["graph.name"]] == "1st dose ASTRAZENECA"){
          
          new.graph.objects <- append(new.graph.objects, list(list(survfit = object[["graph.object"]], 
                                                              outcome = object[["graph.outcome"]], 
                                                              agegroup = object[["graph.agegroup"]])))
        }
      }
    }
    
    new.graph.objects <- list.group(new.graph.objects, outcome, agegroup)
    
    colors <- c("black", "red", "blue", "green")
    subtitle <- ""
    subset <- ""
    
    for(result in new.graph.objects){
    
      for(category in result){
        subtitle <- category[[1]][["outcome"]]
        subset <- category[[1]][["agegroup"]]
        
        png(file = paste0(outputDir, populationName,"_graph_", subtitle, "_", subset,".png"), 
            width     = 10,
            height    = 10,
            units     = "cm",
            res       = 500,
            pointsize = 8
            )
    
        plot(-1,-1, ylim = c(0,1), xlim = c(0,350),
             main = c(populationName, subset, "KM curves"),
             xlab = "Survival Time In Days",
             ylab = "Survival Probabilities",
             sub = subtitle)
        
        for(index in 1:length(category)){
          lines(
            category[[index]][["survfit"]],
            lty = c("solid", "dashed"),
            col = colors[index%%5]
          )
        }

        legend("bottomright", c("Overall control", "Overall exposed",
                               "Pfizer control", "Pfizer exposed",
                               "Moderna control", "Moderna exposed",
                               "Astrazeneca control", "Astrazeneca exposed"),
               lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed" ,"solid", "dashed"),
               col = c("black", "black", "red", "red", "blue", "blue", "green", "green"),
               cex = 0.8)
        dev.off()
      }
    }
  }
}
