



GetStatisticalOutcomes <- function(
        dataset,
        timeStart, #Default NULL
        timeEnd, #Default NULL
        c.exposed,
        c.strata = NULL, # A vector of column names 
        v.strata = NULL, # A vector of column values with the same length as c.strata
        d.t0,
        d.outcome,
        d.cencoring,
        id,
        id.pair = NULL, 
        IPTW_weights = NULL,
        
        #extra option, not tested/last moment implementation
        dateStart = NULL,
        dateEnd = NULL,
        
        #Extra options if timeStart and timeEnd are filled
        datesCheck = "first_free_covid",
        completePairs = T
  ){

    #Strip dataset from not needed elements
    #Select needed columns
    dataset <- copy(dataset[, c(id, id.pair, c.exposed, c.strata, d.t0, d.outcome, d.cencoring, IPTW_weights, datesCheck), with = F])
    
    #Select strata, my assumption is that only those rows are needed given these parameters, please confirm.
    if(!is.null(v.strata)){
  
      #Create expression for stratification based on the columns and his values
      exprs <- paste0(paste0(c.strata, " == '", v.strata,"'"), collapse = " & ")
      
      #Execute the filter based on the expression
      dataset <- dataset[eval(parse(text = exprs )),]
      rm(exprs)
      
    }
    
    #Implement start end end dates
    ###
    if(!is.null(dateStart)){
      dataset <-  dataset[, st := dateStart][, en := dateEnd]
      dataset <-  dataset[(get(d.cencoring) >= st & get(d.t0) <= en),]
      dataset <-  dataset[, eval(d.t0) := pmax(get(d.t0), st)][, eval(d.cencoring) := pmin(get(d.cencoring), en)][, st := NULL][, en := NULL]
      dataset <- dataset[is.na(get(d.outcome)) | !get(d.outcome) < date_start, ]
      #print("Not tested")
    }
    ###
    
    
    ###Determine the days. This is redone here for the reason to get clear what happens
    #Moreover, I am coming from SAS and not accustomed in using vectors, therefore I first want to first create a data frame. 
    
    dataset <- dataset[data.table::between(get(d.outcome), get(d.t0), get(d.cencoring))  , days_to_outcome := get(d.outcome) - get(d.t0) + 1]
    dataset <- dataset[, days_to_cencoring := get(d.cencoring) - get(d.t0) + 1]
    dataset <- dataset[, outcome_boolean := fifelse(is.na(days_to_outcome), 0,1)]
    
    
    #Now we are going to define the time frames if wanted
    ###
    if(!is.null(timeStart)){
      
      #Create start date for new interval
      dataset <- dataset[, date_interval_start := get(d.t0) + timeStart]
      
      #Remove all inrelavant cases that had an event before the start date or who where cencored before the start date.
      dataset <- dataset[get(d.cencoring) >= date_interval_start,]
      dataset <- dataset[is.na(get(d.outcome)) | !get(d.outcome) < date_interval_start, ]
      
      #If you want to exclude subjects based on other dates before the new start date then they are removed
      if(!is.null(datesCheck)) for(x in datesCheck){dataset <- dataset[is.na(get(x)) | !get(x) < date_interval_start, ]}
      
      #Now only 1 of the pair can be removed. To keep balance it is possible to keep omnly pairs with both subjects still in the dataset
      if(completePairs) dataset <- dataset[get(id.pair) %in% dataset[[id.pair]][duplicated(dataset[[id.pair]])],]
      
      #Set the end date of the new time interval based on the end days and the censoring date. Take the minimum.
      dataset <- dataset[, date_interval_end := pmin((get(d.t0) + timeEnd), get(d.cencoring))]
      
      dataset <- dataset[data.table::between(get(d.outcome), date_interval_start, date_interval_end), days_to_outcome2 := get(d.outcome) - date_interval_start + 1]
      
      #first set censoring
      #When original cencoring is within the timeframe then the followup will be shorter the end - start interval.
      dataset <- dataset[, days_to_cencoring2 :=  date_interval_end - date_interval_start + 1]
      
      
      #Filter out the subjects that did already had the outcome in earlier time bins
      ###
      
      
      ###
      dataset <- dataset[, outcome_boolean2 := fifelse(is.na(days_to_outcome2), 0,1)]
      
       
    }
    
    
    ###
    
    #Temp step to remove later. Distinct between with or without time frame to retrieve 1 input dataframe in both situations
    ###
    if(is.null(timeStart)){
      #Now only 1 of the pair can be removed. To keep balance it is possible to keep omnly pairs with both subjects still in the dataset
      if(completePairs) dataset <- dataset[get(id.pair) %in% dataset[[id.pair]][duplicated(dataset[[id.pair]])],]
      dataset <- dataset[, c(id , id.pair,"exposed", "outcome_boolean", "days_to_outcome", "days_to_cencoring", IPTW_weights), with = F]
     
       
    }else{
      
      #Deleting rows where subjects not are followed within the period to interval
      dataset <- dataset[date_final_censoring > date_interval_start,]
      if(any(is.na(dataset$days_to_cencoring2))) stop("subjects witout a cencoring date")
      
      dataset <-dataset[, c(id, id.pair, "exposed", "outcome_boolean2", "days_to_outcome2", "days_to_cencoring2", IPTW_weights), with = F]
      setnames(dataset, c("outcome_boolean2", "days_to_outcome2", "days_to_cencoring2"), c("outcome_boolean", "days_to_outcome", "days_to_cencoring"))
      
      
      
    }
    
    #Check file that is used for cox regression on NA and negative numbers.
    if(
    any(dataset$days_to_cencoring < 0) | 
    any(dataset$days_outcome < 0) |
    any(is.na(dataset[,colnames(dataset)[!colnames(dataset) %in% "days_to_outcome"], with = F]))
    ){
      stop("NA or negative numbers in file used for cox regression")
    }
    
    ###
    
    #Step 4. check functions from Dorieke and retrieve needed code. To check for Dorieke if the inputs stored in dataset are correct and meeting demands 
    ###
    
    #Retrieve counts by using parts of n_covid_cases_per_vaccine function. ho. becomes exp. and he. becomes con.  
    
    #Original lines of code
    #he.numcovid <- n_covid_cases_per_vaccine(df, df$first_covid_after_md_date, r$type_vax, FALSE)
    #ho.persondays <- calc_pers_days(df$follow_up_time_any_covid, r$type_vax, TRUE)
    
    exp.numcovid <- nrow(filter(dataset, exposed == 1 & outcome_boolean == 1))
    con.numcovid <- nrow(filter(dataset, exposed == 0 & outcome_boolean == 1))
    
    #Retrieve persontime
    #Original lines of codes
    #he.persondays <- calc_pers_days(df$follow_up_time_any_covid, r$type_vax, FALSE)
    #ho.persondays <- calc_pers_days(df$follow_up_time_any_covid, r$type_vax, TRUE)
    #Needed function to retrieve the input is follow_up_time
    
    dataset$persondays <- ifelse (is.na(dataset[["days_to_outcome"]]),
                                  dataset[["days_to_cencoring"]],
                                  dataset[["days_to_outcome"]])
    
    exp.persondays <- sum(dataset[exposed == 1,][["persondays"]])
    con.persondays <- sum(dataset[exposed == 0,][["persondays"]])
    
    
    
    
    
    #The function bin_outcome, is that still needed since we start with 1/0
    
    #Run the model
    #Original lines of code
    #cr.HR <- A.RR(df$follow_up_time_any_covid, as.numeric(bin_outcome), df$exposed_or_comparator, adjusted = FALSE, df = df)
    #hr.adj <- A.RR(df$follow_up_time_any_covid, as.numeric(bin_outcome), df$exposed_or_comparator, adjusted = TRUE, df = df) # needs adjusting for confounding
    #ve.adj <- VE(hr.adj)
    
    ###
    #Function deliverd by Dorieke to do cox regression. It checks it subjects are duplicated and it is possible to add weights coming from inverse probability weigting
    A.RR <- function(fup, aesi, group, weights = NULL, df, person, pair = NULL){
      
      if (!any(duplicated(df[[person]]))){
        if (is.null(weights)){
          rr <- coxph(Surv(fup, aesi) ~ group, robust = T, data = df) %>%
            summary()
          fit <- NULL
          
        } else{
          rr <- coxph(Surv(fup, aesi) ~ group, robust = T, weights = weights, data = df) %>%
            summary()
          fit <- survfit(Surv(fup, aesi) ~ group, weights = weights, data = df)
          
          
        }
        c(rr$table_body$estimate[3],rr$table_body$conf.low[3],rr$table_body$conf.high[3])
      }
      if (any(duplicated(df[[person]]))){
        if (is.null(weights)){
          rr <- coxph(Surv(fup, aesi) ~ group, cluster = pair_id, robust=T, data=df) %>%
            summary()
          fit <- NULL
          
        } else{
          rr <- coxph(Surv(fup, aesi) ~ group, cluster = pair_id, robust=T, weights = weights, data = df) %>%
            summary()
          fit <- survfit(Surv(fup, aesi) ~ group, weights = weights, data = df)
          
          
          
        }
        
        
        
        
      }
      
      if(any(is.na(c(rr$conf.int[1],rr$conf.int[3],rr$conf.int[4])))){return(list(hr = c(0,0,0)))}else{
      #if(any(is.na(fit))){return(0)}else{
      #return(as.numeric(format(round(c(rr$conf.int[1],rr$conf.int[3],rr$conf.int[4]), 2), nsmall = 2)))}
      return(list(fit = fit, hr = as.numeric(format(round(c(rr$conf.int[1],rr$conf.int[3],rr$conf.int[4]), 2), nsmall = 2))))}
    }
    
    
    #Execute the function 
    
    if(nrow(dataset) > 0){
    
    if(any(dataset[["exposed"]] == 1) & any(dataset[["exposed"]] == 0)){  
    cr.HR <- A.RR(fup = dataset$persondays, 
                  aesi = dataset$outcome_boolean, 
                  group = dataset$exposed,  #ifelse(dataset$exposed == 1, "exposed", "comparator")
                  df = dataset,
                  person = id,
                  pair = id.pair
                  )$hr
    
    hr.tmp <- A.RR(fup = dataset$persondays, 
                  aesi = dataset$outcome_boolean, 
                  group = dataset$exposed,
                  df = dataset,
                  weights = dataset[[IPTW_weights]],
                  person = id,
                  pair = id.pair
    )
    
    hr.adj <- hr.tmp$hr
    if(exists("fit", where = hr.tmp)){hr.graph <- hr.tmp$fit
    }else{(hr.graph <- NULL)}
    
    
    }else{
      warning("Only subjects from 1 group (exposed/control) in the dataset. 0's are returned for cox regression")
      cr.HR <- c(0,0,0)
      hr.adj <- c(0,0,0)
      hr.graph <- NULL
    }
    
    VE <- function(df_variable){
      ve <- (1-df_variable)
      ve <- as.numeric(format(round(ve, 2), nsmall = 2))
      return(ve)
    }
    
    ve.adj <- VE(hr.adj)
    
    
    
    
    
    ###
    
    IR <- function(n.persons, n.events, n.fupyrs, scale_IR, repeatedMeasures = FALSE) {
      if (repeatedMeasures == FALSE) {
        IR <- n.events / n.fupyrs
      } else {
        ln.fupyrs <- log(n.fupyrs / 365.25)
        if (!any(n.events != 0)) { ## geem() will fail when there are 0 events.
          IR <- IR.lb <- IR.ub <- NA
        }
        if (any(n.events != 0)) {
          gee_poisson <- geem(n.events ~ 1 + offset(ln.fupyrs), id = n.persons, family = "poisson") # , data = d_U2)
          U_logIR <- as.numeric(coef(gee_poisson))
          IR <- exp(U_logIR)
          IR.lb <- exp(as.numeric(U_logIR + qnorm(0.025) * summary(gee_poisson)$se.robust))
          IR.ub <- exp(as.numeric(U_logIR + qnorm(0.975) * summary(gee_poisson)$se.robust))
        }
      }
      rsk <- scale_IR * IR
      rsk <- as.numeric(format(round(rsk, 2), nsmall = 2))
    }
    
    
    
    ###
    
    
    
    #Distinct between only stratifying on category or doing analyses in different time bins
    ###
      
    
    
      
      
    #General
    ###
    con.IRnum <- IR(person_id, con.numcovid, con.persondays, 100000)
    exp.IRnum <- IR(person_id, exp.numcovid, exp.persondays, 100000)
    
    rdnum <- ratedifference(exp.numcovid, con.numcovid,
                            exp.persondays, con.persondays,CRC=FALSE, conf.level = 0.95)$estimate
    
    
    rdnum <- format(rdnum, digits=3)
    
    rd.CI <- ratedifference(exp.numcovid, con.numcovid,
                            exp.persondays, con.persondays,CRC=TRUE, conf.level = 0.95)$conf.int
    
    rd.CI <- format(rd.CI, digits=3)
    
    ve.percent <- percent(ve.adj)
    
    # glm_poisson <- glm(aesi ~ group + offset(log(fup + 1)), weights = weights, family = poisson, data = df)
    # U_logIRR <- as.numeric(coef(glm_poisson)[2])
    # IRR <- exp(U_logIRR)
    # IRR.lb <- exp(as.numeric(U_logIRR + qnorm(0.025) * summary(glm_poisson)$coefficients[2]))
    # IRR.ub <- exp(as.numeric(U_logIRR + qnorm(0.975) * summary(glm_poisson)$coefficients[2]))
    # format(round(IRR, 2), nsmall = 2)
    # format(round(IRR.lb, 2), nsmall = 2)
    # format(round(IRR.ub, 2), nsmall = 2)
    
   #Assumes no duplicate controls 
    
    
    
    
    
return(list(graph = hr.graph, table = c(con.persondays, con.numcovid, con.IRnum, exp.persondays, exp.numcovid, exp.IRnum, rdnum, rd.CI, cr.HR, hr.adj, ve.percent)))
    }else{return(list(table = rep(0 , 18)))}
    
    
}

# GetStatisticalOutcomes(
#       dataset = df[matched == T,],
#       timeStart = 30,
#       timeEnd = 59, 
#       c.exposed = "exposed",
#       c.strata = "type_vax1",
#       v.strata = "PFIZER",
#       d.t0 = "date_start",
#       d.outcome = "first_covid_sev1", 
#       d.cencoring = "date_final_censoring",
#       id = "person_id",
#       IPTW_weights = "IPTW_full_cor_trim" 
# )



