rankall <- function(outcome, num = "Best"){
      oldWarning <- getOption("warn")
      # Opening the dataset for reading
      fichier <- "outcome-of-care-measures.csv"
      con     <- file(fichier, "r")
      data    <- read.csv(con)
      close(con)
      
      # Are the inputs valid?
      outcomeNames <- c("Heart Attack", "Heart Failure", "Pneumonia")
      if(sum(outcome == outcomeNames) == 0){
            stop("Invalid outcome")
      }
      
      state <- sort(unique(data[, 7]))
      stateName <- as.character(levels(state[]))[state[]]

      outcomeCol  <- c(11, 17, 23)
      outcomeCol2 <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") 
      names(outcomeCol2) <- outcomeNames # Matches outcomeNames
      names(outcomeCol)  <- outcomeNames # Matches outcomeNames
      
      # INitializing results dataframe
      results <- c()
      
      for(i in 1:length(state)){
            # Subsetting dataframe according to state input
            stateDataFrame <- data[as.character(data[,7]) == state[i], ]

                              #stateOutcome <- stateDataFrame[, outcomeCol[outcome]]
            
            # Converting factors to numeric or charcters
            options(warn = -1)
            stateDataFrame[, outcomeCol[outcome]] <- as.numeric(levels(stateDataFrame[, outcomeCol[outcome]]))[stateDataFrame[, outcomeCol[outcome]]]
            stateDataFrame[, "Hospital.Name"] <- as.character(levels(stateDataFrame[, "Hospital.Name"]))[stateDataFrame[, "Hospital.Name"]]
            options(warn = oldWarning)
            
            # Ordered dataframe according to the outcome and NAs exclusion
            oStateDataFrame <- stateDataFrame[order(stateDataFrame[outcomeCol[outcome]],
                                                    stateDataFrame["Hospital.Name"]), ]
            totAvail1 <- length(oStateDataFrame[, outcomeCol[outcome]])   # Amount of hospitals available in the state
            NAsXclusion <- is.na(oStateDataFrame[, outcomeCol[outcome]])
            oStateDataFrame <- oStateDataFrame[!NAsXclusion, ]
            
            totAvail2 <- length(oStateDataFrame[, outcomeCol[outcome]])   # Amount of hospitals available with data in the state
            
            if(num == "Best"){
                  results <- rbind(results, c(oStateDataFrame[1, "Hospital.Name"], stateName[i]))
            }else if(num == "Worst"){
                  results <- rbind(results, c(oStateDataFrame[totAvail2, "Hospital.Name"], stateName[i]))
            }else if(is.character(num)){
                  stop("Invalid ranking")
            }else if(num > totAvail1){
                  results <- rbind(results, c(NA, stateName[i]))
            }else if(num > totAvail2){
                  results <- rbind(results, c(NA, stateName[i]))
            }else{
                  results <- rbind(results, c(oStateDataFrame[num, "Hospital.Name"], stateName[i]))
            }
      }
      results <- data.frame(results)
      names(results) <- c("Hospital", "State")
      results
}