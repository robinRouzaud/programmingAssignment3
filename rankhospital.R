rankhospital <- function(state, outcome, num){
      oldWarning <- getOption("warn")
      # Opening the dataset for reading
      fichier <- "outcome-of-care-measures.csv"
      con     <- file(fichier, "r")
      data    <- read.csv(con)
      close(con)
      
      # Are the inputs valid?
      if(sum(as.character(state) == unique(data[, 7])) == 0){
            stop("Invalid state")
      }
      outcomeNames <- c("Heart Attack", "Heart Failure", "Pneumonia")
      if(sum(outcome == outcomeNames) == 0){
            stop("Invalid outcome")
      }
      
      # Subsetting data frame according to state input
      stateDataFrame <- data[as.character(data[,7]) == state, ]
      outcomeCol  <- c(11, 17, 23)
      outcomeCol2 <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") 
      names(outcomeCol2) <- outcomeNames # Matches outcomeNames
      names(outcomeCol)  <- outcomeNames # Matches outcomeNames
      stateOutcome <- stateDataFrame[, outcomeCol[outcome]]
      
      options(warn = -1)
      stateDataFrame[, outcomeCol[outcome]] <- as.numeric(levels(stateDataFrame[, outcomeCol[outcome]]))[stateDataFrame[, outcomeCol[outcome]]]
      options(warn = oldWarning)
      
      oStateDataFrame <- stateDataFrame[order(stateDataFrame[outcomeCol[outcome]]), ]
      oStateDataFrame[num, outcomeCol[outcome]]
      
}