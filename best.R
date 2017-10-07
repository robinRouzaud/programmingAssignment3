best <- function(state, outcome){
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
      
      # Calculating the minimum according to outcome input
      outcomeCol <- c(11, 17, 23) # Matches outcomeNames
      names(outcomeCol) <- outcomeNames
      stateOutcome <- stateDataFrame[, outcomeCol[outcome]]
      options(warn = -1)
      stateOutcome <- as.numeric(levels(stateOutcome))[stateOutcome]
      options(warn = oldWarning)
      lowest <- min(stateOutcome, na.rm = TRUE)
      
      # Convering factors to numerics
      options(warn = -1)
      stateDataFrame[, outcomeCol[outcome]] <- as.numeric(levels(stateDataFrame[, outcomeCol[outcome]]))[stateDataFrame[, outcomeCol[outcome]]]
      options(warn = oldWarning)
      
      # Testing tied values
      selection <- stateDataFrame[, outcomeCol[outcome]] == lowest
      tiesTest <- sum(selection, na.rm = TRUE)
      selection[is.na(selection)] <- FALSE
      selection
      if(tiesTest == 1){
            hospital <- as.character(levels(stateDataFrame[selection, 2]))[stateDataFrame[selection, 2]]
            return(hospital)
      }else if(tiesTest > 1){
            hospitals <- as.character(levels(stateDataFrame[selection, 2]))[stateDataFrame[selection, 2]]
            sort(hospitals)
            print("There's a tie")
            return(hospitals[1])
      }
}




