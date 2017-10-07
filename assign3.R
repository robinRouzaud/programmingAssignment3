best <- function(state, outcome){
      # Opening the dataset for reading
      fichier <- "outcome-of-care-measures.csv"
      con     <- file(fichier, "r")
      data    <- read.csv(con)
      close(con)
      
      # Are the inputs valid?
      if(sum(as.character(state) == unique(data[, 7])) == 0){
            return("Invalid state")
      }
      outcomeNames <- c("Heart Attack", "Heart Failure", "Pneumonia")
      if(sum(outcome == outcomeNames) == 0){
            return("Invalid outcome")
      }
      
      # Subsetting data frame according to state input
      stateDataFrame <- data[as.character(data[,7]) == state, ]
      
      # Calculating the minimum according to outcome input
      outcomeCol <- c(11, 17, 23) # Matches outcomeNames
      names(outcomeCol) <- outcomeNames
      stateOutcome <- stateDataFrame[, outcomeCol[outcome]]
      stateOutcome <- as.numeric(levels(stateOutcome))[stateOutcome]
      min(stateOutcome, na.rm = TRUE)
}




