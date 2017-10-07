best <- function(state, outcome){
      # Opening the dataset for reading
      fichier <- "outcome-of-care-measures.csv"
      con     <- file(fichier, "r")
      data    <- read.csv(con)
      close(con)
      
      # Are the inputs valid?
      if(sum(as.character(state) == unique(data[, 7])) > 0){
            print("State input valid")
      }else{
            return("Invalid state")
      }
      outcomeNames <- c("Heart Attack", "Heart Failure", "Pneumonia")
      if(sum(outcome == outcomeNames) > 0){
            print("Outcome input valid")
      }else{
            print("Invalid outcome")
      }
}