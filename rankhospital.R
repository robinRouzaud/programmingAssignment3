rankhospital <- function(state, outcome, num = "Best"){
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
      
      # Converting factors to numeric
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
            result <- oStateDataFrame[1, "Hospital.Name"]
      }else if(num == "Worst"){
            result <- oStateDataFrame[totAvail2, "Hospital.Name"]
      }else if(is.character(num)){
            stop("Invalid ranking")
      }else if(num > totAvail1){
            return(NA)
      }else if(num > totAvail2){
            stop("No data provided by hospital")
      }else{
            result <- oStateDataFrame[num, "Hospital.Name"]
      }

      print(result)      
}