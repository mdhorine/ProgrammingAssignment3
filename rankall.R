rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE, na.strings = "Not Available")
    
    ## Read in the function variables and take care of other setup
    chosenOutcome <- outcome
    uniqueStates <- unique(data$State)
    sortedStates <- uniqueStates[order(uniqueStates)]
    columnNumbers <- c(1, 2, 7, 11, 17, 23)
    dfOutput <- data.frame(hospital = character(), state = character())
    
    ## Split the list by state for rankings
    fullStateList <- split(data, data$State)
    
    ## Get outcome and set columnName, stop with error if outcome doesn't exist
    
    if (chosenOutcome == "heart attack") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (chosenOutcome == "heart failure") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else if (chosenOutcome == "pneumonia") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  
    }
    else  {
        stop("invalid outcome")
    }
    
    ## Sort the lists based on outcome and then hospital name, returning the columns we care about
    for (chosenState in sortedStates) {
        chosenStateList <- fullStateList[[chosenState]]
        sortedOutcomeList <- chosenStateList[order(as.numeric(chosenStateList[, columnName]), chosenStateList$Hospital.Name, na.last = NA), columnNumbers]
        
        ## Get the ranking that is to be returned
        if (num == "best") ranking <- 1
        else if (num == "worst") ranking <- nrow(sortedOutcomeList)
        else if (num > nrow(sortedOutcomeList)) ranking <- -1
        else ranking <- num
        
        ## Return hospital name
        if (ranking < 1) {
            hospitalName <- NA
        }
        else {
            hospitalName <- as.character(sortedOutcomeList$Hospital.Name[ranking])
        }
        
        newData <- data.frame(hospital = hospitalName, state = chosenState, row.names = chosenState)
        if (nrow(dfOutput) == 0) {
            dfOutput <- newData
        }
        else {
            dfOutput <- rbind(dfOutput, newData)
        }
    }
    
    dfOutput
}