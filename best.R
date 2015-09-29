best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Read in the function variables and take care of other setup
    chosenState <- state
    chosenOutcome <- outcome
    columnNumbers <- c(1, 2, 7, 11, 17, 23)
    
    ## Check that state and outcome are valid
    fullStateList <- split(data, data$State)
    chosenStateList <- fullStateList[[chosenState]]
    if (is.null(nrow(chosenStateList))) {
        stop("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
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
    
    ## Sort the list based on outcome and then hospital name, returning the columns we care about
    sortedOutcomeList <- chosenStateList[order(as.numeric(chosenStateList[, columnName]), chosenStateList$Hospital.Name), columnNumbers]

    ## Return hospital name
    as.character(sortedOutcomeList$Hospital.Name[1])
}