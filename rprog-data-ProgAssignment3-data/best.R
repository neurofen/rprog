## Return the hospital name with lowest 30-day death rate in a given state
## and outcome.
## When multiple hospitals share the best mortality rate; 
## The first name in the alphabetically sorted list is returned.
## state and outcome params are validated.
##
## Usage:
##      best(state, outcome)
##
## Arguments:
##      state - US state available in care measures
##      outcome - "heart attack", "heart failure", "pneumonia"
##
## Example:
##      best("TX", "heart attack")
##      [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

best <- function(state = "", outcome = "") {
    ## Read outcome data
    outcomeMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    validStates <- unique(outcomeMeasures[,7])
    if(state %in% validStates == FALSE) {
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")    
    if(outcome %in% validOutcomes == FALSE) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
   
    ## create lookup matrix to match outcome to column number
    getColForOutcome <- function() {
        validOutcomeLookup <- c(13, 19, 25)
        dim(validOutcomeLookup) <- c(1,3)
        colnames(validOutcomeLookup) <- validOutcomes 
        validOutcomeLookup[,outcome]
    }
    
    # Prepare a dataset with rows matching state and outcome for numeric analysis
    outcomeMeasuresForState <- subset(outcomeMeasures, outcomeMeasures[,7] == state)
    rslt <- outcomeMeasuresForState[,getColForOutcome()]
    rslt[rslt == "Not Available"] <- NA
    nresult <- as.numeric(rslt)

    ## Find lowest motality count in state for outcome
    lowest <- nresult[ nresult == min( nresult[!is.na(nresult)], na.rm = TRUE) ]
    lowestMortality <- unique(lowest[!is.na(lowest)])
    
    ## Retrun hospital name assoiciated to lowest mortality rate for outcome
    sort(subset( outcomeMeasuresForState, nresult == lowestMortality )[,2])[1]

}