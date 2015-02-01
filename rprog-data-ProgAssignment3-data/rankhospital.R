## Return the hospital name ranked at give ranking for
## 30-day death rate in a given state and outcome.
## Data source: 
##      outcome-of-care-measures.csv
## When multiple hospitals share the best mortality rate; 
## The first name in the alphabetically sorted list is returned.
## state and outcome params are validated.
##
## Usage:
##      best(state = "", outcome = "", num = 1)
##
## Arguments:
##      state - (2) character appreviated name of a US state available in care measures
##      outcome - "heart attack", "heart failure", "pneumonia"
##      num - ranking of a hospital in that state for that outcome
##
## Example:
##      rankhospital("TX", "heart failure", 4)
##      [1] "DETAR HOSPITAL NAVARRO"

rankhospital <- function(state, outcome, num = "best") {
    
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
    
    
    ## create lookup matrix to match outcome to column number
    getColForOutcome <- function(validOutcomeLookup) {
        dim(validOutcomeLookup) <- c(1,3)
        colnames(validOutcomeLookup) <- validOutcomes 
        validOutcomeLookup[,outcome]
    }
    
    ## 1. Get subset dataset for state
    outcomeMeasuresForState <- subset(outcomeMeasures, outcomeMeasures[,7] == state)    
    
    ## 2. Remove unwanted columns and rename column names
    stateHospitalRates <- outcomeMeasuresForState[,c(2,11,17,23)]
    names(stateHospitalRates) <- c("hospital", "heart attack", "heart failure", "pneumonia")
 
    outcomeCol <-getColForOutcome(2:4)
    ## 3. Convert all "Not Available" to NA  
    stateHospitalRates[[outcomeCol]][stateHospitalRates[[outcomeCol]] == "Not Available"] <- NA
    
    ## 4. Order ascending by outcome named column
    ranked <- stateHospitalRates[order(as.numeric(stateHospitalRates[[outcomeCol]]), stateHospitalRates[[1]] ),]

    if(!is.numeric(num)) {
        if(identical("best", num)) {
            num <- 1
        }
        else if(identical("worst", num)) {
            num <- length(na.omit(ranked[,outcomeCol]))   
        }
        else {
            num <- 0
        }
    }
    
    ## 5. Only up to rank provided
    #ranked[,c(1, outcomeCol)][1:num,]
    ranked[[1]][num]
    
}