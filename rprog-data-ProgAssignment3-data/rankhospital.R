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
    getColForOutcome <- function() {
        #validOutcomeLookup <- c(13, 19, 25)
        validOutcomeLookup <- c(11, 17, 23)
        #validOutcomeLookup <- c(17, 23, 29)
        dim(validOutcomeLookup) <- c(1,3)
        colnames(validOutcomeLookup) <- validOutcomes 
        validOutcomeLookup[,outcome]
    }
    
    # Prepare a dataset with rows matching state and outcome for numeric analysis
    outcomeMeasuresForState <- subset(outcomeMeasures, outcomeMeasures[,7] == state)
    
    #print(outcomeMeasuresForState)
    
    rslt <- outcomeMeasuresForState[,getColForOutcome()]
    rslt[rslt == "Not Available"] <- NA
    nresult <- as.numeric(rslt)
    
    #print(nresult)
    orderedRates <- sort( nresult[!is.na(nresult)])
    print("Ordered Rates:")
    print(orderedRates)
    
    #uniqueRates <- unique( sort( as.numeric(rslt[!is.na(rslt)])))   
    #print("#### Mortality rates:")
    #print(uniqueRates)
    
    if(!is.numeric(num)) {
        if(identical("best", num)) {
            num <- 1
        }
        else if(identical("worst", num)) {
            num <- length(orderedRates)   
        }
        else {
            num <- 0
        }
    }
    
    print("num is:")
    print(num)
    rankedRate <- orderedRates[orderedRates <= orderedRates[num]]    
    print("#### Ranked rate:")
    print(rankedRate)
    
    print("#### Result:")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    
    hospitalsMeetingRankedRate <- data.frame(subset( outcomeMeasuresForState, orderedRates <= orderedRates[num] )[,2], rankedRate, 1:length(rankedRate))
    names(hospitalsMeetingRankedRate) <- c("hospital", "rate", "ran")
    hospitalsMeetingRankedRate
    #hospitalsMeetingRankedRate <- sort(subset( outcomeMeasuresForState, nresult <= orderedRates[num] )[,2])
}