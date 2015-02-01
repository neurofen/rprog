## Given a outcome and ranking number;
## Return a 2 column data frame containing the
## hospital in each state that has the provided ranking.

## Usage:
##      best(outcome = "", num = "best")
##
## Arguments:
##      outcome - "heart attack", "heart failure", "pneumonia"
##      num - ranking of a hospital for that outcome
##
## Example:
##      head( rankall("heart failure", 4), 3)
##          hospital state
##          <NA> AK
##          D W MCMILLAN MEMORIAL HOSPITAL AL
##          ARKANSAS METHODIST MEDICAL CENTER AR
rankinstate <- function(state, outcome, num = "best") {
    
    ## create lookup matrix to match outcome to column number
    getColForOutcome <- function(validOutcomeLookup) {
        dim(validOutcomeLookup) <- c(1,3)
        colnames(validOutcomeLookup) <- validOutcomes 
        validOutcomeLookup[,outcome]
    }
    
    ## 1. Get subset dataset for state
    outcomeMeasuresForState <- subset(outcomeMeasures, outcomeMeasures[,7] == state)    
    
    ## 2. Remove unwanted columns and rename column names
    stateHospitalRates <- outcomeMeasuresForState[,c(2,11,17,23, 7)]
    names(stateHospitalRates) <- c("hospital", "heart attack", "heart failure", "pneumonia", "state")
    
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
    
    ranked[num, ][,c(1,5)]
}

rankall <- function(outcome, num = "best") {
    ## 1. Read outcome data
    outcomeMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## 2. Get list of states in outcome measures
    states <- sort(unique(outcomeMeasures[,7]))
    
    ## 3. Check that outcome is valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")    
    if(outcome %in% validOutcomes == FALSE) {
        stop("invalid outcome")
    }
    
    ## 4. For each state, find the hospital of the given rank
    results <- data.frame(hospital = character(), state = character())
    str(results)
    for(state in states) {
        item <- rankinstate(state, outcome, num)
        results <- rbind(results, item )
        #rbind( results, rankinstate(state, outcome, num) )    
    }
    
    ## 5. Return a data frame with the hospital names and the
    ## (abbreviated) state name
    results
}

