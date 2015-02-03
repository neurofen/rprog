pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    pollutantMean <- numeric()
    for(i in id) {       
        monitor <- read.csv( paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="") )
        pollutantMean <- c(pollutantMean, monitor[pollutant][[1]])
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(na.omit(pollutantMean))
}
