corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
        
    csvCount<-length(list.files(directory, pattern="*.csv"))

    allCompCases <- complete(directory, 1:csvCount)

    # filter out cases that are below or equal to threshold
    threshCases <- subset(allCompCases, allCompCases["nobs"][[1]] > threshold)

    # load files by file id in threshCases
    myCors<-vector()
    
    for(i in threshCases["id"][[1]]) {
        monitor <- read.csv( paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="") )
        myCors <- c( myCors, cor(monitor["sulfate"], monitor["nitrate"], use="pairwise.complete.obs") )
    }
    myCors
}