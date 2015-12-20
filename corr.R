## http://stats.stackexchange.com/questions/4040/r-compute-correlation-by-group
require(plyr)
corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    dat <- readDirectoryFullOfFiles(directory)

    ## lose records where sulfate or nitrate is NA
    dat <- subset(dat, !is.na(dat$sulfate) & !is.na(dat$nitrate))

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    sufficientCase <- subset(complete(directory), nobs > threshold)

    ## JOIN to get records from monitors with a sufficient number of complete
    ## cases
    dat <- merge(x = dat, y = sufficientCase, by.x = "ID", by.y = "id")

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    monitorPolutantCorrelation <- ddply(dat, .(ID), function(df){
        return(data.frame(COR = cor(df$sulfate, df$nitrate)))})

    monitorPolutantCorrelation$COR
}
