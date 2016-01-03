complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    dat <- readDirectoryFullOfFiles(directory)

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    ## TODO: get a better way to check case completeness
    subdat <- dat[dat$ID %in% id 
                    & !is.na(dat[["nitrate"]])
                    & !is.na(dat[["sulfate"]])
                    & !is.na(dat[["Date"]])
                    & !is.na(dat[["ID"]]),]
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    counts <- data.frame(table(subdat$ID))

    ## rename columns
    colnames(counts) <- c("id","nobs")

    ## order according to id vector input
    ## TODO: use SQL instead? The example output is ordered according to the
    ##       input parameter "id"
    counts <- merge (x = data.frame("id" = id), y = counts, by = "id", all.x = TRUE, sort = FALSE)

    ## replace NA with 0, since, e.g., ids in the range [291,294] have no
    ## complete cases
    counts[is.na(counts)] <- 0

    ## order according to id vector input, since 
    counts <- merge (x = data.frame("id" = id), y = counts, by = "id", all.x = TRUE, sort = FALSE)

    counts
}