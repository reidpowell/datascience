## TODO: get rid of hard coding
rankall <- function(outcome, num) {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that outcome is valid
    colIndex = 0
    if (outcome == "heart attack") {
        colIndex = 11
    } else {
        if (outcome == "heart failure") {
            colIndex = 17
        } else {
            if (outcome == "pneumonia") {
                colIndex = 23
            } else {
                stop("invalid outcome")
            }
        }
    }

    dat <- dat[,c(2,7,colIndex)] # 2 is name, 7 is state
    dat <- dat[complete.cases(dat),]
    dat <- dat[dat[3] != "Not Available",]
    names(dat) <- c("hospital","state","outcome")
    dat <- dat[with(dat, order(outcome,hospital)),]
    dat <- transform(dat,outcome.rank = ave(as.numeric(outcome), state, FUN = function(x) rank(x, ties.method = "first")))
    # dat <- dat[order(-dat[,3],dat[,1])]
    #dat[with(dat, order(outcome,hospital)),]
    dat <- dat[dat$outcome.rank == num, ]
    if (num == "worst") {
        dat <- dat[with(dat, order)]
    }


    states <- data.frame(sort(unique(dat$state)))
    names(states) = c("abbrev")

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result <- merge(x = states, y = dat, by.x = "abbrev", by.y = "state", all.x = TRUE)[,c("hospital","abbrev")]
    names(result) <- c("hospital","state")
    result
}