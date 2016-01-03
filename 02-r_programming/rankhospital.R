## TODO: get rid of hard coding
rankhospital <- function(state, outcome, rank = "best") {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    states <- unique(dat[,7]) ## 7 = State Column
    if (!is.element(state,states)) {
        stop("invalid state")
    }

    colIndex = ""
    if (outcome == "heart attack") {
        colIndex = grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack$",colnames(dat))
    }
    
    if (outcome == "heart failure") {
        colIndex = grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure$",colnames(dat))
    }
            
    if (outcome == "pneumonia") {
        colIndex = grep("^Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia$",colnames(dat))
    }
    
    

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    hospitals <- subset(dat,dat$State == state & !is.na(dat[colIndex]) & dat[colIndex]!= "Not Available")
    ranking <- hospitals[with(hospitals, order(-as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), Hospital.Name)),]
    
    ## check rank input
    if (rank == "worst") {
        rank = nrow(ranking)
    } else {
        if (rank == "best") {
            rank = 1
        }
    }

    ranking[rank,"Hospital.Name"]
}