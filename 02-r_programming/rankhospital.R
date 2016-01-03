## TODO: get rid of hard coding
best <- function(state, outcome, rank) {
	## Read outcome data
	dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	states <- unique(dat[,7]) ## 7 = State Column
	if (!is.element(state,states)) {
		stop("invalid state")
	}

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

	if (rank == "worst") {
		rank = 0
	} else {
		if (rank == "best") {
			rank = 1
		}
	}

	## Return hospital name in that state with lowest 30-day death
	## rate
	sort(dat[2][dat[7] == state & dat[colIndex] == min(dat[colIndex][dat[7] == state & !is.na(dat[colIndex])])])[1] ## Column 2 = Hospital Name
}