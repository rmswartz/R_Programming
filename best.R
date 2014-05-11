best <- function(state, outcome) {
      ## read in data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                       na.strings = "Not Available")
      ## convert mortality rates to numeric
      data[, 11] <- as.numeric(data[, 11])
      data[, 17] <- as.numeric(data[, 17])
      data[, 23] <- as.numeric(data[, 23])
      ## check for valid state and outcome
      state.check <- state %in% data[, 7]
      outcome.check <- outcome %in% c("heart attack", "heart failure", 
                                      "pneumonia")
      ## return error messages if either are invalid
      if (state.check == FALSE) {
            stop("invalid state")
      } else if (outcome.check == FALSE) {
            stop("invalid outcome")
      ## continue with function
      } else {
            ## subset content to only state of interest
            state.subset <- subset(data, State == state, 
                                   select = c(2, 7, 11, 17, 23))
            ## order the content based on hospital name
            state.subset <- state.subset[order(state.subset[, 2]),]
            ## report the hospital associated with the minimum outcome value
            if (outcome == "heart attack") {
                  state.subset[which.min(state.subset[, 3]), 1]
            } else if (outcome == "heart failure") {
                  state.subset[which.min(state.subset[, 4]), 1]
            } else {
                  state.subset[which.min(state.subset[, 5]), 1]
            }
      }
}