rankhospital <- function(state, outcome, num = "best") {
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
            ## rename variables
            names(state.subset) <- c("Hospital", "State", "heart attack", 
                                     "heart failure", "pneumonia")
            ## rank by outcome, then hospital name
            state.subset <- state.subset[order(state.subset[, outcome], 
                                               state.subset$Hospital), ]
            ## report the hospital associated with the minimum outcome value
            if (num == "best") {
                  state.subset[1, 1]
            ## report the hospital associated with the maximum outcome value
            } else if (num == "worst") {
                  state.subset <- state.subset[order(-state.subset[, outcome], 
                                                     state.subset$Hospital), ]
                  state.subset[1, 1]
            ## report the hospital associated with the specified outcome rank
            } else {
                  state.subset[num, 1]
            }
      }
}