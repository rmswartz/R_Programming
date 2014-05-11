rankall <- function(outcome, num = "best") {
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
            ## subset content to only data of interest
            data.subset <- subset(data, select = c(2, 7, 11, 17, 23))
            ## rename variables
            names(data.subset) <- c("hospital", "state", "heart attack", 
                                     "heart failure", "pneumonia")
            ## rank by state, outcome, then hospital name
            data.subset <- data.subset[order(data.subset[, outcome], 
                                             data.subset$hospital), ]
            ## split the data by state
            s <- split(data.subset, data.subset$state)
            ## report the hospitals associated with the minimum outcome value
            if (num == "best") {
                  report.data <- lapply(s, function(x) x[1, 1:2])
                  do.call(rbind.data.frame, report.data)
            ## report the hospitals associated with the maximum outcome value
            } else if (num == "worst") {
                  data.subset <- data.subset[order(-data.subset[, outcome], 
                                                     data.subset$hospital), ]
                  s <- split(data.subset, data.subset$state)
                  report.data <- lapply(s, function(x) x[1, 1:2])
                  do.call(rbind.data.frame, report.data)
            ## report the hospitals associated with the specified outcome rank
            } else {
                  report.data <- lapply(s, function(x) x[num, 1:2])
                  do.call(rbind.data.frame, report.data)
            }
      }
}