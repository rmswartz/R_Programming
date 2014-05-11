complete <- function(directory, id = 1:332) {
      full.data <- matrix(NA, nrow = length(id), ncol = 2, dimnames = list(1:length(id), c("id", "nobs")))
      x <- 0
      for (i in seq_along(id)) {
            i <- id[i]
            if (i > 99) {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", i, ".csv"))
            } else if (i > 9) {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", "0", i, ".csv"))
            } else {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", "00", i, ".csv"))
            }
            x <- x + 1
            full.data[x, 1] <- id[x]
            full.data[x, 2] <- sum(complete.cases(data))
      }
full.data <- data.frame(full.data)
full.data
}