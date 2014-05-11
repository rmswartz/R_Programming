corr <- function(directory, threshold = 0) {
      num.files <- length(list.files(paste0(getwd(), "\\", directory)))
      full.data <- matrix(NA, nrow = num.files, ncol = 3, 
                          dimnames = list(1:num.files, c("id", "nobs", "cor")))
      x <- 0
      for(i in 1:num.files) {
            if (i > 99) {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", i, ".csv"))
            } else if (i > 9) {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", "0", i, ".csv"))
            } else {
                  data <- read.csv(paste0(getwd(), "\\", directory, "\\", "00", i, ".csv"))
            }
      x <- x + 1
      
      full.data[x, 1] <- i
      full.data[x, 2] <- sum(complete.cases(data))
      data <- subset(data, !is.na(Date) & !is.na(sulfate) & !is.na(nitrate) & !is.na(ID))
      full.data[x, 3] <- cor(data$sulfate, data$nitrate)
      }
full.data <- data.frame(full.data)
thresh.data <- subset(full.data, nobs >= threshold & !is.na(cor), select = cor)
thresh.vector <- thresh.data[,1]
}