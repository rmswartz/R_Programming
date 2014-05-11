pollutantmean <- function(directory, pollutant, id = 1:332) {
      full.data.NA <- NA 
      for (i in id[1]:id[length(id)]) {
            if (i > 99) {
                  part.data <- read.csv(paste0(getwd(), "\\", directory, "\\", i, ".csv"))
            } else if (i > 9) {
                  part.data <- read.csv(paste0(getwd(), "\\", directory, "\\", "0", i, ".csv"))
            } else {
                  part.data <- read.csv(paste0(getwd(), "\\", directory, "\\", "00", i, ".csv"))
            }
      full.data.NA <- rbind(full.data.NA, part.data)
      }
      full.data <- subset(full.data.NA, !is.na(full.data.NA[[pollutant]]))
      mean(full.data[[pollutant]])
}