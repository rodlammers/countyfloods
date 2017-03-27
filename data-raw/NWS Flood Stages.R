library(dataRetrieval)
library(plyr)

#How do I refer to this file if its saved as part of the package?
# BA: You can assume the package is working from the main package directory,
# so best practice is to do relative file names from there (data-raw/filename)
#setwd("C:/Users/rlammers/Documents/NWS Flood Stages")

# BA: Do we have information anywhere about where we got these two files ("NWS
# Flood Stages.txt" and "USGS-NWS Gages.txt")? Maybe the website where we
# downloaded them?

NWS_data <- read.table("data-raw/NWS Flood Stages.txt", header = TRUE, sep = "\t")
gage_no <- read.table("data-raw/USGS-NWS Gages.txt", header = TRUE, sep = "\t",
                      colClasses = c("character", "character"))

#remove NWS data without at least one "flood stage" metric
NWS_data$test <- apply(NWS_data, 1, function(x){sum(!is.na(x[13:16]))})
NWS_data <- NWS_data[NWS_data$test > 0, ]

#match USGS gage numbers to NWS gage names
for (i in 1:length(NWS_data[, 1])){
  gage <- gage_no$USGS[gage_no$NWS %in% NWS_data$GaugeLID[i]]
  if (length(gage) < 1){gage <- NA}

  NWS_data$USGS[i] <- as.character(gage[1])
}

#Filter out rows with no matching USGS gage
NWS_data <- NWS_data[!is.na(NWS_data$USGS), ]

#Get rating table and estimate discharge values for each "flood" stage
getQ <- function(gage){
  stages <- gage[13:16]

  rating <- readNWISrating(gage$USGS)

  if (length(rating[, 1]) < 1 | is.null(rating$INDEP) |
      is.null(rating$DEP)) {
    Q <- rep(NA, 4)
  }else {
    #interpolate points
    points <- spline(rating$INDEP, rating$DEP, n = 50)

    #find discharge values based on stages
    Q <- approx(points$x, points$y, xout = stages)
    Q <- Q$y
  }

  gage[27:30] <- Q
  return(gage)
}

NWS_data2 <- adply(NWS_data, 1, getQ)

#remove NWS data without at least one "flood discharge" metric
NWS_data2$test <- apply(NWS_data2, 1, function(x){sum(!is.na(x[27:30]))})
NWS_data2 <- NWS_data2[NWS_data2$test > 0, ]

NWS_data3 <- NWS_data2[ ,-c(1, 2, 4, 10, 11, 17:23, 25)]
names(NWS_data3)[14:17] <- c("Action_Q", "Flood_Q", "Moderate_Q", "Major_Q")

write.table(NWS_data3, "data-raw/NWS Flood Discharge Data.txt", col.names = TRUE, sep = "\t", row.names = FALSE)
