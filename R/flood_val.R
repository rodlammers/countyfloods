#Option 1: Get median or other flow value (Q90) for each day based on daily flow stats
# buildURL <- function(siteNumber){
#   url <- "http://waterservices.usgs.gov/nwis/stat/?format=rdb&sites="
#   url <- paste0(url, siteNumber, "&statReportType=daily&statTypeCd=p50")
#   return(url)
# }
#
# readNWISstat <- function(siteNumber, parameterCd)
# {
#   url <- buildURL(siteNumber)
#   data <- read.table(url, sep="\t", header=TRUE)
#
#   #remove first row
#   data <- data[2:length(data[ ,1]),]
#
#   #keep only discharge values
#   data <- data[data$parameter_cd == parameterCd, ]
#
#   return(data)
# }
#
#
# stat.data <- alply(gages,1,function(x){
#   readNWISstat(siteNumber=x$site_no[1],parameterCd="00060")
# })

#Option 2: Median flood (Q2) to serve as threshold/comparison
#obtain peak flow data from USGS
find_Q2 <- function(site_no){
  #retrieve max daily discharge data from USGS (dataRetrieval package)
  Peaks <- suppressWarnings(dataRetrieval::readNWISpeak(siteNumbers = site_no))

  #use values to construct probability plot using the Weibull plotting method
  Q2 <- plyr::ddply(Peaks, "site_no", function(x) {
    vals <- x$peak_va

    #Remove NAs, rank, and find the probability
    vals <- vals[!is.na(vals)]
    n <- length(vals)

    if (n < 2) {
      Q2 <- NA
    }else {
      rank <- rank(-vals)
      prob <- rank/(n+1)

      Q2 <- approx(x = prob, y = vals, xout = 0.5)
      Q2 <- Q2$y
    }
    return(data.frame(Q2 = Q2, Years = n))
  })

  return(Q2)
}

