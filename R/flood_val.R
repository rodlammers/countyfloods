#' Get median flood for each gage
#'
#' This function will get annual maximum flow series for each USGS gage and
#' compute median flood (Q2) to serve as flood threshold
#'
#' @param site_no Character vector with USGS gage IDs of stream gage sites to
#'   pull.
#'
#' @return A data frame with median flood values (Q2) and the number of years of
#'   data used to compute this value.
#'
#' @examples
#'
#' miami_gages <- gage_extract("12086", start_date = "2000-01-01",
#'                             end_date = "2009-12-31")
#' miami_q2 <- find_q2(site_no = miami_gages$site_no)
#'
#' @importFrom dplyr %>%
#'
#' @export
find_q2 <- function(site_no){
  #retrieve annual peak discharge data from USGS (dataRetrieval package)
  peaks <- suppressWarnings(dataRetrieval::readNWISpeak(siteNumbers = site_no))

  #use values to construct probability plot using the Weibull plotting method
  flood <- peaks %>%
    dplyr::group_by_(~ site_no) %>%
    dplyr::summarize_(flood_val = ~ construct_prob_plot(peak_va),
                      years = ~ sum(!is.na(peak_va)))

  return(flood)
}

#' Construct probability plot using the Weibull plotting method
#'
#' @param vals A numeric vector with peak discharge values ... [more on what
#'    this column of the data from readNWISpeak gives]
#'
#' @export
construct_prob_plot <- function(vals){

  #Remove NAs, rank, and find the probability
  vals <- vals[!is.na(vals)]
  n <- length(vals)

  if (n < 2) {
    Q2 <- NA
  } else {
    rank <- rank(-vals)
    prob <- rank / (n + 1)

    Q2 <- approx(x = prob, y = vals, xout = 0.5)
    Q2 <- Q2$y
  }
  return(Q2)
}

#' Get National Weather Service (NWS) flood stage/discharge levels for gages.
#'
#' Use National Weather Service designated flood stages/discharges as flood
#' thresholds. These come in four levels: "action", "flood", "moderate", and
#' "major". Note that most USGS gages do not have these values specified (or may
#' not have all levels) so using this definition of the flood threshold can
#' severely limit the sample size of the data output.
#'
#' @param site_no Character vector with USGS gage IDs of stream gage sites to
#'   pull.
#' @param type Character string with the type of flood stage to be used. Can be
#'   one of four options: "action", "flood", "moderate", and "major".
#'
#' @return Data frame of gage IDs and the corresponding NWS flood value, if
#' available.
#'
#' @examples
#'
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_NWS <- find_NWS(site_no = va_gages$site_no, type = "moderate")
#'
#' @export
find_NWS <- function(site_no, type) {

  #check capitalization and append "_Q" to type
  type <- R.utils::capitalize(tolower(type))
  type <- paste0(type, "_Q")

  #match gage and type to NWS data stored internally
  flood_val <- plyr::ldply(site_no, function(x) {
    val <- NWS_flood_discharge[ ,type][NWS_flood_discharge$USGS %in% x]

    if (length(val) == 0) {val <- NA}

    return(data.frame(site_no = x, flood_val = val))

  })

}
