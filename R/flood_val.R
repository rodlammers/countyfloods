#' Get median flood for each gage
#'
#' This function will get annual maximum flow series for each USGS gage and
#' compute median flood (Q2) to serve as flood threshold. Peak flow data
#' is obtained using the \code{readNWISpeak} function from the
#' \code{dataRetrieval} package.
#'
#' @param site_no Character vector with USGS gage IDs of stream gage sites to
#'   pull.
#'
#' @return A data frame with median flood values (Q2) and the number of years of
#'   data used to compute this value.
#'
#' @seealso \code{\link[dataRetrieval]{readNWISpeak}}
#'
#' @examples
#'
#' miami_gages <- gage_extract("12086", start_date = "2000-01-01",
#'                             end_date = "2009-12-31")
#' miami_q2 <- find_q2(site_no = miami_gages$site_no,
#'                     start_date = "2000-01-01", end_date = "2009-12-31")
#'
#' va_gages <- get_county_cd("Virginia") %>%
#'    get_gages(start_date = "2015-01-01", end_date = "2015-12-31")
#' va_q2 <- find_q2(va_gages$site_no)
#'
#' @importFrom dplyr %>%
#'
#' @export
find_q2 <- function(site_no){
  #retrieve annual peak discharge data from USGS (dataRetrieval package)
  peaks <- suppressWarnings(dataRetrieval::readNWISpeak(siteNumbers = site_no,
                                                        convertType = FALSE))

  #use values to construct probability plot using the Weibull plotting method
  flood <- peaks %>%
    dplyr::mutate_(peak_dt = ~ suppressWarnings(lubridate::ymd(peak_dt)),
                   peak_va = ~ as.numeric(peak_va)) %>%
    dplyr::filter_(~ !is.na(peak_dt)) %>%
    dplyr::group_by_(~ site_no) %>%
    dplyr::summarize_(flood_val = ~ construct_prob_plot(peak_va),
                      years = ~ sum(!is.na(peak_va)))

  return(flood)
}

#' Construct probability plot using the Weibull plotting method
#'
#' @param vals A numeric vector of annual peak discharge values obtained from
#'   the \code{readNWISpeak} function of the \code{dataRetrieval} package.
#'
#' @details
#' The Weibull plotting method is commonly used in flood-frequency analysis.
#' The basic procedure invovles ranking the values from highest to lowest and
#' calculating an exceedence probability (\eqn{p = rank / (n + 1)}) where n
#' is the total number of observations. The median annual flood (Q2) is the
#' flow with a probability of 0.5.
#'
#' @references
#' Rao, A.R. and Hamed, K.H. 2000. Flood Frequency Analysis. CRC Press: Boca Raton.
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

    Q2 <- stats::approx(x = prob, y = vals, xout = 0.5)
    Q2 <- Q2$y
  }
  return(Q2)
}

#' Get National Weather Service (NWS) flood stage/discharge levels for gages.
#'
#' Use National Weather Service designated flood stages/discharges as flood
#' thresholds. These come in four levels: "action", "flood", "moderate", and
#' "major".
#'
#' @note Since most USGS gages do not have these values specified (or may
#' not have all levels), using this definition of the flood threshold can
#' severely limit the sample size of the data output.
#'
#' @param site_no Character vector with USGS gage IDs of stream gage sites to
#'   pull.
#' @param type Character string with the type of flood stage to be used. Can be
#'   one of four options: "action", "flood", "moderate", and "major". Defaults
#'   to "flood".
#'
#' @return Data frame of gage IDs and the corresponding NWS flood value, if
#' available.
#'
#' @examples
#'
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_nws <- find_nws(site_no = va_gages$site_no, type = "moderate")
#'
#' @export
find_nws <- function(site_no, type = "flood") {

  #check capitalization and append "_Q" to type
  type <- R.utils::capitalize(tolower(type))
  type <- paste0(type, "_Q")

  #match gage and type to NWS data stored internally
  flood_val <- NWS_flood_discharge %>%
    dplyr::select_(~ USGS, ~ Action_Q, ~ Flood_Q, ~ Moderate_Q, ~ Major_Q) %>%
    dplyr::filter_(~ USGS %in% site_no) %>%
    tidyr::gather_(key_col = "key", value_col = "flood_val",
                   gather_cols = c("Action_Q", "Flood_Q",
                                   "Moderate_Q", "Major_Q")) %>%
    dplyr::filter_(~ key == type & !is.na(flood_val)) %>%
    dplyr::select_(.dots = list("-key")) %>%
    dplyr::rename_(.dots = list(site_no = "USGS"))

  return(flood_val)
}
