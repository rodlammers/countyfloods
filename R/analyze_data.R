#' Compute flood statistics
#'
#' Takes flow data and computes flood statistics based on selected flood
#' threshold.
#'
#' @param flow_data A data frame with discharge data for each USGS gage found
#'   for the specified data range. Output from \code{get_flow_data} function.
#' @param peaks A data frame of USGS gage IDs and flood values obtained from
#'   either the \code{find_Q2} or \code{find_NWS} function.
#' @param gages A data frame of all USGS gages and metadata obtained from the
#'   \code{get_gages} function. This input is used to add lat/long and county
#'   codes to the summarized output.
#'
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' site_no \tab character \tab USGS gage ID\cr
#' avg_peak \tab numeric \tab Mean flood ratio for date range (discharge/flood threshold)\cr
#' flood_dur \tab numeric \tab Number of days in date range discharge above flood threshold\cr
#' peak \tab numeric \tab Maximum value of flood ratio for date range (discharge/flood threshold)\cr
#' lat \tab numeric \tab Gage latitude\cr
#' long \tab numeric \tab Gage longitude\cr
#' county_cd \tab character \tab FIPS code of gage county location\cr
#' county \tab character \tab County name\cr
#' state \tab character \tab State name\cr
#' flood \tab character \tab Flood magnitude category based on peak
#' }
#'
#' @examples
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_flow_data <- get_flow_data(va_gages, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_peaks <- find_q2(va_gages$site_no)
#' va_stats <- flood_analysis(flow_data = va_flow_data, peaks = va_peaks,
#'                        gages = va_gages)
#'
#' @importFrom dplyr %>%
#'
#' @export
flood_analysis <- function(flow_data, peaks, gages){

  #Add Q2 or NWS flood discharge values to flow_data data frame
  #Compute statistics for each gage using flood ratios
  flood_stats <- flow_data %>%
    dplyr::left_join(gages, by = "site_no") %>%
    dplyr::left_join(peaks, by = "site_no") %>%
    dplyr::rename_(.dots = list(lat = "dec_lat_va", long = "dec_long_va")) %>%
    dplyr::mutate_(flood_ratio = ~ discharge / flood_val) %>%
    dplyr::group_by_(~ site_no) %>%
    dplyr::summarize_(county_cd = ~ dplyr::first(county_cd),
                     lat = ~ dplyr::first(lat),
                     long = ~ dplyr::first(long),
                     avg_peak = ~ mean(flood_ratio, na.rm = TRUE),
                     flood_dur = ~ sum(flood_ratio > 1, na.rm = TRUE),
                     peak = ~ ifelse(sum(!is.na(flood_ratio) > 0),
                                   max(flood_ratio, na.rm = TRUE), NA),
                     fips = ~ as.numeric(county_cd)) %>%
    dplyr::left_join(maps::county.fips, by = "fips") %>%
    dplyr::select_(.dots = list("-fips")) %>%
    dplyr::mutate_(map_id = ~ polyname) %>%
    tidyr::separate_(col = "polyname", into = c("state", "county"), sep = ",") %>%
    dplyr::arrange_(~ dplyr::desc(peak)) %>%         #sort flood stats by magnitude
    dplyr::mutate_(flood = ~ cut(peak, breaks = c(0, 1, 1.5, 2, 5, 1000),
                                 labels = c("None", "Minor", "Moderate",
                                            "Major", "Extreme"),
                                 include.lowest = TRUE, right = FALSE)) %>%
    dplyr::filter_(~ !is.na(peak)) %>%    #Remove NAs
    dplyr::ungroup()

  return(flood_stats)
}

#' Get county level output
#'
#' Function aggregates gage-level output into county-level output
#'
#' @param flood_stats Data frame of gage-level output from \code{flood_analysis}
#'   function.
#'
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' county \tab character \tab County name\cr
#' state \tab character \tab State name\cr
#' num_gages \tab numeric \tab Number of analyzed gages in county\cr
#' max_peak \tab numeric \tab Maximum observed flood ratio\cr
#' avg_peak \tab numeric \tab Average flood ratio among county gages\cr
#' minor \tab numeric \tab Percentage of gages at or above "minor" flood class (flood ratio > 1)\cr
#' moderate \tab numeric \tab Percentage of gages at or above "moderate" flood class (flood ratio > 1.5)\cr
#' major \tab numeric \tab Percentage of gages at or above "major" flood class (flood ratio > 2)\cr
#' extreme \tab numeric \tab Percentage of gages at or above "extreme" flood class (flood ratio > 5)\cr
#' max_dur \tab numeric \tab Maximum flood duration in county\cr
#' avg_dur \tab numeric \tab Average flood duration in county
#' }
#'
#' @examples
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_flow_data <- get_flow_data(va_gages, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_peaks <- find_Q2(va_gages$gage_no)
#' va_stats <- flood_analysis(flow_data = va_flow_data, peaks = va_peaks,
#'                        gages = va_gages)
#' va_county_stats <- county_aggregates(flood_stats = va_stats)
#'
#' @export
county_aggregates <- function(flood_stats){

  aggregate_fun <- function(gage_flood){
    #county and state name
    county <- gage_flood$county[1]
    state <- gage_flood$state[1]

    #number of gages
    n_gage <- length(gage_flood$site_no)

    #max flood peak
    max_peak <- max(gage_flood$peak)

    #avg flood peak
    avg_peak <- mean(gage_flood$peak)

    #number of gages at different flood stages
    stage <- c("Minor", "Moderate", "Major", "Extreme")

    num <- sapply(stage, function(x) {sum(gage_flood$flood == x)})

    #Convert numbers into percentages at or above that flood class
    minor <- round(sum(num) / n_gage * 100, 1)
    moderate <- round(sum(num[2:4]) / n_gage * 100, 1)
    major <- round(sum(num[3:4]) / n_gage * 100, 1)
    extreme <- round(sum(num[4]) / n_gage * 100, 1)

    #max flood duration
    max_dur <- max(gage_flood$flood_dur)

    #avg flood duration
    avg_dur <- mean(gage_flood$flood_dur)

    return(data.frame(county = county, state = state, num_gages = n_gage,
                      max_peak = max_peak, avg_peak = avg_peak,
                      minor = minor, moderate = moderate, major = major,
                      extreme = extreme, max_dur = max_dur, avg_dur = avg_dur))
  }

  county_stats <- plyr::ddply(flood_stats, "county_cd", aggregate_fun)

  return(county_stats)
}

