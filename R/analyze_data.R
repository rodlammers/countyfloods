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
#' @param county_cd Character vector with the county FIPS code(s)
#' @param q2_val A data frame with values of the median annual flood to be used
#' to compare relative sizes of streams at gage locations
#' @param threshold Character string of the flood threshold to be used in the
#'   analysis (either "Q2" or "NWS"). Used to determine which type of summary
#'   statistics to compute.
#'
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' site_no \tab character \tab USGS gage ID\cr
#' county_cd \tab character \tab FIPS code of gage county location\cr
#' lat \tab numeric \tab Gage latitude\cr
#' long \tab numeric \tab Gage longitude\cr
#' avg_peak \tab numeric \tab Mean flood ratio for date range (discharge/flood threshold)\cr
#' flood_dur \tab numeric \tab Number of days in date range discharge above flood threshold\cr
#' max_peak \tab numeric \tab Maximum value of flood ratio for date range (discharge/flood threshold)\cr
#' num_missing \tab numeric \tab Number of days with missing discharge data from input date range\cr
#' size \tab numeric \tab Metric of the relative size of the river (logarithm of median annual flood)\cr
#' state \tab character \tab State name\cr
#' county \tab character \tab County name\cr
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
flood_analysis <- function(flow_data, peaks, gages, county_cd, q2_val =
                             data.frame(site_no = "", q2 = NA, stringsAsFactors = FALSE),
                           threshold){

  #Add Q2 or NWS flood discharge values to flow_data data frame
  #Compute statistics for each gage using flood ratios
  flood_stats <- flow_data %>%
    dplyr::left_join(gages, by = "site_no") %>%
    dplyr::left_join(peaks, by = "site_no") %>%
    dplyr::left_join(q2_val, by = "site_no") %>%
    dplyr::rename_(.dots = list(lat = "dec_lat_va", long = "dec_long_va")) %>%
    dplyr::mutate_(flood_ratio = ~ discharge / flood_val) %>%
    dplyr::group_by_(~ site_no) %>%
    dplyr::summarize_(county_cd = ~ dplyr::first(county_cd),
                     lat = ~ dplyr::first(lat),
                     long = ~ dplyr::first(long),
                     avg_peak = ~ mean(flood_ratio, na.rm = TRUE),
                     flood_dur = ~ sum(flood_ratio > 1, na.rm = TRUE),
                     max_peak = ~ ifelse(sum(!is.na(flood_ratio) > 0),
                                   max(flood_ratio, na.rm = TRUE), NA),
                     fips = ~ as.numeric(county_cd),
                     num_missing = ~ sum(is.na(flood_ratio)),
                     size = ~ dplyr::first(log10(q2)),
                     size = ~ ifelse(is.na(size), 1, size)) %>%
    dplyr::filter_(~ !is.na(max_peak))   #Remove NAs

    #add any county_cd in input that has no gages
    add_county_cd <- county_cd[!(county_cd %in% flood_stats$county_cd)]
    if(length(add_county_cd) > 0) {
      flood_stats <- dplyr::bind_rows(flood_stats,
                                      data.frame(county_cd = add_county_cd,
                                                fips = as.numeric(add_county_cd),
                                                site_no = as.character(-1:(-length(add_county_cd)))))
    }

    flood_stats <- flood_stats %>% dplyr::left_join(maps::county.fips, by = "fips") %>%
    dplyr::select_(.dots = list("-fips")) %>%
    tidyr::separate_(col = "polyname", into = c("state", "county"), sep = ",") %>%
    dplyr::arrange_(~ dplyr::desc(max_peak))        #sort flood stats by magnitude

    if (threshold == "Q2") {
      flood_stats <- flood_stats %>% dplyr::mutate_(flood = ~ cut(max_peak,
                                 breaks = c(0, 1, 1.5, 2, 5, 1000),
                                 labels = c("None", "Minor", "Moderate",
                                            "Major", "Extreme"),
                                 include.lowest = TRUE, right = FALSE))
    }else if (threshold == "NWS") {
      flood_stats <- flood_stats %>% dplyr::mutate_(flood = ~ cut(max_peak,
                                 breaks = c(0, 1, 1000),
                                 labels = c("No Flood", "Flood"),
                                 include.lowest = TRUE, right = FALSE))

    }

    flood_stats <- dplyr::ungroup(flood_stats)

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
#' If threshold = "NWS", the columns "minor", "moderate", "major", and "extreme"
#' are replaced with two columns: "no_flood" and "yes_flood" which show the
#' percentage of gages in the county with or without flooding.
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
#' @importFrom dplyr %>%
#'
#' @export
county_aggregates <- function(flood_stats, county_cd){

  if (dplyr::first(flood_stats$flood) == "No Flood" | dplyr::first(flood_stats$flood) == "Flood"){
    county_stats <- flood_stats %>%
      dplyr::group_by_(~ county_cd) %>%
      dplyr::summarize_(county = ~ dplyr::first(county),
                        state = ~ dplyr::first(state),
                        num_gage = ~ n(),
                        max_peak = ~ max(max_peak, na.rm = TRUE),
                        avg_peak = ~ mean(max_peak, na.rm = TRUE),
                        no_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                 round(100 * sum((flood %in% c("No Flood")) /
                                                                   num_gage), 1)),
                        yes_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                    round(100 * sum((flood %in% c("Flood")) /
                                                                      num_gage), 1)),
                        max_dur = ~ max(flood_dur, na.rm = TRUE),
                        avg_dur = ~ mean(flood_dur, na.rm = TRUE))
  } else {
    county_stats <- flood_stats %>%
      dplyr::group_by_(~ county_cd) %>%
      dplyr::summarize_(county = ~ dplyr::first(county),
                        state = ~ dplyr::first(state),
                        num_gage = ~ n(),
                        max_peak = ~ max(max_peak, na.rm = TRUE),
                        avg_peak = ~ mean(max_peak, na.rm = TRUE),
                        minor = ~ dplyr::if_else(is.na(max_peak), -1,
                                                 round(100 * sum((flood %in% c("Minor",
                                                                "Moderate",
                                                                "Major",
                                                                "Extreme")) /
                                                    num_gage), 1)),
                        moderate = ~ dplyr::if_else(is.na(max_peak), -1,
                                                    round(100 * sum((flood %in% c("Moderate",
                                                                   "Major",
                                                                   "Extreme")) /
                                                       num_gage), 1)),
                        major = ~ dplyr::if_else(is.na(max_peak), -1,
                                                 round(100 * sum((flood %in% c("Major",
                                                                "Extreme")) /
                                                    num_gage), 1)),
                        extreme = ~ dplyr::if_else(is.na(max_peak), -1,
                                                   round(100 * sum((flood %in% c("Extreme")) /
                                                      num_gage), 1)),
                        max_dur = ~ max(flood_dur, na.rm = TRUE),
                        avg_dur = ~ mean(flood_dur, na.rm = TRUE))
  }


  return(county_stats)
}


#' Get county level output from \code{long_range_flood} analysis
#'
#' Function aggregates gage-level output into county-level output. This is
#' the same as the \code{county_aggregates} function but it summarizes by
#' county and date range instead of just county.
#'
#' @inheritParams county_aggregates
#'
#' @return A data frame with the following columns:
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   county_cd \tab character \tab FIPS code of gage county location\cr
#'   start_date \tab date \tab Input start date\cr
#'   end_date \tab date \tab Input end date\cr
#'   county \tab character \tab County name\cr
#'   state \tab character \tab State name\cr
#'   num_gages \tab numeric \tab Number of analyzed gages in county\cr
#'   max_peak \tab numeric \tab Maximum observed flood ratio\cr
#'   avg_peak \tab numeric \tab Average flood ratio among county gages\cr
#'   minor \tab numeric \tab Percentage of gages at or above "minor" flood
#'      class (flood ratio > 1)\cr
#'   moderate \tab numeric \tab Percentage of gages at or above "moderate"
#'      flood class (flood ratio > 1.5)\cr
#'   major \tab numeric \tab Percentage of gages at or above "major" flood
#'      class (flood ratio > 2)\cr
#'   extreme \tab numeric \tab Percentage of gages at or above "extreme"
#'      flood class (flood ratio > 5)\cr
#'   max_dur \tab numeric \tab Maximum flood duration in county\cr
#'   avg_dur \tab numeric \tab Average flood duration in county }
#'
#' If threshold = "NWS", the columns "minor", "moderate", "major", and "extreme"
#' are replaced with two columns: "no_flood" and "yes_flood" which show the
#' percentage of gages in the county with or without flooding.
#'
#' @examples
#' Internal function used within \code{long_term_flood} function.
#'
#' @importFrom dplyr %>%
#'
#' @export
county_aggregates2 <- function(flood_stats, county_cd){

  if (dplyr::first(flood_stats$flood) == "No Flood" | dplyr::first(flood_stats$flood) == "Flood"){
    county_stats <- flood_stats %>%
      dplyr::group_by_(~ county_cd, ~ start_date, ~end_date) %>%
      dplyr::summarize_(county = ~ dplyr::first(county),
                        state = ~ dplyr::first(state),
                        num_gage = ~ n(),
                        max_peak = ~ max(max_peak, na.rm = TRUE),
                        avg_peak = ~ mean(max_peak, na.rm = TRUE),
                        no_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                    round(100 * sum((flood %in% c("No Flood")) /
                                                                      num_gage), 1)),
                        yes_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                     round(100 * sum((flood %in% c("Flood")) /
                                                                       num_gage), 1)),
                        max_dur = ~ max(flood_dur, na.rm = TRUE),
                        avg_dur = ~ mean(flood_dur, na.rm = TRUE))
  }else {
    county_stats <- flood_stats %>%
      dplyr::group_by_(~ county_cd, ~ start_date, ~end_date) %>%
      dplyr::summarize_(county = ~ dplyr::first(county),
                        state = ~ dplyr::first(state),
                        num_gage = ~ n(),
                        max_peak = ~ max(max_peak, na.rm = TRUE),
                        avg_peak = ~ mean(max_peak, na.rm = TRUE),
                        minor = ~ dplyr::if_else(is.na(max_peak), -1,
                                                 round(100 * sum((flood %in% c("Minor",
                                                                               "Moderate",
                                                                               "Major",
                                                                               "Extreme")) /
                                                                   num_gage), 1)),
                        moderate = ~ dplyr::if_else(is.na(max_peak), -1,
                                                    round(100 * sum((flood %in% c("Moderate",
                                                                                  "Major",
                                                                                  "Extreme")) /
                                                                      num_gage), 1)),
                        major = ~ dplyr::if_else(is.na(max_peak), -1,
                                                 round(100 * sum((flood %in% c("Major",
                                                                               "Extreme")) /
                                                                   num_gage), 1)),
                        extreme = ~ dplyr::if_else(is.na(max_peak), -1,
                                                   round(100 * sum((flood %in% c("Extreme")) /
                                                                     num_gage), 1)),
                        max_dur = ~ max(flood_dur, na.rm = TRUE),
                        avg_dur = ~ mean(flood_dur, na.rm = TRUE))
  }

  return(county_stats)
}

#This function will analyze floods across a long time period, returning
#useful summary results

#' Get time series output
#'
#' Function takes flow data and summarizes flood occurrence through time at
#' both the gage and county level.
#'
#' @param flow_data A data frame with discharge data for each USGS gage found
#'   for the specified data range. Output from \code{get_flow_data} function.
#' @param peaks A data frame of USGS gage IDs and flood values obtained from
#'   either the \code{find_Q2} or \code{find_NWS} function.
#' @param gages A data frame of all USGS gages and metadata obtained from the
#'   \code{get_gages} function. This input is used to add lat/long and county
#'   codes to the summarized output.
#' @param county_cd Character vector with the county FIPS code(s)
#' @param q2_val A data frame with values of the median annual flood to be used
#' to compare relative sizes of streams at gage locations (from the \code{find_Q2}
#' function).
#'
#' @return A list with two data frames, summarizing the results by gage and by county:
#'
#' Gage:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' site_no \tab character \tab USGS gage ID\cr
#' date \tab date \tab Date of observation\cr
#' discharge \tab numeric \tab Observed mean daily discharge (cubic feet per second)\cr
#' lat \tab numeric \tab Gage latitude\cr
#' long \tab numeric \tab Gage longitude\cr
#' county_cd \tab character \tab FIPS code of gage county location\cr
#' size \tab numeric \tab Metric of the relative size of the river (logarithm of median annual flood)\cr
#' flood_ratio \tab numeric \tab Metric of the observed discharge divided by the defined flood threshold\cr
#' state \tab character \tab State name\cr
#' county \tab character \tab County name\cr
#' flood \tab character \tab Flood magnitude category based on peak
#' }
#'
#' County:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' date \tab date \tab Date of observation\cr
#' county \tab character \tab County name\cr
#' state \tab character \tab State name\cr
#' num_gages \tab numeric \tab Number of analyzed gages in county\cr
#' max_peak \tab numeric \tab Maximum observed flood ratio\cr
#' avg_peak \tab numeric \tab Average flood ratio among county gages\cr
#' minor \tab numeric \tab Percentage of gages at or above "minor" flood class (flood ratio > 1)\cr
#' moderate \tab numeric \tab Percentage of gages at or above "moderate" flood class (flood ratio > 1.5)\cr
#' major \tab numeric \tab Percentage of gages at or above "major" flood class (flood ratio > 2)\cr
#' extreme \tab numeric \tab Percentage of gages at or above "extreme" flood class (flood ratio > 5)\cr
#' }
#'
#' If threshold = "NWS", the columns "minor", "moderate", "major", and "extreme"
#' are replaced with two columns: "no_flood" and "yes_flood" which show the
#' percentage of gages in the county with or without flooding.
#'
#' @examples
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_flow_data <- get_flow_data(va_gages, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' va_peaks <- find_q2(va_gages$site_no)
#' va_time_series <- time_series_analysis(flow_data = va_flow_data, peaks = va_peaks,
#'                         gages = va_gages, county_cd = va_counties,
#'                         q2_val = dplyr::rename_(va_peaks, .dots = list(q2 = "flood_val")))
#'
#' @importFrom dplyr %>%
#'
#' @export
time_series_analysis <- function(flow_data, peaks, gages, county_cd, q2_val, threshold){

  #Add Q2 or NWS flood discharge values to flow_data data frame
  #Compute statistics for each gage using flood ratios
  flood_series <- flow_data %>%
    dplyr::left_join(gages, by = "site_no") %>%
    dplyr::left_join(peaks, by = "site_no") %>%
    dplyr::left_join(q2_val, by = "site_no") %>%
    dplyr::rename_(.dots = list(lat = "dec_lat_va", long = "dec_long_va")) %>%
    dplyr::mutate_(flood_ratio = ~ discharge / flood_val) %>%
    dplyr::mutate_(size = ~ log10(q2)) %>%
    dplyr::mutate_(fips = ~ as.numeric(county_cd)) %>%
    dplyr::filter_(~ !is.na(flood_val)) %>%
    dplyr::filter_(~ flood_ratio > 1) %>%
    dplyr::left_join(maps::county.fips, by = "fips") %>%
    dplyr::select_(.dots = list("-fips")) %>%
    dplyr::mutate_(map_id = ~ polyname) %>%
    tidyr::separate_(col = "polyname", into = c("state", "county"), sep = ",")

    if (threshold == "Q2") {
      flood_series <- flood_series %>% dplyr::mutate_(flood = ~ cut(flood_ratio,
                                                                  breaks = c(0, 1, 1.5, 2, 5, 1000),
                                                                  labels = c("None", "Minor", "Moderate",
                                                                             "Major", "Extreme"),
                                                                  include.lowest = TRUE, right = FALSE))
    }else if (threshold == "NWS") {
      flood_series <- flood_series %>% dplyr::mutate_(flood = ~ cut(flood_ratio,
                                                                  breaks = c(0, 1, 1000),
                                                                  labels = c("No Flood", "Flood"),
                                                                  include.lowest = TRUE, right = FALSE))
    }

    flood_series <- flood_series %>%
      dplyr::select_(.dots = list("site_no", "date", "discharge", "lat",
                                "long", "county_cd", "size", "flood_ratio",
                                "state", "county", "flood"))

    if(nrow(flood_series) == 0) stop("There were no observed floods during this date range")

  #Summarize output by county
  if (dplyr::first(flood_series$flood) == "No Flood" | dplyr::first(flood_series$flood) == "Flood"){
    county_series <- flood_series %>%
      dplyr::group_by_(~ county_cd) %>%
      dplyr::group_by_(~ date) %>%
      dplyr::summarize_(county = ~dplyr::first(county),
                        state = ~dplyr::first(state),
                        num_gage = ~ n(),
                        max_peak = ~ max(flood_ratio, na.rm = TRUE),
                        avg_peak = ~ mean(flood_ratio, na.rm = TRUE),
                        no_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                    round(100 * sum((flood %in% c("No Flood")) /
                                                                      num_gage), 1)),
                        yes_flood = ~ dplyr::if_else(is.na(max_peak), -1,
                                                     round(100 * sum((flood %in% c("Flood")) /
                                                                       num_gage), 1))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_(~ county)         #sort by county name
  }else {
    county_series <- flood_series %>%
      dplyr::group_by_(~ county_cd) %>%
      dplyr::group_by_(~ date) %>%
      dplyr::summarize_(county = ~dplyr::first(county),
                        state = ~dplyr::first(state),
                       num_gage = ~ n(),
                       max_peak = ~ max(flood_ratio, na.rm = TRUE),
                       avg_peak = ~ mean(flood_ratio, na.rm = TRUE),
                       minor = ~ dplyr::if_else(is.na(max_peak), -1,
                                                round(100 * sum((flood %in% c("Minor",
                                                                              "Moderate",
                                                                              "Major",
                                                                              "Extreme")) /
                                                                  num_gage), 1)),
                       moderate = ~ dplyr::if_else(is.na(max_peak), -1,
                                                   round(100 * sum((flood %in% c("Moderate",
                                                                                 "Major",
                                                                                 "Extreme")) /
                                                                     num_gage), 1)),
                       major = ~ dplyr::if_else(is.na(max_peak), -1,
                                                round(100 * sum((flood %in% c("Major",
                                                                              "Extreme")) /
                                                                  num_gage), 1)),
                       extreme = ~ dplyr::if_else(is.na(max_peak), -1,
                                                  round(100 * sum((flood %in% c("Extreme")) /
                                                                    num_gage), 1))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_(~ county)         #sort by county name
  }

  return(list(flood_series, county_series))
}
