#' Return flood metrics by county codes or state names
#'
#' Access USGS databases to retrieve gages and flow data for the specified
#' counties/states or county FIPS codes and the specified date ranges. Flooding
#' at these gage locations are assessed by one of two metrics. Data can be
#' returned at the gage level or the county level.
#'
#' @param county_cd Character vector with the county FIPS code(s)
#' @param state Character vector of state names. Used to obtain county FIPS
#'   codes if county_cd is NULL
#' @param start_date Character string with the starting date, using "YYYY-MM-DD"
#'   notation.
#' @param end_date Character string with the end date, using "YYYY-MM-DD"
#'   notation.
#' @param threshold Character string of the flood threshold to be used in the
#'   analysis (either "Q2" or "NWS"). Defaults to "Q2".
#' @param flood_type Character string of the defined flood type based on NWS
#'   classifications (one of "action", "flood", "moderate", or "major")
#' @param output Character string of output summary type (either "gage",
#'   "county", or "both"). Defaults to "both".
#' @param weight Character string of variable to be used to scale by river size
#'   for weighted averages and scaling point sizes on maps. Options are median
#'   annual flood ("Q2") or drainage area ("DA"). Defaults to "Q2".
#'
#' @return A data frame with output at either the gage or county level,
#'   depending on the value of "output". If output = "gage" a data frame with
#'   the following columns is returned:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   site_no \tab character \tab USGS gage ID\cr
#'   county_cd \tab character \tab FIPS code of gage county location\cr
#'   lat \tab numeric \tab Gage latitude\cr
#'   long \tab numeric \tab Gage longitude\cr
#'   avg_peak \tab numeric \tab Mean flood ratio for date range
#'      (discharge/flood threshold)\cr
#'   flood_dur \tab numeric \tab Number of days in date range
#'      discharge above flood threshold\cr
#'   max_peak \tab numeric \tab Maximum value of flood ratio for date
#'      range (discharge/flood threshold)\cr
#'   num_missing \tab numeric \tab Number of days in given date
#'      range with no discharge data at that gage\cr
#'   Q2 \tab numeric \tab Median annual discharge (cubic feet per second)\cr
#'   DA \tab numeric \tab Drainage area of the gage (square miles)\cr
#'   size \tab numeric \tab Relative river size, logarithm of either Q2 or DA
#'      depending on user specified \code{weight}\cr
#'   state \tab character \tab State name\cr
#'   county \tab character \tab County name\cr
#'   flood \tab character \tab Flood magnitude category based on peak }
#'
#'   If output = "county" a data frame with the following columns is returned:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   county \tab character \tab County name\cr
#'   state \tab character \tab State name\cr
#'   num_gage \tab numeric \tab Number of analyzed gages in county\cr
#'   avg_peak \tab numeric \tab Average flood ratio among county gages\cr
#'   max_peak \tab numeric \tab Maximum observed flood ratio\cr
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
#' If output = "both" a list containing both data frames is returned.
#' In both cases, if data in counties were requested but not avaialable,
#' these counties are included as additional rows with data values of NA.
#'
#' @examples
#' #Use Q2 as flood threshold and get get gage-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "Q2",
#'                       output = "gage")
#'
#' #Use NWS flood thresholds and get county-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "NWS",
#'                       flood_type = "action",
#'                       output = "county")
#'
#' @export
run_flood <- function(county_cd = NULL, state = NULL, start_date, end_date, threshold = "Q2",
                      flood_type = "flood", output = "both", weight = "Q2"){

  #Check inputs and return error messages as necessary
  if(!is.character(county_cd) & !is.null(county_cd)) stop("Input county_cd must be a character")
  if(!is.character(state) & !is.null(state)) stop("Input state must be a character")
  if(is.null(county_cd) & is.null(state)) stop("must specify either county_cd or state")
  if(threshold != "Q2" & threshold != "NWS") stop("threshold must be set to either 'Q2' or 'NWS'")
  flood_type <- tolower(flood_type)
  if(flood_type != "action" & flood_type != "flood" & flood_type != "moderate" &
     flood_type != "major") stop("flood_type must be one of 'action', 'flood', 'moderate', or 'major'")
  output <- tolower(output)
  if(output != "gage" & output != "county" &
     output != "both") stop("output must be one of 'gage', 'county', or 'both'")
  if(weight != "Q2" & weight != "DA") stop("weight must be either 'Q2' or 'DA'")

  #Determine if county codes or state name was provided. If state name given,
  #find all county codes in the state
  if (is.null(county_cd) & !is.null(state)) {
    county_cd <- get_county_cd(state)
  }

  #get gages
  gages <- get_gages(county_cd, start_date = start_date, end_date = end_date)

  #get flow peaks
  q2_val <- find_q2(gages$site_no)
  if (threshold == "Q2") {
    peaks <- q2_val
  }else if (threshold == "NWS") {
    peaks <- find_nws(gages$site_no, type = flood_type)
  }
  q2_val <- dplyr::rename_(q2_val, .dots = list(q2 = "flood_val"))

  gages <- dplyr::filter_(gages, ~ site_no %in% peaks$site_no)

  #get flow data
  flow_data <- get_flow_data(gages, start_date = start_date,
                             end_date = end_date)

  #get flood stats by gage
  flood_stats <- flood_analysis(flow_data = flow_data, peaks = peaks, gages = gages,
                                county_cd = county_cd, q2_val = q2_val, threshold = threshold, weight = weight)

  #get flood stats by county
  county_stats <- county_aggregates(flood_stats = flood_stats, county_cd = county_cd)

  if (output == "gage") {
    return(flood_stats)
  }else if (output == "county") {
    return(county_stats)
  }else if (output == "both") {
    return(list(flood_stats, county_stats))
  }
}


#' Return flood metrics by county codes for a data frame input
#'
#' Access USGS databases to retrieve gages and flow data for the specified
#' county FIPS codes and the specified date ranges. Flooding
#' at these gage locations are assessed by one of two metrics. Data can be
#' returned at the gage level or the county level. This is the same as the
#' \code{run_flood} function but accepts a data frame as input with multiple
#' county codes and date ranges for each.
#'
#' @param input_df Data frame with three columns: \code{county_cd},
#' \code{start_date}, and \code{end_date}
#' @param threshold Character string of the flood threshold to be used in the
#'   analysis (either "Q2" or "NWS"). Defaults to "Q2".
#' @param flood_type Character string of the defined flood type based on NWS
#'   classifications (one of "action", "flood", "moderate", or "major")
#' @param weight Character string of variable to be used to scale by river size
#'   for weighted averages and scaling point sizes on maps. Options are median
#'   annual flood ("Q2") or drainage area ("DA"). Defaults to "Q2"
#'
#' @return A list with two data frames summarizing data by gage and by county.
#'
#'   Gage:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   start_date \tab date \tab Input start date\cr
#'   end_date \tab date \tab Input end date\cr
#'   site_no \tab character \tab USGS gage ID\cr
#'   county_cd \tab character \tab FIPS code of gage county location\cr
#'   lat \tab numeric \tab Gage latitude\cr
#'   long \tab numeric \tab Gage longitude\cr
#'   avg_peak \tab numeric \tab Mean flood ratio for date range
#'      (discharge/flood threshold)\cr
#'   flood_dur \tab numeric \tab Number of days in date range
#'      discharge above flood threshold\cr
#'   max_peak \tab numeric \tab Maximum value of flood ratio for date
#'      range (discharge/flood threshold)\cr
#'   num_missing \tab numeric \tab Number of days in given date
#'      range with no discharge data at that gage\cr
#'   Q2 \tab numeric \tab Median annual discharge (cubic feet per second)\cr
#'   DA \tab numeric \tab Drainage area of the gage (square miles)\cr
#'   size \tab numeric \tab Relative river size, logarithm of either Q2 or DA
#'      depending on user specified \code{weight}\cr
#'   state \tab character \tab State name\cr
#'   county \tab character \tab County name\cr
#'   flood \tab character \tab Flood magnitude category based on peak }
#'
#'   County:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   county_cd \tab character \tab FIPS code of gage county location\cr
#'   start_date \tab date \tab Input start date\cr
#'   end_date \tab date \tab Input end date\cr
#'   county \tab character \tab County name\cr
#'   state \tab character \tab State name\cr
#'   num_gage \tab numeric \tab Number of analyzed gages in county\cr
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
#' #Northern VA flooding every April
#' county_cd <- c(rep("51013", 5), rep("51107", 5), rep("51059", 5))
#' start_date <- rep(c("2010-04-01", "2011-04-01", "2012-04-01", "2013-04-01", "2014-04-01"), 3)
#' end_date <- rep(c("2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", "2014-04-30"), 3)
#' input_df <- data.frame(county_cd = county_cd, start_date = start_date, end_date = end_date,
#'                       stringsAsFactors = FALSE)
#'
#' #With default values
#' VA_floods <- long_term_flood(input_df)
#'
#' #Using NWS values
#' VA_floods <- long_term_flood(input_df, threshold = "NWS")
#'
#' @importFrom dplyr %>%
#'
#' @export
long_term_flood <- function(input_df, threshold = "Q2",
                            flood_type = "flood", weight = "Q2") {

  #Check inputs and return error messages as necessary
  if(!is.character(input_df$county_cd) | !is.character(input_df$start_date) |
      !is.character(input_df$end_date))
    stop("input_df must have characters, not factors (see data.frame(stringsAsFactors = FALSE))")
  if(threshold != "Q2" & threshold != "NWS") stop("threshold must be set to either 'Q2' or 'NWS'")
  flood_type <- tolower(flood_type)
  if(flood_type != "action" & flood_type != "flood" & flood_type != "moderate" &
     flood_type != "major") stop("flood_type must be one of 'action', 'flood', 'moderate', or 'major'")
  if(weight != "Q2" & weight != "DA") stop("weight must be either 'Q2' or 'DA'")

  #get all gages and necessary flow data
  min_date <- min(as.Date(input_df$start_date))
  max_date <- max(as.Date(input_df$end_date))
  county_cd_simple <- as.character(unique(input_df$county_cd))

  #get gages
  gages <- get_gages(county_cd = county_cd_simple, start_date = min_date, end_date = max_date)

  #get flow peaks
  q2_val <- find_q2(gages$site_no)
  if (threshold == "Q2") {
    peaks <- q2_val
  }else if (threshold == "NWS") {
    peaks <- find_nws(gages$site_no, type = flood_type)
  }
  q2_val <- dplyr::rename_(q2_val, .dots = list(q2 = "flood_val"))

  gages <- dplyr::filter_(gages, ~ site_no %in% peaks$site_no)

  #get flow data
  flow_data <- get_flow_data(gages, start_date = min_date,
                             end_date = max_date)

  #get flood stats by gage for each time period
  flood_analysis_loop <- function(flow_data, peaks, gages, county_cd, q2_val, start_date, end_date, threshold) {
    county_cd1 <- county_cd
    flow_data_new <- flow_data %>%
      dplyr::left_join(gages, by = "site_no") %>%
      dplyr::group_by_(~ site_no) %>%
      dplyr::filter_(~ date >= start_date & date <= end_date) %>%
      dplyr::filter_(~ county_cd == county_cd1) %>%
      dplyr::select_(.dots = list("site_no", "date", "discharge"))


    flood_stats <- flood_analysis(flow_data = flow_data_new, peaks = peaks, gages = gages,
                                  county_cd = county_cd, q2_val = q2_val, threshold = threshold,
                                  weight = weight)

    return(flood_stats)
  }

  flood_stats <- input_df %>%
    plyr::mdply(.fun = flood_analysis_loop, flow_data = flow_data,
                 peaks = peaks, gages = gages, q2_val = q2_val, threshold = threshold) %>%
    dplyr::arrange_(~ county) %>%
    dplyr::select_(~ site_no, ~ dplyr::everything())

#   flood_stats <- flood_stats
#     subset(select = c(site_no, start_date:flood))

  #summarize by date range and county
  county_stats <- county_aggregates2(flood_stats = flood_stats, county_cd = county_cd_simple)

  return(list(flood_stats, county_stats))

}


#' Return a time series of flood metrics by county codes or state names
#'
#' Access USGS databases to retrieve gages and flow data for the specified
#' counties/states or county FIPS codes and the specified date ranges. Flooding
#' at these gage locations are assessed by one of two metrics. Data on timing
#' and magnitude of flooding will be returned at the gage level or the county
#' level.
#'
#' @param county_cd Character vector with the county FIPS code(s)
#' @param state Character vector of state names. Used to obtain county FIPS
#'   codes if county_cd is NULL
#' @param start_date Character string with the starting date, using "YYYY-MM-DD"
#'   notation.
#' @param end_date Character string with the end date, using "YYYY-MM-DD"
#'   notation.
#' @param threshold Character string of the flood threshold to be used in the
#'   analysis (either "Q2" or "NWS"). Defaults to "Q2".
#' @param flood_type Character string of the defined flood type based on NWS
#'   classifications (one of "action", "flood", "moderate", or "major")
#' @param weight Character string of variable to be used to scale by river size
#'   for weighted averages and scaling point sizes on maps. Options are median
#'   annual flood ("Q2") or drainage area ("DA"). Defaults to "Q2".
#' @param Q2_magnitude Character string of ratio of daily streamflow to Q2 used
#'   as a binary flood threshold. One of "Minor" (1 < Flow / Q2 < 1.5), "Moderate" (< 2),
#'   "Major" (< 5), and "Extreme" (> 5). Defaults to "Moderate".
#' @param filter_data Logical. If TRUE only dates with a flood occurring are returned for
#'   both gage and county-level data. If FALSE, all dates are returned.
#'
#' @return A list with two data frames, summarizing the results by gage and by county:
#'
#' Gage:
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' site_no \tab character \tab USGS gage ID\cr
#' date \tab date \tab Date of observation\cr
#' lat \tab numeric \tab Gage latitude\cr
#' long \tab numeric \tab Gage longitude\cr
#' county_cd \tab character \tab FIPS code of gage county location\cr
#' Q2 \tab numeric \tab Median annual discharge (cubic feet per second)\cr
#' DA \tab numeric \tab Drainage area of the gage (square miles)\cr
#' size \tab numeric \tab Relative river size, logarithm of either Q2 or DA
#'      depending on user specified \code{weight}\cr
#' discharge \tab numeric \tab Observed mean daily discharge (cubic feet per second)\cr
#' flood_val \tab numeric \tab Selected threshold flood value. Either Q2 or an NWS flood threshold.\cr
#' flood_ratio \tab numeric \tab Ratio of the observed discharge divided by the defined flood threshold\cr
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
#' num_gage \tab numeric \tab Number of analyzed gages in county\cr
#' max_peak \tab numeric \tab Maximum observed flood ratio\cr
#' avg_peak \tab numeric \tab Average flood ratio among county gages\cr
#' minor \tab numeric \tab Percentage of gages at or above "minor" flood class (flood ratio > 1)\cr
#' moderate \tab numeric \tab Percentage of gages at or above "moderate" flood class (flood ratio > 1.5)\cr
#' major \tab numeric \tab Percentage of gages at or above "major" flood class (flood ratio > 2)\cr
#' extreme \tab numeric \tab Percentage of gages at or above "extreme" flood class (flood ratio > 5)\cr
#' flood_metric \tab numeric \tab Fraction of gages in county experiencing a flood, weighted by river size
#'      (\code{size} from gage-level output)
#' }
#'
#' If threshold = "NWS", the columns "minor", "moderate", "major", and "extreme"
#' are replaced with two columns: "no_flood" and "yes_flood" which show the
#' percentage of gages in the county with or without flooding.
#'
#' @examples
#' #Use Q2 as flood threshold
#' va_time_series <- time_series_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "Q2")
#'
#' #Use NWS flood thresholds
#' va_time_series <- time_series_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "NWS",
#'                       flood_type = "action")
#'
#' @export
time_series_flood <- function(county_cd = NULL, state = NULL, start_date, end_date,
                              threshold = "Q2", flood_type = "flood", weight = "Q2",
                              Q2_magnitude = "Moderate", filter_data = TRUE) {


  #Check inputs and return error messages as necessary
  if(!is.character(county_cd) & !is.null(county_cd)) stop("Input county_cd must be a character")
  if(!is.character(state) & !is.null(state)) stop("Input state must be a character")
  if(is.null(county_cd) & is.null(state)) stop("must specify either county_cd or state")
  if(threshold != "Q2" & threshold != "NWS") stop("threshold must be set to either 'Q2' or 'NWS'")
  flood_type <- tolower(flood_type)
  if(flood_type != "action" & flood_type != "flood" & flood_type != "moderate" &
     flood_type != "major") stop("flood_type must be one of 'action', 'flood', 'moderate', or 'major'")
  if(weight != "Q2" & weight != "DA") stop("weight must be either 'Q2' or 'DA'")
  Q2_magnitude <- tolower(Q2_magnitude)
  if(Q2_magnitude != "minor" & Q2_magnitude != "moderate" & Q2_magnitude != "major" &
     Q2_magnitude != "extreme") stop("Q2_magnitude must be one of 'minor', 'moderate', 'major', or 'extreme'")
  if(!is.logical(filter_data)) stop("filter_data must be TRUE or FALSE")

  #Determine if county codes or state name was provided. If state name given,
  #find all county codes in the state
  if (is.null(county_cd) & !is.null(state)) {
    county_cd <- get_county_cd(state)
  }

  #get gages
  gages <- get_gages(county_cd = county_cd, start_date = start_date, end_date = end_date)

  #get flow peaks
  q2_val <- find_q2(gages$site_no)
  if (threshold == "Q2") {
    peaks <- q2_val
  }else if (threshold == "NWS") {
    peaks <- find_nws(gages$site_no, type = flood_type)
  }
  q2_val <- dplyr::rename_(q2_val, .dots = list(q2 = "flood_val"))

  gages <- dplyr::filter_(gages, ~ site_no %in% peaks$site_no)

  #get flow data
  flow_data <- get_flow_data(gages, start_date = start_date, end_date = end_date)

  #get flood stats
  flood_stats <- time_series_analysis(flow_data = flow_data, peaks = peaks, gages = gages,
                                county_cd = county_cd, q2_val = q2_val, threshold = threshold,
                                weight = weight, Q2_magnitude = Q2_magnitude, filter_data = filter_data)

}
