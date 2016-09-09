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
#'
#' @return A data frame with output at either the gage or county level,
#'   depending on the value of "output". If output = "gage" a data frame with
#'   the following columns is returned:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   site_no \tab character \tab USGS gage ID\cr
#'   avg_peak \tab numeric \tab Mean flood ratio for date range
#'      (discharge/flood threshold)\cr
#'   flood_dur \tab numeric \tab Number of days in date range
#'      discharge above flood threshold\cr
#'   peak \tab numeric \tab Maximum value of flood ratio for date
#'      range (discharge/flood threshold)\cr
#'   num_missing \tab numeric \tab Number of days in given date
#'      range with no discharge data at that gage\cr
#'   lat \tab numeric \tab Gage latitude\cr
#'   long \tab numeric \tab Gage longitude\cr
#'   county_cd \tab character \tab FIPS code of gage county location\cr
#'   county \tab character \tab County name\cr
#'   state \tab character \tab State name\cr
#'   flood \tab character \tab Flood magnitude category based on peak }
#'
#'   If output = "county" a data frame with the following columns is returned:
#'
#'   \tabular{lll}{
#'   Name \tab Type \tab Description\cr
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
                      flood_type = "flood", output = "both"){

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

  #Determine if county codes or state name was provided. If state name given,
  #find all county codes in the state
  if (is.null(county_cd) & !is.null(state)) {
    county_cd <- get_county_cd(state)
  }

  #get gages
  gages <- get_gages(county_cd, start_date = start_date, end_date = end_date)

  #get flow data
  flow_data <- get_flow_data(gages, start_date = start_date,
                             end_date = end_date)

  #get flow peaks
  q2_val <- find_q2(gages$site_no)
  if (threshold == "Q2") {
    peaks <- q2_val
  }else if (threshold == "NWS") {
    peaks <- find_nws(gages$site_no, type = flood_type)
  }
  q2_val <- dplyr::rename_(q2_val, .dots = list(q2 = "flood_val"))

  #get flood stats by gage
  flood_stats <- flood_analysis(flow_data = flow_data, peaks = peaks, gages = gages,
                                county_cd = county_cd, q2_val = q2_val)

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


#This is currently non-functioning
# long_term_flood <- function(input_df, threshold = "Q2",
#                             flood_type = "flood") {
#
#   #get all gages and necessary flow data
#   min_date <- min(as.Date(input_df$start_date))
#   max_date <- max(as.Date(input_df$end_date))
#   county_cd_simple <- as.character(unique(input_df$county_cd))
#
#   #get gages
#   gages <- get_gages(county_cd = county_cd_simple, start_date = min_date, end_date = max_date)
#
#   #get flow data
#   flow_data <- get_flow_data(gages, start_date = min_date,
#                              end_date = max_date)
#
#   #get flow peaks
#   q2_val <- find_q2(gages$site_no)
#   if (threshold == "Q2") {
#     peaks <- q2_val
#   }else if (threshold == "NWS") {
#     peaks <- find_nws(gages$site_no, type = flood_type)
#   }
#   q2_val <- dplyr::rename_(q2_val, .dots = list(q2 = "flood_val"))
#
#   #get flood stats by gage for each time period
#   flood_analysis_loop <- function(flow_data, peaks, gages, county_cd, q2_val, start_date, end_date) {
#     flow_data_new <- flow_data %>%
#       dplyr::group_by_(~ site_no) %>%
#       dplyr::filter_(~ date >= start_date & date <= end_date)
#
#     flood_stats <- flood_analysis(flow_data = flow_data_new, peaks = peaks, gages = gages,
#                                   county_cd = county_cd, q2_val = q2_val)
#
#     return(flood_stats)
#   }
#
#   flood_stats <- plyr::adply(input_df, .margins = 1, flood_analysis_loop, flow_data = flow_data,
#                              peaks = peaks, gages = gages, q2_val = q2_val, county_cd = input_df$county_cd,
#                              start_date = input_df$start_date, end_date = input_df$end_date)
#
#   flood_stats <- input_df %>%
#     purrr::map_df(flood_analysis_loop(flow_data = flow_data,
#                         peaks = peaks, gages = gages, q2_val = q2_val))
#
#
# }


#Right now this returns any dates within the specified range where a flood occured at a gage.
#This output can be better summarized by both gage and county.
time_series_flood <- function(county_cd, start_date, end_date, threshold = "Q2", flood_type = "flood") {
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

  gages <- dplyr::filter_(gages, ~ site_no %in% q2_val$site_no)
  #get flow data
  flow_data <- get_flow_data(gages, start_date = start_date, end_date = end_date)



  #get flood stats by gage
  flood_stats <- time_series_analysis(flow_data = flow_data, peaks = peaks, gages = gages,
                                county_cd = county_cd, q2_val = q2_val)

}
