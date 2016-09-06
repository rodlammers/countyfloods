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
#'   analysis (either "Q2" or "NWS")
#' @param flood_type Character string of the defined flood type based on NWS
#'   classifications (one of "action", "flood", "moderate", or "major")
#' @param output Character string of output summary type (either "gage",
#'   "county", or "both"). Defaults to "gage".
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
#'
#' @examples
#' #Use Q2 as flood threshold and get get gage-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "Q2", output = "gage")
#'
#' #Use NWS flood thresholds and get county-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "NWS", flood_type = "action",
#'                       output = "county")

run_flood <- function(county_cd = NULL, state = NULL, start_date, end_date, threshold,
                      flood_type = "flood", output = "gage"){

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
  if (threshold == "Q2") {
    peaks <- find_q2(gages$site_no)
  }else if (threshold == "NWS") {
    peaks <- find_nws(gages$site_no, type = flood_type)
  }

  #get flood stats by gage
  flood_stats <- flood_analysis(flow_data, peaks = peaks, gages = gages)

  #get flood stats by county
  county_stats <- county_aggregates(flood_stats)

  if (output == "gage") {
    return(flood_stats)
  }else if (output == "county") {
    return(county_stats)
  }else if (output == "both") {
    return(list(flood_stats, county_stats))
  }
}
