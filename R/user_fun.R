#' Function returns flood metrics for inputted county codes or state names
#'
#' Access USGS databases to retrieve gages and flow data for the specified
#' counties/states and the specified date ranges. Flooding at these gage
#' locations are assessed by one of two metrics. Data can be returned at
#' the gage level or the county level.
#'
#' @param county_cd Character vector with the county FIPS code
#' @param state Character vector of state names
#' @param start_date Character string with the starting date, using "YYYY-MM-DD"
#'    notation.
#' @param end_date Character string with the end date, using "YYYY-MM-DD"
#'    notation.
#' @param threshold Character string of the flood threshold to be used in the
#'    analysis (either "Q2" or "NWS")
#' @param flood_type Character string of the defined flood type based on NWS
#'    classifications (one of "action", "flood", "moderate", or "major")
#' @param output Character string of output summary type (either "gage" or
#'    "county")
#'
#' @return A dataframe with gage names, locations, observed flood peaks, and flood
#'    durations (if output = "gage") or a datframe with county names and summary
#'    statistics of flooding at gages in those counties (if output = "county")
#'
#'

run_flood <- function(county_cd = NULL, state = NULL, start_date, end_date, threshold,
                      flood_type = NULL, output = "gage"){

  #Determine if county codes or state name was provided. If state name given, find all county
  #codes in the state
  if (is.null(county_cd) & !is.null(state)) {
    county_cd <- get_county_cd(state)
  }

  #get gages
  gages <- get_gages(county_cd, start_date, end_date)

  #get flow data
  flow_data <- get_flow_data(gages$site_no, start_date, end_date)

  #get flow peaks
  if (threshold == "Q2") {
    peaks <- find_Q2(gages$site_no)
  }else if (threshold == "NWS") {
    peaks <- find_NWS(gages$site_no, flood_type)
  }

  #get flood stats by gage
  flood_stats <- flood_analysis(flow_data, peaks, gages)

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

map_flood <- function(flood_stats){
  map_function(flood_stats)
}



