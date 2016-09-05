#' Get all gage site numbers for a county
#'
#' Pulls gage numbers of all gages with discharge data within a county and
#' within the specified date range.
#'
#' @param county_cd Character vector with the county FIPS code
#' @param start_date Character string with the starting date, using "YYYY-MM-DD"
#'    notation.
#' @param end_date Character string with the end date, using "YYYY-MM-DD"
#'    notation.
#'
#' @return A dataframe with gage names and numbers for stream gages within
#'    the county and time range.
#'
#' @examples
#'
#' get_gages("12086", start_date = "1988-01-01", end_date = "2015-01-01")
#'
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' # Equivalent with piping
#' va_gages <- get_county_cd("Virginia") %>%
#'    get_gages(start_date = "2015-01-01", end_date = "2015-12-31")
#'
#' @importFrom dplyr %>%
#'
#' @export
get_gages <- function(county_cd, start_date, end_date){

  #Get gages by county code. This serves two purposes: first, the whatNWISsites function
  #has a limit of 20 county codes so calling by county avoids this issues; second, this
  #allows for the county code to be stored with each gage number
  safe_gage_extract <- purrr::safely(gage_extract, quiet = TRUE)
  gages <- lapply(county_cd, safe_gage_extract, start_date, end_date)

  check_data <- sapply(gages, function(x) is.null(x$result))

  gages_list <- lapply(gages[!check_data], function(x) x$result)
  gages_list <- suppressWarnings(dplyr::bind_rows(gages_list))

  #remove query time column and remove duplicates
  gages_list <- gages_list %>%
    dplyr::select_(.dots = list(quote(-queryTime))) %>%
    dplyr::distinct_()

  return(gages_list)
}

#' Get gage meta-data for a county
#'
#' This function uses the \code{whatNWISsites} function from the
#' \code{dataRetrieval} package to pull information on all stream gages within
#' a county and then adds the county FIPS code as an additional column to the
#' dataframe.
#'
#' @inheritParams get_gages
#'
#' @return A dataframe with information about stream gages within a county for
#'    a specified time frame. This information typically includes each gage's
#'    site number, station name, agency code, site type code, latitude,
#'    longitude, and county code.
#'
#' @examples
#'
#' gage_extract("12086", start_date = "2000-01-01", end_date = "2009-12-31")
#'
#' @export
gage_extract <- function(county_cd, start_date, end_date){
  gages <- dataRetrieval::whatNWISsites(countyCd = county_cd,
                                        hasDataTypeCd = "dv",
                                        parameterCd = c("00060"),
                                        startDT = start_date,
                                        endDT = end_date)

  gages$county_cd <- county_cd

  return(gages)
}

#' Retrieve discharge data at specified gages
#'
#' Pulls all discharge data for the specified gage numbers and date range.
#'
#' @param site_no Character vector with USGS gage IDs of stream gage sites to pull.
#' @inheritParams get_gages
#'
#' @return A list with discharge data for each of the specified monitors. The
#'    element of this list for each monitor is a dataframe that typically
#'    includes columns for the gage site number, date of each observation,
#'    [fill in other common outputs-- X_PUBLISHED_00060_00003 and
#'    X_PUBLISHED_00060_00003_cd].
#'
#' @examples
#'
#' miami_gages <- gage_extract("12086", start_date = "2000-01-01",
#'                             end_date = "2009-12-31")
#' miami_flow_data <- get_flow_data(site_no = miami_gages$site_no,
#'                                  start_date = "2000-01-01",
#'                                  end_date = "2000-01-31")
#'
#' @export
get_flow_data <- function(site_no, start_date, end_date){

  flow_data <- lapply(site_no, function(x){
    dataRetrieval::readNWISdv(siteNumber = x, parameterCd = "00060",
                              startDate = start_date, endDate = end_date)
  })


  #remove stations with no discharge data
  omit <- sapply(flow_data, function(x) {length(x)})
  flow_data <- lapply(flow_data[omit > 0], function(x) {x})

  #rename flow.data Q column
  flow_data <- lapply(flow_data, plyr::rename,
                      replace = c("X_00060_00003" = "Discharge"))
  return(flow_data)
}


#' Get all FIPS county codes within a state
#'
#' This function will return all county FIPS codes for all counties within a
#' state or states.
#'
#' @param state Character vector giving the name of state or states (not case
#'    sensitive) for which you would like to get county FIPS codes.
#'
#' @return A character vector with the 5-digit FIPS codes for all counties
#'    within the specified state or states.
#'
#' @examples
#'  get_county_cd("Virginia")
#'  get_county_cd(c("North Carolina", "South Carolina"))
#'
#' @importFrom dplyr %>%
#'
#' @export
get_county_cd <- function(state){

  chosen_states <- tolower(state)

  fips_cd <- fips_table %>%
    filter_(~ state %in% chosen_states)

  #put all fips codes in single array, adding a leading zero if codes are only
  # 4 digits long. Also, ensure that each FIPS code only is output once.
  fips_cd_array <- unique(sprintf("%05d",fips_cd$fips))

  return(fips_cd_array)
}
