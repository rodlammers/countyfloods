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
#' @return A dataframe that gives the following variables for stream gages within
#'    the county and time range:
#'
#' \tabular{lll}{
#' Name \tab Type \tab Description\cr
#' agency_cd \tab character \tab Agency running the gage (typically will be the USGS)\cr
#' site_no \tab character \tab USGS gage ID\cr
#' station_nm \tab character \tab Name of the gage site\cr
#' site_tp_cd \tab character \tab Type of gage (should always be "ST" for stream)\cr
#' dec_lat_va \tab numeric \tab Latitude of the gage site, in decimal degrees\cr
#' dec_long_va \tab numeric \tab Longitude of the gage site, in decimal degrees\cr
#' county_cd \tab character \tab Five-digit FIPS code of gage county location\cr
#' DA \tab numeric \tab Drainage area of the gage, in square miles\cr
#' }
#'
#'  Note that the returned object is the same as that returned by the \code{whatNWISsites} funtion
#'  in the \code{dataRetrieval} package, but with county FIPS added for each gage.
#'
#' @examples
#' \dontrun{
#' get_gages("12086", start_date = "1988-01-01", end_date = "2015-01-01")
#'
#' va_counties <- get_county_cd("Virginia")
#' va_gages <- get_gages(va_counties, start_date = "2015-01-01",
#'                       end_date = "2015-12-31")
#' # Equivalent with piping
#' library(dplyr)
#' va_gages <- get_county_cd("Virginia") %>%
#'    get_gages(start_date = "2015-01-01", end_date = "2015-12-31")
#'}
#' @importFrom dplyr %>%
#'
#' @export
get_gages <- function(county_cd, start_date, end_date){

  #Check inputs and return error messages as necessary
  if(!is.character(county_cd)) stop("Input county_cd must be a character")

  #Check if more than 20 county_cd values, if so, split
  num_cc <- length(county_cd)
  num_run <- ceiling(num_cc / 20)
  gages_list <- list()

  county_cd_split <- split(county_cd, ceiling(seq_along(county_cd) / 20))

  gages_list <- lapply(county_cd_split, gage_extract, start_date, end_date)

  gages_df <- suppressWarnings(dplyr::bind_rows(gages_list))

  #If queryTime is a column (from older versions of dataRetrieval), then remove
  if ("queryTime" %in% colnames(gages_df)){
    gages_df <- dplyr::select_(gages_df, .dots = list(quote(-queryTime)))
  }

  #remove duplicates, and include only stream gages (no canals or tidal rivers)
  gages_df <- gages_df %>%
    dplyr::filter_(~ site_tp_cd == "ST") %>%
    dplyr::distinct_()

  #Get gage attributes and add drainage area and county code to data frame
  gage_attr <- dataRetrieval::readNWISsite(gages_df$site_no) %>%
    dplyr::select_(.dots = list("site_no", "state_cd", "county_cd", "drain_area_va"))

  #Match county code and drainage area to each gage
  gages_df <- gages_df %>%
    dplyr::left_join(gage_attr, by = "site_no") %>%
    dplyr::mutate_(county_cd2 = ~ paste0(state_cd, county_cd)) %>%
    dplyr::mutate_(DA = ~ drain_area_va) %>%
    dplyr::select_("agency_cd", "site_no", "station_nm", "site_tp_cd", "dec_lat_va",
                                "dec_long_va", "county_cd2", "DA") %>%
    dplyr::rename_(.dots = list(county_cd = "county_cd2"))

  return(gages_df)
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
#'    longitude, and county code. See the \code{whatNWISsites} function from
#'    the \code{dataRetrieval} package for details.
#'
#' @seealso \code{\link[dataRetrieval]{whatNWISsites}}
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

  #gages$county_cd <- county_cd

  return(gages)
}

#' Retrieve discharge data at specified gages
#'
#' Pulls all discharge data for the specified gage numbers and date range.
#'
#' @param gages_df A dataframe that includes the column \code{site_no}, a
#'    character vector with USGS gage IDs of stream gage sites to pull.
#' @inheritParams get_gages
#'
#' @return A dataframe with discharge data for each of the specified monitors.
#'   This is a dataframe that includes columns for the gage site number, date of
#'   each observation, and observed mean daily discharge (cubic feet per
#'   second).
#'
#' @seealso \code{\link[dataRetrieval]{readNWISdv}}
#'
#' @examples
#' \dontrun{
#' miami_gages <- get_gages("12086", start_date = "2000-01-01",
#'                             end_date = "2009-12-31")
#' miami_flow_data <- get_flow_data(gages_df = miami_gages,
#'                                  start_date = "2000-01-01",
#'                                  end_date = "2000-01-31")
#'# Example using piping
#' library(dplyr)
#' miami_flow_data <- get_gages("12086", start_date = "2000-01-01",
#'                                 end_date = "2009-12-31") %>%
#'                    get_flow_data(start_date = "2000-01-01",
#'                                  end_date = "2000-01-31")
#' }
#' @export
get_flow_data <- function(gages_df, start_date, end_date){

  #Check inputs and return error messages as necessary
  if(!is.data.frame(gages_df)) stop("gage_df must be a data.frame")

  flow_data <- lapply(gages_df$site_no, function(x){
    dataRetrieval::readNWISdv(siteNumber = x, parameterCd = "00060",
                              startDate = start_date, endDate = end_date)
  })

  #remove stations with no discharge data
  omit <- sapply(flow_data, function(x) {length(x)})
  flow_data <- lapply(flow_data[omit > 0], function(x) {x})

  #rename discharge and data columns prior to combining into a singe data frame
  flow_data <- lapply(flow_data, function(x) {
    colnames(x)[3] <- "date"
    colnames(x)[4] <- "discharge"
    return(x)
  })

  #Combine into one data frame and keep only site_no, date, and discharge
  #columns
  flow_data <- dplyr::bind_rows(flow_data) %>%
    dplyr::select_(~ site_no, ~ date, ~ discharge)

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
#' @details This function uses the \code{county.fips} dataset from the \code{maps}
#'    package to pull county FIPS for a state.
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
    dplyr::filter_(~ state %in% chosen_states)

  #put all fips codes in single array, adding a leading zero if codes are only
  # 4 digits long. Also, ensure that each FIPS code only is output once.
  fips_cd_array <- unique(sprintf("%05d", fips_cd$fips))

  return(fips_cd_array)
}
