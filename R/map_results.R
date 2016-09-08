#' Function maps data, either by gage or by county
#'
#' Displays a state or multi-state map summarizing flood analysis results either
#' by gage or county.
#'
#' @param flood_stats Data frame of flood analysis results, output of
#'   \code{run_flood} function.
#' @param category Character string of the flood magnitude category to be used
#'   for mapping (one of "minor", "moderate", "major", or "extreme"). This
#'   parameter only works when mapping county-level, rather than gage-level,
#'   values.
#'
#' @return A map of the state(s) analyzed showing counties and gages color coded
#'   based on flood magnitude, depending on the type of data in flood_stats. Gage
#'   flood thresholds are "None" (flood_ratio < 1), "Minor" (flood_ratio < 1.5),
#'   "Moderate" (flood_ratio < 2), "Major" (flood_ratio < 5), and "Extreme"
#'   (flood_ratio > 5). For county aggregate maps, flood exposure is assessed based
#'   on the percentage of gages in the county at or above a specified flood threshold.
#'   Exposure categories include "Low" (0% - 20%), "Moderate" (20% - 40%), "Moderate-High"
#'   (40% - 60%), "High" (60% - 80%), and "very High" (80% - 100%).
#'
#' @examples
#' #Use Q2 as flood threshold and get get gage-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "Q2", output = "gage")
#' #Map results by gage
#' map_flood(va_floods)
#'
#'
#' #Use NWS flood thresholds and get county-level output
#' va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "NWS", flood_type = "action",
#'                       output = "county")
#' #Map results by county
#' map_flood(va_floods)
#'
#' @export
map_flood <- function(flood_stats, category = "minor") {

  #Check if flood_stats data is at the gage- or county-level and call
  #appropriate mapping function
  if (!is.data.frame(flood_stats)) {
    output <- "both"
  }else if (names(flood_stats)[1] == "site_no") {
    output <- "gage"
  }else if (names(flood_stats)[1] == "county_cd") {
    output <- "county"
  }

  if (output == "gage") {
    map_gage(flood_stats)
  }else if (output == "county") {
    map_county(flood_stats, category = category)
  }else if (output == "both") {
    gage_map <- map_gage(flood_stats[[1]])
    county_map <- map_county(flood_stats[[2]], category = category)
    return(list(gage_map = gage_map, county_map = county_map))
  }
}

#' Maps flood data by gage
#'
#' Creates a state level map of flood analysis output by USGS gage. Gages are
#' color coded based on maximum flood magnitude.
#'
#' @inheritParams map_flood
#'
#' @return A map of USGS gages color coded by maximum flood magnitude.
#'
#' @export
map_gage <- function(flood_stats) {

  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4")
  names(colors) <- c("Extreme", "Major", "Moderate", "Minor", "None")

  region <- as.character(unique(flood_stats$state))

  counties <- ggplot2::map_data("county", region = region)
  counties_sub <- subset(counties, subregion %in% flood_stats$county)
  ggplot2::ggplot(counties_sub, ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(fill = "gray95", color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes(x = long, y = lat, group = group),
                          fill = NA, color = "black") +
    ggplot2::geom_point(data = flood_stats, ggplot2::aes(x = long, y = lat, group = NA, fill = flood),
                        size = 4, alpha = 0.8, pch = 21, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_void()
}

#' Maps flood data by county
#'
#' Creates a state level map of flood analysis output by county. Counties are
#' color coded based on the percentage of gages in that county at or exceeding a
#' given flood magnitude.
#'
#' @inheritParams map_flood
#'
#' @return A map of counties color coded by percentage of gages experiencing flooding.
#'
#' @export
map_county <- function(county_stats, category = "minor") {

  #Check inputs and return error messages as necessary
  category <- tolower(category)
  if(category != "minor" & category != "moderate" & category != "major" &
     category != "extreme") stop("Input category must be one of 'minor', moderate', 'major', or 'extreme'")

  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4")
  exposure_cat <- c("Very High", "High", "Moderate-High", "Moderate", "Low")
  names(colors) <- exposure_cat

  #Set flood exposure "categories" based on percentage of flooded gages and the
  #user-defined metric (e.g. minor, major, extreme flooding)
  county_stats <- county_stats %>%
    dplyr::select_(.dots = list("county", "state", "minor",
                                "moderate", "major", "extreme")) %>%
    tidyr::gather_(key_col = "key", value_col = "value",
                  gather_cols = c("minor", "moderate", "major", "extreme")) %>%
    dplyr::filter_(~ key == category) %>%
    dplyr::mutate_(cat = ~ cut(value, breaks = c(0, 20, 40, 60, 80, 100),
                            labels = c("Low", "Moderate", "Moderate-High",
                                       "High", "Very High"),
                            include.lowest = TRUE, right = FALSE))

  region <- as.character(unique(county_stats$state))

  #Get all counties for states analyzed as well as the subset of counties with
  # actual data
  counties <- ggplot2::map_data("county", region = region)
  counties_sub <- subset(counties, subregion %in% county_stats$county)

  counties_sub$cat <- county_stats$cat[match(counties_sub$subregion,
                                             county_stats$county)]

  ggplot2::ggplot(counties_sub, ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = cat), color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes(x = long, y = lat, group = group),
                          fill = NA, color = "black") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_void()
}
