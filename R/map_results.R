#' Function maps data, either by gage or by county
#'
#' Displays a state or multi-state map summarizing flood analysis results either
#' by gage or county.
#'
#' @param flood_stats Either a data frame of flood analysis results, by gage or by county,
#'   or a list of both data frames.
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
#' \dontrun{
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
#' }
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
    return(list(gage_map, county_map))
  }
}

#' Maps flood data by gage
#'
#' Creates a state level map of flood analysis output by USGS gage. Gages are
#' color coded based on maximum flood magnitude (for flood threshold = "Q2").
#' If the flood threshold is "NWS", points are binary coded based on flood
#' occurrence (e.g. yes/no).
#'
#' @param flood_stats Data frame of flood analysis results, by gage.
#'
#' @return A map of USGS gages color coded by maximum flood magnitude.
#'
#' @importFrom dplyr %>%
#'
#' @export
map_gage <- function(flood_stats) {

  if (dplyr::first(flood_stats$flood) == "No Flood" | dplyr::first(flood_stats$flood) == "Flood"){
    colors <- colors <- c("#993404", "#FFFFFF")
    names(colors) <- c("Flood", "No Flood")
  } else {
    colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFFF")
    names(colors) <- c("Extreme", "Major", "Moderate", "Minor", "None")
  }

  #Check "size" variable, if NA, use other metric (Q2 or DA)
  if (sum(!is.na(flood_stats$size)) == 0 && sum(!is.na(flood_stats$Q2)) == 0) {
    #Q2 NA, replace size with DA
    flood_stats <- flood_stats %>%
      dplyr::mutate_(size = ~ replace(size, is.na(size), log10(DA)))
    warning("Point size based on log10(DA).")
  }else if(sum(!is.na(flood_stats$size)) == 0 && sum(!is.na(flood_stats$Q2)) == 0) {
    #DA NA, replace size with Q2
    flood_stats <- flood_stats %>%
      dplyr::mutate_(size = ~ replace(size, is.na(size), log10(Q2)))
    warning("Point size based on log10(Q2).")
  }


  region <- as.character(unique(flood_stats$state))

  counties <- ggplot2::map_data("county", region = region)
  counties_sub <- counties[counties$subregion %in% flood_stats$county[!is.na(flood_stats$lat)], ]
  counties_sub_ND <- counties[counties$subregion %in% flood_stats$county[is.na(flood_stats$lat)], ]
  ggplot2::ggplot(counties_sub, ggplot2::aes_string(x = "long", y = "lat", group = "group")) +
    ggplot2::geom_polygon(fill = "gray95", color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                          fill = NA, color = "black") +
    ggplot2::geom_polygon(data = counties_sub_ND, ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                          fill = "gray60", color = "black") +
    ggplot2::geom_point(data = flood_stats, ggplot2::aes_string(x = "long", y = "lat", group = NA, fill = "flood",
                        size = "size"), alpha = 0.8, pch = 21, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::coord_map() +
    ggplot2::theme_void()
}

#' Maps flood data by county
#'
#' Creates a state level map of flood analysis output by county. Counties are
#' color coded based on the percentage of gages in that county at or exceeding a
#' given flood magnitude.
#'
#' @param county_stats Data frame of flood analyasis results, summarized by
#'   county.
#' @param category Character string of the flood magnitude category to be used
#'   for mapping (one of "minor", "moderate", "major", or "extreme"). This
#'   parameter only works when mapping county-level, rather than gage-level,
#'   values.
#'
#' @return A map of counties color coded by percentage of gages experiencing flooding.
#'
#' @importFrom dplyr %>%
#'
#' @export
map_county <- function(county_stats, category = "minor") {

  #Check inputs and return error messages as necessary
  category <- tolower(category)
  if(category != "minor" & category != "moderate" & category != "major" &
     category != "extreme") stop("Input category must be one of 'minor', moderate', 'major', or 'extreme'")

  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4", "#bfbfbf")
  exposure_cat <- c("Very High", "High", "Moderate-High", "Moderate", "Low", "No Data")
  names(colors) <- exposure_cat

  #Set flood exposure "categories" based on percentage of flooded gages and the
  #user-defined metric (e.g. minor, major, extreme flooding)
  if ("no_flood" %in% colnames(county_stats)){
    county_stats <- county_stats %>%
      dplyr::select_(.dots = list("county", "state", "no_flood",
                                  "yes_flood")) %>%
      tidyr::gather_(key_col = "key", value_col = "value",
                     gather_cols = c("no_flood", "yes_flood")) %>%
      dplyr::filter_(~ key == "yes_flood") %>%
      dplyr::mutate_(cat = ~ cut(value, breaks = c(-1, 0, 20, 40, 60, 80, 100),
                                 labels = c("No Data", "Low", "Moderate", "Moderate-High",
                                            "High", "Very High"),
                                 include.lowest = TRUE, right = FALSE))
  }else {
    county_stats <- county_stats %>%
      dplyr::select_(.dots = list("county", "state", "minor",
                                  "moderate", "major", "extreme")) %>%
      tidyr::gather_(key_col = "key", value_col = "value",
                    gather_cols = c("minor", "moderate", "major", "extreme")) %>%
      dplyr::filter_(~ key == category) %>%
      dplyr::mutate_(cat = ~ cut(value, breaks = c(-1, 0, 20, 40, 60, 80, 100),
                              labels = c("No Data", "Low", "Moderate", "Moderate-High",
                                         "High", "Very High"),
                              include.lowest = TRUE, right = FALSE))
  }

  region <- as.character(unique(county_stats$state))

  #Get all counties for states analyzed as well as the subset of counties with
  # actual data
  counties <- ggplot2::map_data("county", region = region)
  counties_sub <- counties[counties$subregion %in% county_stats$county, ]

  counties_sub$cat <- county_stats$cat[match(counties_sub$subregion,
                                             county_stats$county)]

  ggplot2::ggplot(counties_sub, ggplot2::aes_string(x = "long", y = "lat", group = "group")) +
    ggplot2::geom_polygon(ggplot2::aes_string(fill = "cat"), color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                          fill = NA, color = "black") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::coord_map() +
    ggplot2::theme_void()
}

#' Function plots time series data by county
#'
#' Displays four time series bar charts per county displaying the number of
#' gages with flooding, maximum flood ratio, average flood ratio, and the
#' percent of gages above a specified flood threshold.
#'
#' @param county_series Data frame of flood time series results by county,
#'   output of \code{time_series_flood} function.
#' @param category Character string of the flood magnitude category to be used
#'   for mapping (one of "minor", "moderate", "major", or "extreme").
#' @param start_date Character string of start date for x-axis of plots. If not
#'   specified, defaults to the earliest observed flood in the data.
#' @param end_date Character string of end date for x-axis of plots. If not
#'   specified, defaults to the latest observed flood in the data.
#'
#' @return Four time series bar charts per county displaying the number of
#' gages with flooding, maximum flood ratio, average flood ratio, and the
#' percent of gages above a specified flood threshold.
#'
#' @examples
#' \dontrun{
#' #Use Q2 as flood threshold
#' va_time_series <- time_series_flood(state = "Virginia", start_date = "2015-01-01",
#'                       end_date = "2015-12-31", threshold = "Q2")
#'
#' #Map results
#' time_series_plot(va_time_series[[2]])
#' }
#' @export
time_series_plot <- function(county_series, category = "moderate",
                             start_date = NULL, end_date = NULL) {

  if(is.null(start_date)) {start_date <- min(county_series$date)}
  if(is.null(end_date)) {end_date <- max(county_series$date)}

  if ("no_flood" %in% colnames(county_series)){
    category = "yes_flood"
  }

  no_output <- suppressWarnings(plyr::ddply(county_series, "county", function(x) {

  p1 <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "date", y = "num_gage")) +
    ggplot2::geom_bar(stat = "identity", width = 10) +
    ggplot2::xlim(start_date, end_date) +
    ggplot2::ylab("Gages") +
    ggplot2::xlab("Date") +
    ggplot2::ggtitle(substitute(paste("Number of gages with a flood (", county, " County)"),
                                list(county = R.utils::capitalize(unique(x$county))))) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p2 <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "date", y = "max_peak")) +
    ggplot2::geom_bar(stat = "identity", width = 10) +
    ggplot2::xlim(start_date, end_date) +
    ggplot2::ylab("Flood Ratio") +
    ggplot2::xlab("Date") +
    ggplot2::ggtitle("Maximum flood ratio") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p3 <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "date", y = "avg_peak")) +
    ggplot2::geom_bar(stat = "identity", width = 10) +
    ggplot2::xlim(start_date, end_date) +
    ggplot2::ylab("Flood Ratio") +
    ggplot2::xlab("Date") +
    ggplot2::ggtitle("Average flood ratio") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p4 <- ggplot2::ggplot(data = x, ggplot2::aes_(x = ~ date, y = ~ x[ ,tolower(category)])) +
    ggplot2::geom_bar(stat = "identity", width = 10) +
    ggplot2::xlim(start_date, end_date) +
    ggplot2::ylab("% Above") +
    ggplot2::xlab("Date") +
    ggplot2::ggtitle("Percent of gages above flood threshold") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  grid::grid.newpage()
  grid::grid.draw(rbind(ggplot2::ggplotGrob(p1), ggplot2::ggplotGrob(p2), ggplot2::ggplotGrob(p3),
                        ggplot2::ggplotGrob(p4), size = "last"))

  return(NA)
  }))

}
