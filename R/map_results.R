#' Function maps data, either by gage or by county
#'
#' Displays a state or multi-state map summarizing flood analysis results either
#' by gage or county.
#'
#' @param flood_stats Data frame of flood analysis results, output of
#'   \code{run_flood} function.
#'
#' @return A map of the state(s) analyzed showing counties and gages color coded
#'   based on flood magnitude, depending on the type of data in flood_stats.
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
map_flood <- function(flood_stats) {

  #Check if flood_stats data is at the gage- or county-level and call
  #appropriate mapping function
  if (!is.data.frame(flood_stats)) {
    output = "both"
  }else if (names(flood_stats)[1] == "site_no") {
    output = "gage"
  }else if (names(flood_stats)[1] == "county_cd") {
    output = "county"
  }

  if (output == "gage") {
    map_gage(flood_stats)
  }else if (output == "county") {
    map_county(flood_stats)
  }else if (output == "both") {
    map_gage(flood_stats[[1]])
    map_county(flood_stats[[2]])
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

  counties <- ggplot2::map_data('county', region = region)
  counties_sub <- subset(counties, subregion %in% flood_stats$county)
  ggplot2::ggplot(counties_sub, ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(fill = "gray95", color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes(x = long, y = lat, group = group),
                          fill = NA, color = "black") +
    ggplot2::geom_point(data = flood_stats, ggplot2::aes(x = long, y = lat, group = NA, fill = flood),
                        size = 4, alpha = 0.8, pch = 21, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw()
}

#' Maps flood data by county
#'
#' Creates a state level map of flood analysis output by county. Counties are
#' color coded based on the percentage of gages in that county at or exceeding a
#' given flood magnitude.
#'
#' @inheritParams map_flood
#' @param category Character string of the flood magnitude category to be used
#'   for mapping (one of "minor", "moderate", "major", or "extreme").
#'
#' @return A map of counties color coded by percentage of gages experiencing flooding.
#'
#' @export
map_county <- function(county_stats, category = "minor") {

  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4")
  exposure_cat <- c("Very High", "High", "Moderate-High", "Moderate", "Low")
  names(colors) <- exposure_cat

  #Set flood exposure "categories" based on percentage of flooded gages and the
  #user-defined metric (e.g. minor, major, extreme flooding)
  flood_cat <- function(county_stats, category) {
    flood_cat <- c("minor", "moderate", "major", "extreme")
    index <- match(category, flood_cat)
    if(county_stats[6 + index] < 20) {
      county_stats$cat <- "Low"
    }else if (county_stats[6 + index] < 40) {
      county_stats$cat <- "Moderate"
    }else if (county_stats[6 + index] < 60) {
      county_stats$cat <- "Moderate-High"
    }else if (county_stats[6 + index] < 80) {
      county_stats$cat <- "High"
    }else {
      county_stats$cat <- "Very High"
    }

    return(county_stats)
  }

  county_stats <- plyr::adply(county_stats, 1, flood_cat, category)
  county_stats$cat <- factor(county_stats$cat, levels = exposure_cat)

  region <- as.character(unique(county_stats$state))

  #Get all counties for states analyzed as well as the subset of counties with actual data
  counties <- ggplot2::map_data('county', region = region)
  counties_sub <- subset(counties, subregion %in% county_stats$county)

  counties_sub$cat <- county_stats$cat[match(counties_sub$subregion, county_stats$county)]

  ggplot2::ggplot(counties_sub, ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = cat), color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes(x = long, y = lat, group = group),
                          fill = NA, color = "black") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw()
}
