#Function maps data, either by gage or by county

map_function <- function(flood_stats) {

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

map_gage <- function(flood_stats) {

  #colors <- colorRampPalette(c("yellow","blue"))(5)#(length(unique(flood_stats$flood)))
  #midpoint <- min((max(flood_stats$peak) - min(flood_stats$peak)) / 2, 2.5)
  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4")
  names(colors) <- c("Extreme", "Major", "Moderate", "Minor", "None")

  region <- as.character(unique(flood_stats$state))

  counties <- ggplot2::map_data('county', region = region)
  counties_sub <- subset(counties, subregion %in% flood_stats$county)
  ggplot2::ggplot(counties_sub, ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(fill = "gray95", color = "black") +
    ggplot2::geom_polygon(data = counties, ggplot2::aes(x = long, y = lat, group = group),
                          fill = NA, color = "black") +
    #ggplot2::geom_point(data = flood_stats, ggplot2::aes(x = long, y = lat, group = NA, color = peak),
                        #size = 4, alpha = 0.8, show.legend = FALSE) +
    ggplot2::geom_point(data = flood_stats, ggplot2::aes(x = long, y = lat, group = NA, fill = flood),
                        size = 4, alpha = 0.8, pch = 21, show.legend = TRUE) +
    #ggplot2::scale_color_gradient2(low = "black", mid = "blue", high = "yellow", midpoint = midpoint, space = "Lab") +
    ggplot2::scale_fill_manual(values = colors) +
    #viridis::scale_color_viridis() +
    ggplot2::theme_bw()
    #ggplot2::theme(legend.position = "none")
}

map_county <- function(county_stats, category = "minor") {

  #colors <- colorRampPalette(c("yellow","blue"))(5)#(length(unique(flood_stats$flood)))
  #midpoint <- min((max(flood_stats$peak) - min(flood_stats$peak)) / 2, 2.5)
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
    #ggplot2::scale_color_gradient2(low = "black", mid = "blue", high = "yellow", midpoint = midpoint, space = "Lab") +
    ggplot2::scale_fill_manual(values = colors) +
    #viridis::scale_color_viridis() +
    ggplot2::theme_bw()
  #ggplot2::theme(legend.position = "none")
}
