#
# map_data <- function(flood_stats){
#
#   #drop NA rows
#   flood_stats <- flood_stats[!is.na(flood_stats$peak), ]
#
#   data(usaMapEnv)
#   data(stateMapEnv)
#   data(countyMapEnv)
#
#   maps_id <- unique(flood_stats$maps_id)
#   maps_states <- unique(flood_stats$state)
#
#   colors <- colorRampPalette(c("blue","yellow"))(length(flood_stats$peak))
#   maps::map('county', regions = maps_id, fill = TRUE, col = "gray80")
#   maps::map('county', regions = maps_states, add = TRUE)
#   points(flood_stats$long, flood_stats$lat, pch = 19, cex = 1.5, col = colors)
# }

map_function <- function(flood_stats) {
  #drop NA rows
  flood_stats <- flood_stats[!is.na(flood_stats$peak), ]

  #colors <- colorRampPalette(c("yellow","blue"))(5)#(length(unique(flood_stats$flood)))
  #midpoint <- min((max(flood_stats$peak) - min(flood_stats$peak)) / 2, 2.5)
  colors <- c("#993404", "#D95F0E", "#FE9929", "#FED98E", "#FFFFD4")
  names(colors) <- c("Extreme", "Major", "Moderate", "Minor", "None")

  #Create categories of flood peaks for display
  flood_cat <- function(flood_stats) {
    if (flood_stats$peak < 1) {
      flood_stats$flood <- "None"
    }else if (flood_stats$peak < 1.5) {
      flood_stats$flood <- "Minor"
    }else if (flood_stats$peak < 2) {
      flood_stats$flood <- "Moderate"
    }else if (flood_stats$peak < 5) {
      flood_stats$flood <- "Major"
    }else {
      flood_stats$flood <- "Extreme"
    }

    return(flood_stats)
  }

  flood_stats <- plyr::adply(flood_stats, 1, flood_cat)
  flood_stats$flood <- as.factor(flood_stats$flood)
  flood_stats$flood <- factor(flood_stats$flood, levels(flood_stats$flood)[c(1,2,4,3,5)])

  region <- as.character(unique(flood_stats$state))

  counties <- ggplot2::map_data('county', region = region)
  counties_sub <- subset(counties, subregion %in% test$county)
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
