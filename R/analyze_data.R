
#Takes flow data and computes flood statistics based on selected flood definition
flood_analysis <- function(flow_data, peaks, gages){

  #Add Q2 values to flow_data data frame
  add_Q2 <- function(flow_data, peaks, gages){
    #Find matching Q2 value
    Q2 <- peaks$Q2[peaks$site_no %in% flow_data$site_no[1]]

    #If there is no site match, set Q2 to zero
    if (length(Q2) == 0) {Q2 <- NA}
    flow_data$Q2 <- Q2
    flow_data$flood_ratio <- flow_data$Discharge / flow_data$Q2

    #add lat-longs
    lat <- gages$dec_lat_va[gages$site_no %in% flow_data$site_no[1]]
    long <- gages$dec_long_va[gages$site_no %in% flow_data$site_no[1]]
    flow_data$lat <- lat
    flow_data$long <- long

    #add county codes
    fips_cd <- gages$county_cd[gages$site_no %in% flow_data$site_no[1]]
    flow_data$fips_cd <- fips_cd

    return(flow_data)
  }

  flow_data <- lapply(flow_data, add_Q2, peaks = peaks, gages = gages)

#   flow_data <- lapply(flow_data, function(x) {
#     x$flood_ratio <- x$Discharge / x$Q2
#     return(x)
#   })

  flood_stats <- plyr::ldply(flow_data, function(x) {
    avg_peak <- mean(x$flood_ratio, na.rm = TRUE)
    flood_dur <- sum(x$flood_ratio > 1, na.rm = TRUE)
    if (sum(!is.na(x$flood_ratio)) == 0){
      peak <- NA
    }else{
      peak <- max(x$flood_ratio, na.rm = TRUE)
    }

    fips_cd <- x$fips_cd[1]
    fips_cd <- as.numeric(fips_cd)
    maps_id <- maps::county.fips$polyname[maps::county.fips$fips %in% fips_cd]
    maps_id <- droplevels(maps_id)
    if (length(maps_id) == 0) {
      county <- NA
      state <- NA
      maps_id <- NA
    }else {
      location <- strsplit(as.character(maps_id), split = ",")
      county <- location[[1]][2]
      state <- location[[1]][1]
    }
    return(data.frame(site_no = x$site_no[1], avg_peak = avg_peak, flood_dur = flood_dur,
                      peak = peak, lat = x$lat[1], long = x$long[1], county_cd = fips_cd,
                      maps_id = maps_id, county = county, state = state))
  })

  #sort flood stats by magnitude
  flood_stats <- flood_stats[order(flood_stats$peak),]

  return(flood_stats)
}


