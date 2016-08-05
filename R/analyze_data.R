#' Compute flood statistics
#'
#'  Takes flow data and computes flood statistics based on selected flood
#' definition.
flood_analysis <- function(flow_data, peaks, gages){

  #Add Q2 values to flow_data data frame
  add_flood <- function(flow_data, peaks, gages){
    #Find matching Q2 value
    flood <- peaks$flood_val[peaks$site_no %in% flow_data$site_no[1]]

    #If there is no site match, set Q2 to zero
    if (length(flood) == 0) {flood <- NA}
    flow_data$flood <- flood
    flow_data$flood_ratio <- flow_data$Discharge / flow_data$flood

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

  flow_data <- lapply(flow_data, add_flood, peaks = peaks, gages = gages)

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

#     #add NWS flood discharge value
#     action <- NWS_flood_discharge$Action_Q[NWS_flood_discharge$USGS %in% x$site_no[1]]
#     if (length(action) == 0) {action <- NA}

    return(data.frame(site_no = x$site_no[1], avg_peak = avg_peak, flood_dur = flood_dur,
                      peak = peak, lat = x$lat[1], long = x$long[1], county_cd = fips_cd,
                      maps_id = maps_id, county = county, state = state))
  })

  #sort flood stats by magnitude
  flood_stats <- flood_stats[order(flood_stats$peak),]

  #Create categories of flood peaks for display
  flood_cat <- function(flood_stats) {
    if (is.na(flood_stats$peak)) {
      flood_stats$flood <- NA
    }else if(flood_stats$peak < 1) {
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

  #Remove NAs
  flood_stats <- flood_stats[!is.na(flood_stats$peak), ]

  return(flood_stats)
}

#Function aggregates gage-level output into county-level output
county_aggregates <- function(flood_stats){

  aggregate_fun <- function(gage_flood){
    #county and state name
    county <- gage_flood$county[1]
    state <- gage_flood$state[1]

    #number of gages
    n_gage <- length(gage_flood$site_no)

    #max flood peak
    max_peak <- max(gage_flood$peak)

    #avg flood peak
    avg_peak <- mean(gage_flood$peak)

    #number of gages at different flood stages
    stage <- c("Minor", "Moderate", "Major", "Extreme")

    num <- sapply(stage, function(x) {sum(gage_flood$flood == x)})

    #Convert numbers into percentages at or above that flood class
    minor <- round(sum(num) / n_gage * 100, 1)
    moderate <- round(sum(num[2:4]) / n_gage * 100, 1)
    major <- round(sum(num[3:4]) / n_gage * 100, 1)
    extreme <- round(sum(num[4]) / n_gage * 100, 1)

    #max flood duration
    max_dur <- max(gage_flood$flood_dur)

    #avg flood duration
    avg_dur <- mean(gage_flood$flood_dur)

    return(data.frame(county = county, state = state, num_gages = n_gage,
                      max_peak = max_peak, avg_peak = avg_peak,
                      minor = minor, moderate = moderate, major = major,
                      extreme = extreme, max_dur = max_dur, avg_dur = avg_dur))
  }

  county_stats <- plyr::ddply(flood_stats, "county_cd", aggregate_fun)

  return(county_stats)
}

