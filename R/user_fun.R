run_flood <- function(county_cd = NULL, state = NULL, start_date, end_date, threshold, flood_type = NULL){

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

  #get flood stats
  flood_stats <- flood_analysis(flow_data, peaks, gages)

  return(flood_stats)
}

map_flood <- function(flood_stats){
  map_function(flood_stats)
}



