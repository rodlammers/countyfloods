run_flood <- function(county_cd, start_date, end_date){

  #get gages
  gages <- get_gages(county_cd, start_date, end_date)

  #get flow data
  flow_data <- get_flow_data(gages$site_no, start_date, end_date)

  #get flow peaks
  peaks <- find_Q2(gages$site_no)

  #get flood stats
  flood_stats <- flood_analysis(flow_data, peaks, gages)

  return(flood_stats)
}

map_flood <- function(flood_stats){
  map_data(flood_stats)
}



