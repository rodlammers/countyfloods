not_cran <- Sys.getenv("NOT_CRAN") == "true"

if(not_cran){
library(countyfloods)

#Colorado Front Range Floods - September 2013
start_date <- as.Date("2013-09-09", format = "%Y-%m-%d")
end_date <- as.Date("2013-09-30", format = "%Y-%m-%d")
county_cd <- c("08013", "08031", "08069", "08001", "08059", "08123")

test <- run_flood(county_cd = county_cd, start_date = start_date,
                  end_date = end_date, threshold = "Q2", output = "both", weight = "Q2")
gage_out<- test[[1]]
county_out <- test[[2]]

map_flood(test)

#South Carolina Floods - October 2015
start_date <- as.Date("2015-10-01", format = "%Y-%m-%d")
end_date <- as.Date("2015-10-15", format = "%Y-%m-%d")
county_cd <- c("45063", "45079", "45055", "45061", "45085",
               "45017", "45075", "45027", "45089", "45015", "45035",
               "45019", "45043", "45071", "45081", "45039")

test <- run_flood(county_cd = county_cd, start_date = start_date,
                  end_date = end_date, threshold = "Q2", output = "county")
map_flood(test)

#Entire state of South Carolina
state <- "south carolina"

start_date <- as.Date("2015-10-01", format = "%Y-%m-%d")
end_date <- as.Date("2015-10-15", format = "%Y-%m-%d")

test <- run_flood(state = state, start_date = start_date, end_date = end_date,
                  threshold = "Q2", output = "both", weight = "DA")
gage_output <- test[[1]]
county_output <- test[[2]]

map_flood(test)
map_flood(gage_output)

#Virginia Tests
va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
                      end_date = "2015-12-31", threshold = "Q2", output = "gage")
map_flood(va_floods)

va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
                      end_date = "2015-12-31", threshold = "NWS", flood_type = "action",
                      output = "both")
map_flood(va_floods)

#Input data frame of county_cds and date ranges
county_cd <- c(rep("08069", 10), rep("08013", 10))
start_date <- rep(c("2000-06-01", "2001-06-01", "2002-06-01", "2003-06-01", "2004-06-01",
                "2005-06-01", "2006-06-01", "2007-06-01", "2008-06-01", "2009-06-01"), 2)
end_date <- rep(c("2000-06-30", "2001-06-30", "2002-06-30", "2003-06-30", "2004-06-30",
              "2005-06-30", "2006-06-30", "2007-06-30", "2008-06-30", "2009-06-30"), 2)
input_df <- data.frame(county_cd = county_cd, start_date = start_date, end_date = end_date, stringsAsFactors = FALSE)

test <- long_term_flood(input_df = input_df)
gage <- test[[1]]
county <- test[[2]]
map_flood(county)

county_cd <- c(rep("51013", 5), rep("51107", 5), rep("51059", 5))
start_date <- rep(c("2010-04-01", "2011-04-01", "2012-04-01", "2013-04-01", "2014-04-01"), 3)
end_date <- rep(c("2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", "2014-04-30"), 3)
input_df <- data.frame(county_cd = county_cd, start_date = start_date, end_date = end_date, stringsAsFactors = FALSE)

#With default values
VA_floods <- long_term_flood(input_df)

#Using NWS values
VA_floods <- long_term_flood(input_df, threshold = "NWS")


#Time series
county_cd <- c("08069", "08013")
start_date <- "2010-01-01"
end_date <- "2014-01-01"
test <- time_series_flood(county_cd = county_cd, start_date = start_date, end_date = end_date,
                          threshold = "Q2", filter_data = TRUE)
gages <- test[[1]]
county <- test[[2]]

time_series_plot(county, category = "Moderate")

#Virginia
va_time_series <- time_series_flood(county_cd = c("51013", "51107", "51059"), start_date = "2010-01-01",
                      end_date = "2015-12-31", threshold = "NWS",
                      flood_type = "flood", filter_data = TRUE, weight = "DA")
gage <- va_time_series[[1]]
county <- va_time_series[[2]]
time_series_plot(va_time_series[[2]])

#Texas flooding
# state <- "Texas"
# tx <- time_series_flood(state = state, start_date = "2017-08-24", end_date = "2017-09-10", filter_data = FALSE)
# time_series_map(tx[[1]], filename = "C:/Users/rlammers/Desktop/Hurricane Photos/TX")
#
# #Puerto Rico
# pr <- time_series_flood(state = "Puerto Rico", start_date = "2017-09-17", end_date = "2017-09-30",
#                         filter_data = FALSE)
# time_series_map(pr[[1]], filename = "C:/Users/rlammers/Desktop/Hurricane Photos/PR")
#
# #Florida
# fl <- time_series_flood(state = "Florida", start_date = "2017-09-08", end_date = "2017-09-25", filter_data = FALSE)
# time_series_map(fl[[2]], filename = "C:/Users/rlammers/Desktop/Hurricane Photos/FL")
#
# #Hawaii
# hi <- time_series_flood(state = "Hawaii", start_date = "2017-09-01", end_date = "2017-09-03", filter_data = FALSE)
# time_series_map(hi[[1]])
#
# #Alaska
# ak <- time_series_flood(state = "Alaska", start_date = "2017-09-01", end_date = "2017-09-01", filter_data = FALSE)
# time_series_map(ak[[1]])
}


