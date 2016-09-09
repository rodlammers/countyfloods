#Colorado Front Range Floods - September 2013
start_date <- as.Date("2013-09-09", format = "%Y-%m-%d")
end_date <- as.Date("2013-09-30", format = "%Y-%m-%d")
county_cd <- c("08013", "08031", "08069", "08001", "08059", "08123")

test <- run_flood(county_cd = county_cd, start_date = start_date,
                  end_date = end_date, threshold = "Q2", output = "gage")
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
                  threshold = "Q2", output = "both")
gage_output <- test[[1]]
county_output <- test[[2]]

map_flood(county_output)
map_flood(gage_output)
map_flood(test)


#Virginia Tests
va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
                      end_date = "2015-12-31", threshold = "Q2", output = "gage")
map_flood(va_floods)
va_floods <- run_flood(state = "Virginia", start_date = "2015-01-01",
                      end_date = "2015-12-31", threshold = "NWS", flood_type = "action",
                      output = "county")
map_flood(va_floods)

miami_flow_data <- get_gages("12086", start_date = "2000-01-01",
                                end_date = "2009-12-31") %>%
                   get_flow_data(start_date = "2000-01-01",
                                 end_date = "2000-01-31")

#Input data frame of county_cds and date ranges
county_cd <- rep("12086", 10)
start_date <- c("2000-06-01", "2001-06-01", "2002-06-01", "2003-06-01", "2004-06-01",
                "2005-06-01", "2006-06-01", "2007-06-01", "2008-06-01", "2009-06-01")
end_date <- c("2000-06-30", "2001-06-30", "2002-06-30", "2003-06-30", "2004-06-30",
              "2005-06-30", "2006-06-30", "2007-06-30", "2008-06-30", "2009-06-30")
input_df <- data.frame(county_cd = county_cd, start_date = start_date, end_date = end_date, stringsAsFactors = FALSE)
str(input)

#Time series
county_cd <- "12086"
start_date <- "2000-01-01"
end_date <- "2010-01-01"
test <- time_series_flood(county_cd = county_cd, start_date = start_date, end_date = end_date)

