#Colorado Front Range Floods - September 2013
start_date <- as.Date("2013-09-09", format = "%Y-%m-%d")
end_date <- as.Date("2013-09-30", format = "%Y-%m-%d")
county_cd <- c("08013", "08031", "08069", "08001", "08059", "08123")

test <- run_flood(county_cd = county_cd, start_date = start_date,
                  end_date = end_date, threshold = "Q2", output = "both")
map_flood(test)

#South Carolina Floods - October 2015
start_date <- as.Date("2015-10-01", format = "%Y-%m-%d")
end_date <- as.Date("2015-10-15", format = "%Y-%m-%d")
county_cd <- c("45063", "45079", "45055", "45061", "45085",
               "45017", "45075", "45027", "45089", "45015", "45035",
               "45019", "45043", "45071", "45081", "45039")

test <- run_flood(county_cd, start_date, end_date, threshold = "Q2")
map_flood(test)

#Entire state of South Carolina
state <- "south carolina"

start_date <- as.Date("2015-10-01", format = "%Y-%m-%d")
end_date <- as.Date("2015-10-15", format = "%Y-%m-%d")

test <- run_flood(state = state, start_date = start_date, end_date = end_date, threshold = "Q2", output = "county")

map_flood(test)

