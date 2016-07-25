start_date <- as.Date("2013-09-09", format = "%Y-%m-%d")
end_date <- as.Date("2013-09-30", format = "%Y-%m-%d")
county_cd <- c("08013", "08031", "08069", "08001", "08059", "08123")

test <- run_flood(county_cd, start_date, end_date)
map_flood(test)
