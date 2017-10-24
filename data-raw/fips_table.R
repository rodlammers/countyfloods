#Get the county.fips table from the maps package and modify it so that state and
#county names each have their own column

#Old approach
library(maps)
fips_table <- plyr::adply(county.fips, 1, function(x) {
  state <- strsplit(as.character(x$polyname), split = ",")[[1]][1]
  county <- strsplit(as.character(x$polyname), split = ",")[[1]][2]
  return(data.frame(state, county))
})

fips_table$county <- plyr::revalue(fips_table$county, c("currituck:knotts" = "currituck",
                                                          "currituck:main" = "currituck",
                                                          "currituck:spit" = "currituck"))

#Drop polyname
fips_table <- fips_table[,-2]

#Get additional fips data to add Puerto Rico, Alaska, and Hawaii
fips2 <- read.csv("data-raw/national_fips.txt", header = FALSE, sep = ",", colClasses = "character")
fips2$fips <- paste0(fips2$V2, fips2$V3)
fips2 <- fips2[fips2$V1 %in% c("PR", "AK", "HI"), ]
fips2$state <- dplyr::if_else(fips2$V1 == "PR", "puerto rico",
                              dplyr::if_else(fips2$V1 == "AK", "alaska", "hawaii"))
fips2$county <- gsub("\\s*\\w*$", "", fips2$V4)
fips2$county <- tolower(fips2$county)
fips2 <- fips2[,6:8]

fips_table <- rbind(fips_table, fips2)
fips_table$fips <- sprintf("%05d", as.numeric(fips_table$fips))

# state_abb <- c(state.abb, "AS", "GU", "MP", "PR", "UM", "VI", "DC")
# state_nm <- c(state.name, "American Somoa", "Guam", "Mariana Islands", "Puerto Rico", "Midway", "Virgin Islands",
#               "Distric of Columbia")
# states <- data.frame(state = state_nm, V1 = state_abb)
# fips_table <- dplyr::left_join(fips_table, states, by = "V1") %>%
#   select(fips, state, county = "V4")
#
# fips_table$county <- gsub("\\s*\\w*$", "", fips_table$county)
# fips_table$county <- tolower(fips_table$county)
# fips_table$state <- tolower(fips_table$state)
