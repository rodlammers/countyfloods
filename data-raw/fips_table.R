#Get the county.fips table from the maps package and modify it so that state and
#county names each have their own column

library(maps)
fips_table <- plyr::adply(county.fips, 1, function(x) {
  state <- strsplit(as.character(x$polyname), split = ",")[[1]][1]
  county <- strsplit(as.character(x$polyname), split = ",")[[1]][2]
  return(data.frame(state, county))
})

fips_table$county <- plyr::revalue(fips_table$county, c("currituck:knotts" = "currituck",
                                                          "currituck:main" = "currituck",
                                                          "currituck:spit" = "currituck"))
