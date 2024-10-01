library(nrwAPI)



sites <- nrw_stations(key)

easting <- 262645
northing <- 221978


coords <- c(easting, northing)

sites <- sites %>% mutate(dist = sqrt((coords[1] - as.numeric(easting))^2 + (coords[2] - as.numeric(northing))^2)) %>% arrange(dist)

