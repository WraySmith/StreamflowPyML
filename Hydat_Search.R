# This function recreates the location search function available in weathercan 
# but for hydrometric stations
hydat_loc_search <- function(coords = NULL, dist = 10, stn = tidyhydat::allstations) {
        coords <- as.numeric(as.character(coords[c(2, 1)]))
        locs <- as.matrix(stn[!is.na(stn$LATITUDE), c("LONGITUDE", 
                                                         "LATITUDE")])
        stn$distance <- NA
        stn$distance[!is.na(stn$LATITUDE)] <- sp::spDistsN1(pts = locs, 
                                                               pt = coords, longlat = TRUE)
        stn <- dplyr::arrange(stn, distance)
        i <- which(stn$distance <= dist)
        if (length(i) == 0) {
                i <- 1:10
                if (!quiet) 
                        message("No stations within ", dist, "km. Returning closest 10 stations.")
        }
        
        stn <- stn[i, ]
        stn <- dplyr::arrange(stn, distance, STATION_NAME, STATION_NUMBER)
        stn
}
