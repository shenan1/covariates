#' Load covariate data, including 2D, time and 1 variable.
#' Includes flag for data source.
#' Also returns origin from which time is counted.
#' 
#' @param file path to file
#' @param source ECMWF
#' 
#' @return Covariate data and origin.
load.data.2d <- function(file, source = 'CMEMS') {
  if (source == 'ECMWF') {
    lon.id <- 'longitude'
    lat.id <- 'latitude'} else {
      lon.id <- 'lon'
      lat.id <- 'lat'}
  
  covariate <- nc_open(file)
  lon <- ncvar_get(covariate, varid = lon.id)
  lat <- ncvar_get(covariate, varid = lat.id)
  time <- ncvar_get(covariate, varid = 'time')
  var <- ncvar_get(covariate)
  
  text1 <- grep("since", as.character(covariate), value=TRUE)[1]                # returns first line containing "since"
  text2 <- sub(".*since ", "", text1)                                           # returns everything after "since " in text1
  origin <- substr(text2, 1, 10)                                                # returns characters 1-10 in text2
  
  nc_close(covariate)
  rm(covariate)
  
  if (source == 'ECMWF') {
    lat <- rev(lat)
    time <- time*3600}
  
  return(list(lon    = lon, 
              lat    = lat, 
              time   = time, 
              var    = var, 
              origin = origin))}
