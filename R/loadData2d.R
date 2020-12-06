#' Load covariate data, including 2D, time and 1 variable
#' Includes flag for data source
#' 
#' @param file path to file
#' @param source ECMWF
#' 
#' @return Covariate data.
load.data.2d <- function(file, source = 'CMEMS') {
  if (source == 'ECMWF') {
    lon.id <- 'longitude'
    lat.id <- 'latitude'} 
  else {
    lon.id <- 'lon'
    lat.id <- 'lat'}
  covariate <- nc_open(file)
  lon <- ncvar_get(covariate, varid = lon.id)
  lat <- ncvar_get(covariate, varid = lat.id)
  time <- ncvar_get(covariate, varid = 'time')
  var <- ncvar_get(covariate)
  nc_close(covariate)
  rm(covariate)
  if (source == 'ECMWF') {
    lat <- rev(lat)
    time <- time*3600}
  return(list(lon = lon, 
              lat = lat, 
              time = time, 
              var = var))}
