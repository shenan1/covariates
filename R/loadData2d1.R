#' Load covariate data, including 2D, time and 1 variable
#' Includes flag for data source
#' 
#' @param data.file path to data file
#' @param data.source CMEMS or ECMWF
#' 
#' @return Covariate data.
load.data.2d1 <- function(data.file, data.source) {
  if (data.source == 'ECMWF') {
    lon.id <- 'longitude'
    lat.id <- 'latitude'} 
  else {
    lon.id <- 'lon'
    lat.id <- 'lat'}
  covariate <- nc_open(data.file)
  cov.lon <- ncvar_get(covariate, varid = lon.id)
  cov.lat <- ncvar_get(covariate, varid = lat.id)
  cov.time <- ncvar_get(covariate, varid = 'time')
  cov.var <- ncvar_get(covariate)[,,]
  nc_close(covariate)
  rm(covariate)
  if (data.source == 'ECMWF') {
    cov.lat <- rev(cov.lat)
    cov.time <- cov.time*3600}
  return(list(cov.lon, cov.lat, cov.time, cov.var))}
