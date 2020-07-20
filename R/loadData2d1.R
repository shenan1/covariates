#' Load covariate data, including 2D, time and 1 variable
#' 
#' @param data.file path to data file
#' 
#' @return Covariate data.
load.data.2d1 <- function(data.file) {
  covariate <- nc_open(data.file)
  cov.lon <- ncvar_get(covariate, varid = 'lon')
  cov.lat <- ncvar_get(covariate, varid = 'lat')
  cov.time <- ncvar_get(covariate, varid = 'time')
  cov.var <- ncvar_get(covariate)[,,]
  nc_close(covariate)
  rm(covariate)
  return(list(cov.lon, cov.lat, cov.time, cov.var))
}
