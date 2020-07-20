#' Load covariate data, including 3D, time and 2 variables
#' 
#' @param data.file path to data file
#' 
#' @return Covariate data.
load.data.3d2 <- function(data.file) {
  covariate <- nc_open(data.file)
  cov.lon <- ncvar_get(covariate, varid = 'lon')
  cov.lat <- ncvar_get(covariate, varid = 'lat')
  cov.depth <- ncvar_get(covariate, varid = 'depth')
  cov.time <- ncvar_get(covariate, varid = 'time')
  cov.var1 <- ncvar_get(covariate, varid = 'uo')[,,,]
  cov.var2 <- ncvar_get(covariate, varid = 'vo')[,,,]
  nc_close(covariate)
  rm(covariate)
  return(list(cov.lon, cov.lat, cov.depth, cov.time, cov.var1, cov.var2))
}
