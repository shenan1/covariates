#' Calculate covariates at grid locations, using trilinear (2D + T) interpolation
#' 
#' @param dat.lon mld.lon, ...
#' @param variable mlotst, ...
#' 
#' @return Covariates at grid locations.
covariate.2dt.grid <- function(grid.list, dat.lon, dat.lat, dat.time, dat.time.predicting, k, variable) {
  time.nearby <- find.time(dat.time.predicting, k, dat.time)
  covariate.nearest <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, time.nearby[1]]), grid.list)$z
  covariate.neighbour <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, time.nearby[2]]), grid.list)$z
  approx.grid(c(dat.time[time.nearby[1]], dat.time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), dat.time.predicting[k])
}
