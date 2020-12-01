#' Calculate covariates at grid locations, using trilinear (2D + T) interpolation
#' 
#' @param cov mld, rad, win
#' 
#' @return Covariates at grid locations.
covariate.2dt.grid <- function(grid.list, cov.lon, cov.lat, cov.time, cov.time.predicting, k, cov.var) {
  time.nearby <- find.index(cov.time.predicting, k, cov.time)
  covariate.nearest <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, time.nearby[1]]), grid.list)$z
  covariate.neighbour <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, time.nearby[2]]), grid.list)$z
  approx.grid(c(cov.time[time.nearby[1]], cov.time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), cov.time.predicting[k])}
