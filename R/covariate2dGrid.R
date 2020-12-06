#' Calculate covariates at grid locations and pred.time, 
#' using trilinear (2D + T) interpolation
#' 
#' @param COV MLD, RAD, WIN
#' 
#' @return Covariates at grid locations and pred.time.
covariate.2d.grid <- function(pred.grid, COV, pred.time) {
  time.nearby <- find.index(pred.time, COV$time)
  covariate.nearest <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, time.nearby[1]]), pred.grid)$z
  covariate.neighbour <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, time.nearby[2]]), pred.grid)$z
  approx.grid(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), pred.time)}
