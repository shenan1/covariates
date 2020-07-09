#' Calculate covariates at grid locations, using quadrilinear (3D + T) interpolation
#' 
#' @param dat.lon chl.lon, ...
#' @param variable chlor, ...
#' 
#' @return Covariates at grid locations.
covariate.3dt.grid <- function(grid.list, gldr.depth.mean, dat.lon, dat.lat, dat.depth, dat.time, dat.time.predicting, k, variable) {
  depth.nearby <- find.depth.grid(gldr.depth.mean, dat.depth)
  time.nearby <- find.time(dat.time.predicting, k, dat.time)
  covariate1 <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], time.nearby[1]]), grid.list)$z
  covariate2 <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], time.nearby[1]]), grid.list)$z
  covariate3 <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], time.nearby[2]]), grid.list)$z
  covariate4 <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], time.nearby[2]]), grid.list)$z
  covariate.nearest <- approx.grid(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), list(covariate1, covariate2), gldr.depth.mean)
  covariate.neighbour <- approx.grid(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), list(covariate3, covariate4), gldr.depth.mean)
  approx.grid(c(dat.time[time.nearby[1]], dat.time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), dat.time.predicting[k])
}
