#' Calculate covariates at grid locations, using quadrilinear (3D + T) interpolation
#' 
#' @param cov chl, oxy, nit, pho, tur
#' 
#' @return Covariates at grid locations.
covariate.3dt.grid <- function(grid.list, gldr.depth.mean, cov.lon, cov.lat, cov.depth, cov.time, cov.time.predicting, k, cov.var) {
  depth.nearby <- find.depth.grid(gldr.depth.mean, cov.depth)
  time.nearby <- find.time(cov.time.predicting, k, cov.time)
  covariate1 <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], time.nearby[1]]), grid.list)$z
  covariate2 <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], time.nearby[1]]), grid.list)$z
  covariate3 <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], time.nearby[2]]), grid.list)$z
  covariate4 <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], time.nearby[2]]), grid.list)$z
  covariate.nearest <- approx.grid(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), list(covariate1, covariate2), gldr.depth.mean)
  covariate.neighbour <- approx.grid(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), list(covariate3, covariate4), gldr.depth.mean)
  approx.grid(c(cov.time[time.nearby[1]], cov.time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), cov.time.predicting[k])
}
