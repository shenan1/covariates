#' Calculate covariates at grid locations, using trilinear (3D) interpolation
#'
#' @param dat.lon tem.lon, cur.lon, ...
#' @param variable thetao, uo, vo, ...
#'
#' @return Covariates at grid locations.
covariate.3d.grid <- function(grid.list, gldr.depth.mean, dat.lon, dat.lat, dat.depth, dat.predicting, k, variable) {
  depth.nearby <- find.depth.grid(gldr.depth.mean, dat.depth)
  covariate.nearest <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], dat.predicting[k]]), grid.list)$z
  covariate.neighbour <- interp.surface.grid(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], dat.predicting[k]]), grid.list)$z
  approx.grid(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), list(covariate.nearest, covariate.neighbour), gldr.depth.mean)
}
