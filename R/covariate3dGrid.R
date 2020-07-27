#' Calculate covariates at grid locations, using trilinear (3D) interpolation
#'
#' @param cov tem, cur
#'
#' @return Covariates at grid locations.
covariate.3d.grid <- function(grid.list, gldr.depth.mean, cov.lon, cov.lat, cov.depth, cov.predicting, k, cov.var) {
  depth.nearby <- find.depth.grid(gldr.depth.mean, cov.depth)
  covariate.nearest <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], cov.predicting[k]]), grid.list)$z
  covariate.neighbour <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], cov.predicting[k]]), grid.list)$z
  approx.grid(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), list(covariate.nearest, covariate.neighbour), gldr.depth.mean)
}
