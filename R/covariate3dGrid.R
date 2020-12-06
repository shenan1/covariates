#' Calculate covariates at grid locations and pred.time, 
#' using quadrilinear (3D + T) interpolation
#' 
#' @param COV CHL, CUX, CUY, OXY, NIT, PHO, TEM, TUR
#' 
#' @return Covariates at grid locations and pred.time.
covariate.3d.grid <- function(pred.grid, pred.depth, COV, pred.time) {
  depth.nearby <- find.index(pred.depth, COV$depth)
  time.nearby <- find.index(pred.time, COV$time)
  covariate1 <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[1], time.nearby[1]]), pred.grid)$z
  covariate2 <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[2], time.nearby[1]]), pred.grid)$z
  covariate3 <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[1], time.nearby[2]]), pred.grid)$z
  covariate4 <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[2], time.nearby[2]]), pred.grid)$z
  covariate.nearest <- approx.grid(c(COV$depth[depth.nearby[1]], COV$depth[depth.nearby[2]]), list(covariate1, covariate2), pred.depth)
  covariate.neighbour <- approx.grid(c(COV$depth[depth.nearby[1]], COV$depth[depth.nearby[2]]), list(covariate3, covariate4), pred.depth)
  approx.grid(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), pred.time)}
