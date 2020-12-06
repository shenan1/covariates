#' Calculate covariates at grid locations and pred.time, 
#' using quadrilinear (3D + T) interpolation
#' in an alternative order (depth, time, lon, lat)
#' 
#' @param COV CHLF, CHLR, OXYF, OXYR
#' 
#' @return Covariates at grid locations and pred.time.
covariate.alt.3d.grid <- function(pred.grid, pred.depth, COV, pred.time) {
  time.nearby <- find.index(pred.time, COV$time)
  z.nearest <- z.neighbour <- matrix(0, length(COV$lon), length(COV$lat))
  for (i in 1:length(COV$lon)){
    for (j in 1:length(COV$lat)){
      depth.nearest <- which.min(abs(pred.depth - COV$depth[i, j, ]))
      z.nearest[i, j] <- COV$var[i, j, depth.nearest, time.nearby[1]]
      z.neighbour[i, j] <- COV$var[i, j, depth.nearest, time.nearby[2]]}}
  covariate.nearest <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=z.nearest), pred.grid)$z
  covariate.neighbour <- interp.surface.grid(list(x=COV$lon, y=COV$lat, z=z.neighbour), pred.grid)$z
  approx.grid(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), pred.time)}
