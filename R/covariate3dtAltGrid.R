#' Calculate covariates at grid locations, using quadrilinear (3D + T) interpolation
#' in an alternative order (depth, time, lon, lat)
#' 
#' @param cov ass.chl
#' 
#' @return Covariates at grid locations.
covariate.3dt.alt.grid <- function(grid.list, gldr.depth.mean, cov.lon, cov.lat, cov.depth, cov.time, cov.time.predicting, k, cov.var) {
  time.nearby <- find.index(cov.time.predicting, k, cov.time)
  z.nearest <- z.neighbour <- matrix(0, length(cov.lon), length(cov.lat))
  for (i in 1:length(cov.lon)){
    for (j in 1:length(cov.lat)){
      depth.nearest <- which.min(abs(gldr.depth.mean - cov.depth[i, j, ]))
      z.nearest[i, j] <- cov.var[i, j, depth.nearest, time.nearby[1]]
      z.neighbour[i, j] <- cov.var[i, j, depth.nearest, time.nearby[2]]}}
  covariate.nearest <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=z.nearest), grid.list)$z
  covariate.neighbour <- interp.surface.grid(list(x=cov.lon, y=cov.lat, z=z.neighbour), grid.list)$z
  approx.grid(c(cov.time[time.nearby[1]], cov.time[time.nearby[2]]), list(covariate.nearest, covariate.neighbour), cov.time.predicting[k])}
