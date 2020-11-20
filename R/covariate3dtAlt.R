#' Calculate covariates at gldr.mean locations, using quadrilinear (3D + T) interpolation
#' in an alternative order (depth, time, lon, lat)
#' 
#' @param cov ass.chl
#' 
#' @return Covariates at gldr.mean locations.
covariate.3dt.alt <- function(train.length, training, gldr.mean, cov.lon, cov.lat, cov.depth, cov.time, cov.time.training, cov.var) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA} 
    else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA} 
    else {
      lon.nearby <- find.index(gldr.mean$lon, i, cov.lon)
      lat.nearby <- find.index(gldr.mean$lat, i, cov.lat)
      covariate1 <- interp.surface(list(x=cov.depth[lon.nearby[1], lat.nearby[1], ], y=cov.time, z=cov.var[lon.nearby[1], lat.nearby[1], , ]), 
                                   cbind(x=gldr.mean$depth[i], y=cov.time.training[i]))
      covariate2 <- interp.surface(list(x=cov.depth[lon.nearby[2], lat.nearby[1], ], y=cov.time, z=cov.var[lon.nearby[2], lat.nearby[1], , ]), 
                                   cbind(x=gldr.mean$depth[i], y=cov.time.training[i]))
      covariate3 <- interp.surface(list(x=cov.depth[lon.nearby[1], lat.nearby[2], ], y=cov.time, z=cov.var[lon.nearby[1], lat.nearby[2], , ]), 
                                   cbind(x=gldr.mean$depth[i], y=cov.time.training[i]))
      covariate4 <- interp.surface(list(x=cov.depth[lon.nearby[2], lat.nearby[2], ], y=cov.time, z=cov.var[lon.nearby[2], lat.nearby[2], , ]), 
                                   cbind(x=gldr.mean$depth[i], y=cov.time.training[i]))
      if (is.na(covariate1) & is.na(covariate2)){
        covariate.nearest <- NA} 
      else if (is.na(covariate1)){
        covariate.nearest <- covariate2} 
      else if (is.na(covariate2)){
        covariate.nearest <- covariate1} 
      else {
        covariate.nearest <- approx(c(cov.lon[lon.nearby[1]], cov.lon[lon.nearby[2]]),
                                    c(covariate1, covariate2),
                                    xout=gldr.mean$lon[i], method="linear", rule=1, ties=mean)$y}
      if (is.na(covariate3) & is.na(covariate4)){
        covariate.neighbour <- NA} 
      else if (is.na(covariate3)){
        covariate.neighbour <- covariate4} 
      else if (is.na(covariate4)){
        covariate.neighbour <- covariate3} 
      else {
        covariate.neighbour <- approx(c(cov.lon[lon.nearby[1]], cov.lon[lon.nearby[2]]),
                                      c(covariate3, covariate4),
                                      xout=gldr.mean$lon[i], method="linear", rule=1, ties=mean)$y}
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA} 
      else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour} 
      else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest} 
      else {
        covariate[i] <- approx(c(cov.lat[lat.nearby[1]], cov.lat[lat.nearby[2]]),
                               c(covariate.nearest, covariate.neighbour),
                               xout=gldr.mean$lat[i], method="linear", rule=1, ties=mean)$y}}}
  return(covariate)}
