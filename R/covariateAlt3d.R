#' Calculate covariates at train.data locations and train.times, 
#' using quadrilinear (3D + T) interpolation 
#' in an alternative order (depth, time, lon, lat)
#' 
#' @param COV CHLF, CHLR, OXYF, OXYR
#' 
#' @return Covariates at train.data locations and train.times.
covariate.alt.3d <- function(train.length, train.data, COV, train.times) {
  covariate <- integer(train.length)
  for (i in 1:train.length){
    if (sum(is.na(train.data[i,])) != 0){
      covariate[i] <- NA} 
    else if (train.data$depth[i] == 0){
      covariate[i] <- NA} 
    else {
      lon.nearby <- find.index(train.data$lon[i], COV$lon)
      lat.nearby <- find.index(train.data$lat[i], COV$lat)
      covariate1 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[1], ], y=COV$time, z=COV$var[lon.nearby[1], lat.nearby[1], , ]), 
                                   cbind(x=train.data$depth[i], y=train.times[i]))
      covariate2 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[1], ], y=COV$time, z=COV$var[lon.nearby[2], lat.nearby[1], , ]), 
                                   cbind(x=train.data$depth[i], y=train.times[i]))
      covariate3 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[2], ], y=COV$time, z=COV$var[lon.nearby[1], lat.nearby[2], , ]), 
                                   cbind(x=train.data$depth[i], y=train.times[i]))
      covariate4 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[2], ], y=COV$time, z=COV$var[lon.nearby[2], lat.nearby[2], , ]), 
                                   cbind(x=train.data$depth[i], y=train.times[i]))
      if (is.na(covariate1) & is.na(covariate2)){
        covariate.nearest <- NA} 
      else if (is.na(covariate1)){
        covariate.nearest <- covariate2} 
      else if (is.na(covariate2)){
        covariate.nearest <- covariate1} 
      else {
        covariate.nearest <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                    c(covariate1, covariate2),
                                    xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
      if (is.na(covariate3) & is.na(covariate4)){
        covariate.neighbour <- NA} 
      else if (is.na(covariate3)){
        covariate.neighbour <- covariate4} 
      else if (is.na(covariate4)){
        covariate.neighbour <- covariate3} 
      else {
        covariate.neighbour <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                      c(covariate3, covariate4),
                                      xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA} 
      else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour} 
      else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest} 
      else {
        covariate[i] <- approx(c(COV$lat[lat.nearby[1]], COV$lat[lat.nearby[2]]),
                               c(covariate.nearest, covariate.neighbour),
                               xout=train.data$lat[i], method="linear", rule=1, ties=mean)$y}}}
  return(covariate)}
