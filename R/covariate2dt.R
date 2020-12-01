#' Calculate covariates at gldr.mean locations, using trilinear (2D + T) interpolation
#' 
#' @param cov mld, rad, win
#' 
#' @return Covariates at gldr.mean locations.
covariate.2dt <- function(train.length, training, gldr.mean, cov.lon, cov.lat, cov.time, cov.time.training, cov.var) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA} 
    else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA} 
    else {
      time.nearby <- find.index(cov.time.training, i, cov.time)
      covariate.nearest <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, time.nearby[1]]), 
                                          cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate.neighbour <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, time.nearby[2]]), 
                                            cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA} 
      else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour} 
      else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest} 
      else {
        covariate[i] <- approx(c(cov.time[time.nearby[1]], cov.time[time.nearby[2]]), 
                               c(covariate.nearest, covariate.neighbour), 
                               xout=cov.time.training[i], method="linear", rule=1, ties=mean)$y}}}
  return(covariate)}
