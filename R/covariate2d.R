#' Calculate covariates at train.data locations and train.times, 
#' using trilinear (2D + T) interpolation
#' 
#' @param COV MLD, RAD, WIN
#' 
#' @return Covariates at train.data locations and train.times.
covariate.2d <- function(train.length, train.data, COV, train.times) {
  covariate <- integer(train.length)
  for (i in 1:train.length){
    if (sum(is.na(train.data[i,])) != 0){
      covariate[i] <- NA} 
    else if (train.data$depth[i] <= 0){
      covariate[i] <- NA} 
    else {
      time.nearby <- find.index(train.times[i], COV$time)
      covariate.nearest <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, time.nearby[1]]), 
                                          cbind(x=train.data$lon[i], y=train.data$lat[i]))
      covariate.neighbour <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, time.nearby[2]]), 
                                            cbind(x=train.data$lon[i], y=train.data$lat[i]))
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA} 
      else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour} 
      else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest} 
      else {
        covariate[i] <- approx(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]), 
                               c(covariate.nearest, covariate.neighbour), 
                               xout=train.times[i], method="linear", rule=1, ties=mean)$y}}}
  return(covariate)}
