#' Calculate covariates at train.data locations and train.times, 
#' using quadrilinear (3D + T) interpolation
#' 
#' @param COV CHL, CUX, CUY, OXY, NIT, PHO, TEM, TUR
#' 
#' @return Covariates at train.data locations and train.times.
covariate.3d <- function(train.length, train.data, COV, train.times) {
  covariate <- integer(train.length)
  for (i in 1:train.length){
    if (sum(is.na(train.data[i,])) != 0){
      covariate[i] <- NA} else if (train.data$depth[i] == 0){
        covariate[i] <- NA} else {
          depth.nearby <- find.index(train.data$depth[i], COV$depth)
          time.nearby <- find.index(train.times[i], COV$time)
          covariate1 <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[1], time.nearby[1]]), 
                                       cbind(x=train.data$lon[i], y=train.data$lat[i]))
          covariate2 <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[2], time.nearby[1]]), 
                                       cbind(x=train.data$lon[i], y=train.data$lat[i]))
          covariate3 <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[1], time.nearby[2]]), 
                                       cbind(x=train.data$lon[i], y=train.data$lat[i]))
          covariate4 <- interp.surface(list(x=COV$lon, y=COV$lat, z=COV$var[,, depth.nearby[2], time.nearby[2]]), 
                                       cbind(x=train.data$lon[i], y=train.data$lat[i]))
          if (is.na(covariate1) & is.na(covariate2)){
            covariate.nearest <- NA} else if (is.na(covariate1)){
              covariate.nearest <- covariate2} else if (is.na(covariate2)){
                covariate.nearest <- covariate1} else {
                  covariate.nearest <- approx(c(COV$depth[depth.nearby[1]], COV$depth[depth.nearby[2]]), 
                                              c(covariate1, covariate2), 
                                              xout=train.data$depth[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate3) & is.na(covariate4)){
            covariate.neighbour <- NA} else if (is.na(covariate3)){
              covariate.neighbour <- covariate4} else if (is.na(covariate4)){
                covariate.neighbour <- covariate3} else {
                  covariate.neighbour <- approx(c(COV$depth[depth.nearby[1]], COV$depth[depth.nearby[2]]), 
                                                c(covariate3, covariate4), 
                                                xout=train.data$depth[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
            covariate[i] <- NA} else if (is.na(covariate.nearest)){
              covariate[i] <- covariate.neighbour} else if (is.na(covariate.neighbour)){
                covariate[i] <- covariate.nearest} else {
                  covariate[i] <- approx(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]), 
                                         c(covariate.nearest, covariate.neighbour), 
                                         xout=train.times[i], method="linear", rule=1, ties=mean)$y}}}
  return(covariate)}
