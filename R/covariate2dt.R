#' Calculate covariates at gldr.mean locations, using trilinear (2D + T) interpolation
#' 
#' @param dat.lon mld.lon, ...
#' @param variable mlotst, ...
#' 
#' @return Covariates at gldr.mean locations.
covariate.2dt <- function(train.length, training, gldr.mean, dat.lon, dat.lat, dat.time, dat.time.training, variable) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA
    } else {
      time.nearby <- find.time(dat.time.training, i, dat.time)
      covariate.nearest <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, time.nearby[1]]), 
                                          cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate.neighbour <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, time.nearby[2]]), 
                                            cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate[i] <- approx(c(dat.time[time.nearby[1]], dat.time[time.nearby[2]]), 
                             c(covariate.nearest, covariate.neighbour), 
                             xout=dat.time.training[i], method="linear", rule=1, ties=mean)$y
    }
  }
  return(covariate)
}
