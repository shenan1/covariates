#' Calculate covariates at gldr.mean locations, using quadrilinear (3D + T) interpolation
#' 
#' @param dat.lon chl.lon, ...
#' @param variable chlor, ...
#' 
#' @return Covariates at gldr.mean locations.
covariate.3dt <- function(train.length, training, gldr.mean, dat.lon, dat.lat, dat.depth, dat.time, dat.time.training, variable) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA
    } else {
      depth.nearby <- find.depth(gldr.mean, i, dat.depth)
      time.nearby <- find.time(dat.time.training, i, dat.time)
      covariate1 <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], time.nearby[1]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate2 <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], time.nearby[1]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate3 <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], time.nearby[2]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate4 <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], time.nearby[2]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate.nearest <- approx(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), 
                                  c(covariate1, covariate2), 
                                  xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
      covariate.neighbour <- approx(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), 
                                    c(covariate3, covariate4), 
                                    xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
      covariate[i] <- approx(c(dat.time[time.nearby[1]], dat.time[time.nearby[2]]), 
                             c(covariate.nearest, covariate.neighbour), 
                             xout=dat.time.training[i], method="linear", rule=1, ties=mean)$y
    }
  }
  return(covariate)
}
