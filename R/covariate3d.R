#' Calculate covariates at gldr.mean locations, using trilinear (3D) interpolation
#' 
#' @param dat.lon tem.lon, cur.lon, ...
#' @param variable thetao, uo, vo, ...
#' 
#' @return Covariates at gldr.mean locations.
covariate.3d <- function(train.length, training, gldr.mean, dat.lon, dat.lat, dat.depth, dat.training, variable) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA
    } else {
      depth.nearby <- find.depth(gldr.mean, i, dat.depth)
      covariate.nearest <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[1], dat.training[i]]), 
                                          cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate.neighbour <- interp.surface(list(x=dat.lon, y=dat.lat, z=variable[,, depth.nearby[2], dat.training[i]]), 
                                            cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate[i] <- approx(c(dat.depth[depth.nearby[1]], dat.depth[depth.nearby[2]]), 
                             c(covariate.nearest, covariate.neighbour), 
                             xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
    }
  }
  return(covariate)
}
