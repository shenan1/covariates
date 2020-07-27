#' Calculate covariates at gldr.mean locations, using quadrilinear (3D + T) interpolation
#' 
#' @param cov chl, oxy, nit, pho, tur
#' 
#' @return Covariates at gldr.mean locations.
covariate.3dt <- function(train.length, training, gldr.mean, cov.lon, cov.lat, cov.depth, cov.time, cov.time.training, cov.var) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA
    } else {
      depth.nearby <- find.depth(gldr.mean, i, cov.depth)
      time.nearby <- find.time(cov.time.training, i, cov.time)
      covariate1 <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], time.nearby[1]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate2 <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], time.nearby[1]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate3 <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], time.nearby[2]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate4 <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], time.nearby[2]]), 
                                   cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      
      if (is.na(covariate1) & is.na(covariate2)){
        covariate.nearest <- NA
      } else if (is.na(covariate1)){
        covariate.nearest <- covariate2
      } else if (is.na(covariate2)){
        covariate.nearest <- covariate1
      } else {
        covariate.nearest <- approx(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), 
                                    c(covariate1, covariate2), 
                                    xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
      }
      
      if (is.na(covariate3) & is.na(covariate4)){
        covariate.neighbour <- NA
      } else if (is.na(covariate3)){
        covariate.neighbour <- covariate4
      } else if (is.na(covariate4)){
        covariate.neighbour <- covariate3
      } else {
        covariate.neighbour <- approx(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), 
                                      c(covariate3, covariate4), 
                                      xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
      }
      
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA
      } else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour
      } else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest
      } else {
        covariate[i] <- approx(c(cov.time[time.nearby[1]], cov.time[time.nearby[2]]), 
                               c(covariate.nearest, covariate.neighbour), 
                               xout=cov.time.training[i], method="linear", rule=1, ties=mean)$y
      }
    }
  }
  return(covariate)
}
