#' Calculate covariates at gldr.mean locations, using trilinear (3D) interpolation
#' 
#' @param cov tem, cur
#' 
#' @return Covariates at gldr.mean locations.
covariate.3d <- function(train.length, training, gldr.mean, cov.lon, cov.lat, cov.depth, cov.training, cov.var) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else if (gldr.mean$depth[i] == 0){
      covariate[i] <- NA
    } else {
      depth.nearby <- find.depth(gldr.mean, i, cov.depth)
      covariate.nearest <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[1], cov.training[i]]), 
                                          cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      covariate.neighbour <- interp.surface(list(x=cov.lon, y=cov.lat, z=cov.var[,, depth.nearby[2], cov.training[i]]), 
                                            cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
      
      if (is.na(covariate.nearest) & is.na(covariate.neighbour)){
        covariate[i] <- NA
      } else if (is.na(covariate.nearest)){
        covariate[i] <- covariate.neighbour
      } else if (is.na(covariate.neighbour)){
        covariate[i] <- covariate.nearest
      } else {
        covariate[i] <- approx(c(cov.depth[depth.nearby[1]], cov.depth[depth.nearby[2]]), 
                               c(covariate.nearest, covariate.neighbour), 
                               xout=gldr.mean$depth[i], method="linear", rule=1, ties=mean)$y
      }
    }
  }
  return(covariate)
}
