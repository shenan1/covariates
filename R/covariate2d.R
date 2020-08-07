#' Calculate covariates at gldr.mean locations, using bilinear (2D) interpolation
#' 
#' @param cov rad
#' 
#' @return Covariates at gldr.mean locations.
covariate.2d <- function(train.length, training, gldr.mean, cov.lon, cov.lat, cov.training, cov.var) {
  covariate <- integer(train.length)
  for (i in training){
    if (sum(is.na(gldr.mean[i,])) != 0){
      covariate[i] <- NA
    } else {
      covariate[i] <- interp.surface(list(x=cov.lon, y=cov.lat, 
                                          z=cov.var[,, cov.training[i]]), 
                                     cbind(x=gldr.mean$lon[i], y=gldr.mean$lat[i]))
    }
  }
  return(covariate)
}
