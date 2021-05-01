#' Calculate covariates at grid locations and pred.time, 
#' using quadrilinear (3D + T) interpolation
#' for a sigma coordinate system, where depth = f(lon, lat, depth, time)
#' 
#' @param COV MASS-4D CHL
#' 
#' @return Covariates at grid locations and pred.time.
covariate.4d.grid <- function(pred.grid, pred.depth, COV, pred.time) {
  pred.data <- data.frame(matrix(0, 1, 3))
  colnames(pred.data) <- c("lon", "lat", "depth")
  pred.data$depth <- pred.depth
  covariate <- matrix(0, length(pred.grid$x), length(pred.grid$y))
  for (i in 1:length(pred.grid$x)) {
    for (j in 1:length(pred.grid$y)) {
      pred.data$lon <- pred.grid$x[i]
      pred.data$lat <- pred.grid$y[j]
      covariate[i, j] <- covariate.4d(1, pred.data, COV, pred.time)
    }}
  return(covariate)
  }
