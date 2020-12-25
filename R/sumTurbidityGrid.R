#' Sum turbidity to pred.depth at pred.grid locations and pred.time
#' 
#' 
#' @return Turbidity sum at pred.grid locations and pred.time.
sum.turbidity.grid <- function(pred.grid, pred.depth, TUR, pred.time) {
  length.lon <- length(pred.grid$x)
  length.lat <- length(pred.grid$y)
  depth.nearby <- find.index(pred.depth, TUR$depth)
  depth.min <- min(depth.nearby)
  tur.grid.all <- array(0, dim=c(length.lon, length.lat, depth.min))
  tur.grid.sum <- matrix(0, length.lon, length.lat)
  tur.grid <- covariates:::covariate.3d.grid(pred.grid, pred.depth, TUR, pred.time)
  
  # calculate turbidity at TUR depths (to depth.min) at pred.grid locations and pred.time
  for (i in 1:depth.min){
    tur.grid.all[,, i] <- covariate.3d.grid(pred.grid, TUR$depth[i], TUR, pred.time)}
  
  # sum turbidity to pred.depth at pred.grid locations and pred.time
  for (i in 2:depth.min){
    tur.grid.sum <- tur.grid.sum + apply(abind(tur.grid.all[,, i-1], tur.grid.all[,, i], along=3), 1:2, mean)*(TUR$depth[i] - TUR$depth[i-1])}
  tur.grid.sum <- tur.grid.sum + apply(abind(tur.grid.all[,, depth.min], tur.grid, along=3), 1:2, mean)*(pred.depth - TUR$depth[depth.min])
  
  return(tur.sum)}
