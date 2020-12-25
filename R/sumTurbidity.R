#' Sum turbidity to train.data depths at train.data locations and train.times
#' 
#' 
#' @return Turbidity sum at train.data locations and train.times.
sum.turbidity <- function(train.length, train.data, TUR, train.times) {
  depth.length <- length(TUR$depth) - 1
  train.copy <- train.data
  tur.all <- matrix(0, depth.length, train.length)
  tur.sum <- integer(train.length)
  tur <- covariate.3d(train.length, train.data, TUR, train.times)
  
  # calculate turbidity at TUR depths (except deepest) at train.data locations and train.times
  for (i in 1:depth.length){
    train.copy$depth <- rep(TUR$depth[i], nrow(train.copy))
    tur.all[i,] <- covariate.3d(train.length, train.copy, TUR, train.times)}
  
  # sum turbidity to train.data depths at train.data locations and train.times
  for (j in training){
    if (is.na(train.data$depth[j])){
      tur.sum[j] <- NA} else if (train.data$depth[j] == 0){
        tur.sum[j] <- NA} else {
          depth.nearby <- find.index(train.data$depth[j], TUR$depth)
          depth.min <- min(depth.nearby)
          if (depth.min > 1){
            for (i in 2:depth.min){
              tur.sum[j] <- tur.sum[j] + mean(c(tur.all[i-1, j], tur.all[i, j]), na.rm = TRUE)*(TUR$depth[i] - TUR$depth[i-1])}}
          tur.sum[j] <- tur.sum[j] + mean(c(tur.all[depth.min, j], tur[j]), na.rm = TRUE)*(train.data$depth[j] - TUR$depth[depth.min])}}
  
  return(tur.sum)}
