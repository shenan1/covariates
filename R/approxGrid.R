#' Linear interpolate between 2 parallel grids
#'
#' @param x vector of 2 scalars
#' @param y list of 2 grids (must be equi-spaced)
#' @param xout scalar to interpolate at
#'
#' @return A grid of interpolated values (or NA values), with dimension matching that of y[[1]].

# old formula
# approx.grid <- function(x, y, xout) {
#   (y[[1]]*abs(xout - x[2]) + y[[2]]*abs(xout - x[1]))/abs(x[1] - x[2])
# }

approx.grid <- function(x, y, xout) {
  covariate <- matrix(0, dim(y[[1]])[1], dim(y[[1]])[2])
  for (i in 1:dim(y[[1]])[1]){
    for (j in 1:dim(y[[1]])[2]){
      if (is.na(y[[1]][i,j]) & is.na(y[[2]][i,j])){
        covariate[i,j] <- NA
      } else if (is.na(y[[1]][i,j])){
        covariate[i,j] <- y[[2]][i,j]
      } else if (is.na(y[[2]][i,j])){
        covariate[i,j] <- y[[1]][i,j]
      } else {
        covariate[i,j] <- approx(c(x[1], x[2]), 
                                 c(y[[1]][i,j], y[[2]][i,j]), 
                                 xout=xout, method="linear", rule=1, ties=mean)$y
      }
    }
  }
  return(covariate)
}
