#' Linear interpolate between 2 parallel grids
#'
#' @param x vector of 2 scalars
#' @param y list of 2 grids (must be equi-spaced)
#' @param xout scalar to interpolate at
#'
#' @return A grid of interpolated values (or NA values), with dimension matching that of y[[1]].
approx.grid <- function(x, y, xout) {
  (y[[1]]*abs(xout - x[2]) + y[[2]]*abs(xout - x[1]))/abs(x[1] - x[2])
}
