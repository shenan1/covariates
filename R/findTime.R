#' Find nearest and neighbour time indices
#' 
#' @param cov.time mld.time, chl.time, ...
#' @param cov.time.simulating cov.time.training, cov.time.predicting
#' 
#' @return Nearest and neighbour time indices.
find.time <- function(cov.time.simulating, i, cov.time) {
  time.nearest <- which.min(abs(cov.time.simulating[i] - cov.time))
  if (cov.time.simulating[i] < cov.time[time.nearest]){
    time.neighbour <- time.nearest - 1
  } else {
    time.neighbour <- time.nearest + 1
  }
  return(c(time.nearest, time.neighbour))
}
