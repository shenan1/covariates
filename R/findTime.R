#' Find nearest and neighbour time indices
#' 
#' @param dat.time mld.time, chl.time, ...
#' @param dat.time.simulating dat.time.training, dat.time.predicting
#' 
#' @return Nearest and neighbour time indices.
find.time <- function(dat.time.simulating, i, dat.time) {
  time.nearest <- which.min(abs(dat.time.simulating[i] - dat.time))
  if (dat.time.simulating[i] <= dat.time[time.nearest]){
    time.neighbour <- time.nearest - 1
  } else {
    time.neighbour <- time.nearest + 1
  }
  return(c(time.nearest, time.neighbour))
}
