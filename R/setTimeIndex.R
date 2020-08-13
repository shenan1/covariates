#' Calculate training and predicting time indices
#' 
#' @param dat.time.start time CMEMS & ECMWF records begin
#' @param cov.time covariate time values
#' 
#' @return Training and predicting time indices.
set.time.index <- function(gldr.time.start, dat.time.start, cov.time, train.length, nt) {
  cov.time.start <- gldr.time.start - as.numeric(as.POSIXct(dat.time.start, tz = "GMT", origin="1970-01-01")) # converts gldr.mean start time to cov.time.start
  cov.training.start <- which.min(abs(cov.time - cov.time.start)) # index in cov data the model was started from
  cov.training.end <- cov.training.start + train.length - 1
  cov.training <- cov.training.start:cov.training.end
  cov.predicting <- cov.training.end + 1:nt
  return(list(cov.training, cov.predicting))
}
