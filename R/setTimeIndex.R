#' Calculate training and predicting time indices
#' 
#' @param data.time.start time CMEMS & ECMWF records begin
#' @param origin.time time glider records begin
#' @param cov.time covariate time values
#' 
#' @return Training and predicting time indices.
set.time.index <- function(gldr.time.start, data.time.start, origin.time, cov.time, train.length, nt) {
  cov.time.start <- gldr.time.start - as.numeric(difftime(data.time.start, origin.time, tz = "GMT", units = "secs")) 
  # converts gldr.time.start from being relative to origin.time to being relative to data.time.start
  cov.training.start <- which.min(abs(cov.time - cov.time.start)) # index in cov data the model was started from
  cov.training.end <- cov.training.start + train.length - 1
  cov.training <- cov.training.start:cov.training.end
  cov.predicting <- cov.training.end + 1:nt
  return(list(cov.training, cov.predicting))}
