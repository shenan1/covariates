#' Calculate training and predicting times
#' 
#' @param cmems.time.start time CMEMS records begin
#' 
#' @return Training and predicting times.
set.time <- function(gldr.time.start, cmems.time.start, train.length, nt) {
  cov.time.training.start <- gldr.time.start - as.numeric(as.POSIXct(cmems.time.start, tz = "GMT", origin="1970-01-01")) # converts gldr.mean start time to cov.time.start
  cov.time.training.end <- cov.time.training.start + (train.length - 1)*3600
  cov.time.training <- seq(from = cov.time.training.start, to = cov.time.training.end, by = 3600)
  cov.time.predicting.start <- cov.time.training.end + 3600
  cov.time.predicting.end <- cov.time.predicting.start + (nt-1)*3600
  cov.time.predicting <- seq(from = cov.time.predicting.start, to = cov.time.predicting.end, by = 3600)
  return(list(cov.time.training, cov.time.predicting))
}
