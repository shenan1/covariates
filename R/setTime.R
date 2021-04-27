#' Calculate training and predicting times
#' 
#' @param data.time.start time CMEMS & ECMWF records begin
#' @param origin.time time glider records begin
#' @param train.interval interval between training times
#' @param pred.interval interval between prediction times
#' @param flight.interval interval between training flight and prediction flight
#' 
#' @return Training and predicting times.
set.time <- function(gldr.time.start, data.time.start, origin.time, train.length, nt, train.interval = 3600, pred.interval = 3600, flight.interval = 3600) {
  cov.time.training.start <- gldr.time.start - as.numeric(difftime(data.time.start, origin.time, tz = "GMT", units = "secs")) 
  # converts gldr.time.start from being relative to origin.time to being relative to data.time.start
  cov.time.training.end <- cov.time.training.start + (train.length - 1)*train.interval
  cov.time.training <- seq(from = c(cov.time.training.start), to = c(cov.time.training.end), by = train.interval)
  cov.time.predicting.start <- cov.time.training.end + flight.interval
  cov.time.predicting.end <- cov.time.predicting.start + (nt-1)*pred.interval
  cov.time.predicting <- seq(from = c(cov.time.predicting.start), to = c(cov.time.predicting.end), by = pred.interval)
  return(list(cov.time.training, cov.time.predicting))}
