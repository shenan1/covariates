#' Calculate number of dives.
#' 
#' @param pilot.start time pilot starts work [hour]
#' @param pilot.end time pilot ends work [hour] (24-hour)
#' @param dives.flight standard number of dives per flight
#' @param seconds.dive number of seconds per dive
#' @param comms communication time with glider between flights [minutes]
#' 
#' @return Number of dives.
n.dives <- function(gldr.time.end, pilot.start = 7, pilot.end = 22, dives.flight = 3, seconds.dive = 3600, comms = 20) {
gldr.time.end.mod <- gldr.time.end %% (24*3600)
last.dive <- pilot.end*3600 - dives.flight*seconds.dive - comms*60
first.dive <- (pilot.start + 24)*3600
if (gldr.time.end.mod < last.dive){
  n.dives <- dives.flight} else {
    n.dives <- ceiling((first.dive - (gldr.time.end.mod + comms*60))/seconds.dive)}}
