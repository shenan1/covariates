#' Identify whether inla crashed.
#' 
#' @param mod.mode result of inla
#' 
#' @return 0 = no crash; 1 = crash.
inla.error <- function(mod.mode, l = 1, k, RData) {
  
  load(paste(RData, 'errors.RData', sep = ''))
  
  if (class(mod.mode) != "inla") {
    errors[l, k] <- errors[l, k] + 1
    save(errors, file = paste(RData, 'errors.RData', sep = ''))}
  
  return(errors[l, k])}
