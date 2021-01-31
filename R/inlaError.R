#' Identify whether inla crashed.
#' 
#' @param mod.mode result of inla
#' 
#' @return 0 = no crash; 1 = crash.
inla.error <- function(mod.mode, l = 1, k, RData) {
  
  load(paste(RData, 'errors.RData', sep = ''))
  
  if (class(mod.mode) != "inla") {
    errors[l, k] <- errors[l, k] + 1
    print(errors[l, k])
    save(errors, file = paste(RData, 'errors.RData', sep = ''))} else {
      errors[l, k] <- 0}
  print(errors[l, k])
  
  return(errors[l, k])}
