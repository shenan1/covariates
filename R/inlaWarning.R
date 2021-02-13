#' Returns number of warnings from log.
#' If no warnings, extracts and saves run time and number of stupid searches from log.
#' If warning(s), prints "INLA warning(s); repeating calculation...".
#' 
#' @param mod.mode result of inla
#' @param directory output directory
#' @param write write the log file (TRUE or FALSE)
#' 
#' @return 0 = no warnings; !0 = warnings.
inla.warning <- function(mod.mode, n.fixed, l = 1, k, RData, directory, write = FALSE) {
  
  load(paste(RData, 'runtime.RData', sep = ''))
  load(paste(RData, 'stupid.RData', sep = ''))
  load(paste(RData, 'warnings.RData', sep = ''))
  
  log <- mod.mode$logfile
  
  warning <- 0
  for (j in 1:n.fixed) {                                                              # record negative eigenvalue (warning)
    if (as.numeric(log[grep("Eigenvalue", log) + j]) < 0) {
      warning <- warning + 1}}
  
  if (warning != 0) {
    warnings[l, k] <- warnings[l, k] + 1
    print("INLA warning(s); repeating calculation...")
    save(warnings, file = paste(RData, 'warnings.RData', sep = ''))} else {
      runtime[l, k] <- as.numeric(substr(grep("Total", log, value=TRUE), 27, 29))
      stupid[l, k] <- length(grep("stupid", log))
      save(runtime, file = paste(RData, 'runtime.RData', sep = ''))
      save(stupid, file = paste(RData, 'stupid.RData', sep = ''))
      if (write == TRUE) {
        log.txt <- paste(logs, 'log', l, '_k', k, '.txt', sep = '')
        writeLines(log, log.txt)}}
  
  return(warning)}
