#' Extract and save run time, stupid searches and warnings from log.
#' Returns number of warnings.
#' 
#' @param mod.mode result of inla
#' @param directory output directory
#' @param write write the log file (TRUE or FALSE)
#' 
#' @return Fixed effects mean and sd; range 0.025 and stdev 0.975 quantiles.
inla.log <- function(mod.mode, l = 1, k, RData, directory, write = FALSE) {
  
  load(paste(RData, 'runtime.RData', sep = ''))
  load(paste(RData, 'stupid.RData', sep = ''))
  load(paste(RData, 'warnings.RData', sep = ''))
  
  log <- mod.mode$logfile
  
  warning <- 0
  for (j in 1:3) {                                                              # record negative eigenvalue (warning)
    if (as.numeric(log[grep("Eigenvalue", log) + j]) < 0) {
      warning <- warning + 1}}
  
  if (warning == 0) {
    runtime[l, k] <- as.numeric(substr(grep("Total", log, value=TRUE), 27, 29))
    stupid[l, k] <- length(grep("stupid", log))
    save(runtime, file = paste(RData, 'runtime.RData', sep = ''))
    save(stupid, file = paste(RData, 'stupid.RData', sep = ''))} else {
      warnings[l, k] <- warnings[l, k] + 1
      save(warnings, file = paste(RData, 'warnings.RData', sep = ''))}
  
  if (write == TRUE) {
    log.txt <- paste(logs, 'log', l, '_k', k, '.txt', sep = '')
    writeLines(log, log.txt)}
  
  return(warning)}
