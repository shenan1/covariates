#' Extract and save fixed effects mean and sd.
#' Extract and save practical range 0.025 and sd 0.975 quantiles.
#' 
#' @param mod.mode result of inla()
#' @param n.fixed number of fixed effects
#' @param directory output directory
#' @param write write the results (TRUE or FALSE)
#' 
#' @return Fixed effects mean and sd; range 0.025 and stdev 0.975 quantiles.
inla.summary <- function(mod.mode, n.fixed, n.hyper, l = 1, k, RData, directory, write = FALSE) {
  
  load(paste(RData, 'beta.mean.RData', sep = ''))
  load(paste(RData, 'beta.sd.RData', sep = ''))
  load(paste(RData, 'p.range.RData', sep = ''))
  load(paste(RData, 'stdev.RData', sep = ''))
  load(paste(RData, 'rho.RData', sep = ''))
  
  summary.char <- as.character(summary(mod.mode))
  fixed.effects <- summary.char[3]
  hyperparameters <- summary.char[4]
  if (!any(grepl("mean", fixed.effects))) {                                     # if fixed effects not summarised use else statement
    for (i in 1:n.fixed) {                                                      # extract fixed effects mean and sd
      beta.mean[[i]][l, k] <- strsplit(fixed.effects, ", ")[[1]][i]             # extract ith value separated by commas
      beta.sd[[i]][l, k]   <- strsplit(fixed.effects, ", ")[[1]][n.fixed + i]}
    beta.mean[[1]][l, k] <- 
      substr(beta.mean[[1]][l, k], 3, nchar(beta.mean[[1]][l, k]))              # remove first 2 characters
    p.range[l, k] <- strsplit(fixed.effects, ", ")[[1]][2*n.hyper + 2]          # extract practical range 0.025 quantile
    stdev[l, k]   <- strsplit(fixed.effects, ", ")[[1]][4*n.hyper + 3]          # extract stdev 0.975 quantile
    rho[l, k]     <- strsplit(fixed.effects, ", ")[[1]][3*n.hyper]                # extract rho 0.025 quantile
    rho[l, k]     <- substr(rho[l, k], 1, nchar(rho[l, k]) - 1)} else {           # remove last character
      for (i in 1:n.fixed) {
        beta.mean[[i]][l, k] <- beta.sd[[i]][l, k] <- NA}
      p.range[l, k] <- strsplit(fixed.effects, ", ")[[1]][2*n.hyper + 2]
      stdev[l, k]   <- strsplit(fixed.effects, ", ")[[1]][4*n.hyper + 3]
      rho[l, k]     <- strsplit(fixed.effects, ", ")[[1]][3*n.hyper]
      rho[l, k]     <- substr(rho[l, k], 1, nchar(rho[l, k]) - 1)}
  
  for (i in 1:n.fixed) {
    beta.mean[[i]] <- apply(beta.mean[[i]], 1:2, as.numeric)
    beta.sd[[i]]   <- apply(beta.sd[[i]], 1:2, as.numeric)}
  p.range <- apply(p.range, 1:2, as.numeric)
  stdev   <- apply(stdev, 1:2, as.numeric)
  rho     <- apply(rho, 1:2, as.numeric)
  
  save(beta.mean, file = paste(RData, 'beta.mean.RData', sep = ''))
  save(beta.sd, file = paste(RData, 'beta.sd.RData', sep = ''))
  save(p.range, file = paste(RData, 'p.range.RData', sep = ''))
  save(stdev, file = paste(RData, 'stdev.RData', sep = ''))
  save(rho, file = paste(RData, 'rho.RData', sep = ''))
  
  if (write == TRUE) {
    writeLines(fixed.effects, paste(directory, 'fixed.effects.', l, '.k.', k, '.txt', sep = ''))
    writeLines(hyperparameters, paste(directory, 'hyperparameters.', l, '.k.', k, '.txt', sep = ''))
    capture.output(summary(mod.mode), file = paste(directory, 'summary.', l, '.k.', k, '.txt', sep = ''))}

  return(invisible(NULL))}
