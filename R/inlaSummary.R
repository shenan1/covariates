#' Extract and save fixed effects mean and sd.
#' Extract and save practical range 0.025 and sd 0.975 quantiles.
#' 
#' @param mod.mode result of inla()
#' @param n.fixed number of fixed effects
#' @param directory output directory
#' @param write write the results (TRUE or FALSE)
#' 
#' @return Fixed effects mean and sd; range 0.025 and stdev 0.975 quantiles.
inla.summary <- function(mod.mode, n.fixed, l = 1, k, RData, directory, write = FALSE) {
  
  load(paste(RData, 'beta.mean.RData', sep = ''))
  load(paste(RData, 'beta.sd.RData', sep = ''))
  load(paste(RData, 'p.range.RData', sep = ''))
  load(paste(RData, 'stdev.RData', sep = ''))
  
  summary.char <- as.character(summary(mod.mode))
  fixed.effects <- summary.char[3]
  hyperparameters <- summary.char[4]
  if (!any(grepl("mean", fixed.effects))) {                                     # if fixed effects not summarised use else statement
    for (i in 1:n.fixed) {                                                      # extract fixed effects mean and sd
      beta.mean[[i]][l, k] <- strsplit(fixed.effects , ", ")[[1]][i]            # extract ith value separated by commas
      beta.sd[[i]][l, k] <- strsplit(fixed.effects , ", ")[[1]][n.fixed + i]}
    beta.mean[[1]][l, k] <- 
      substr(beta.mean[[1]][l, k], 3, nchar(beta.mean[[1]][l, k]))              # remove first 2 characters
    p.range[l, k] <- strsplit(hyperparameters, ", ")[[1]][8]                    # extract practical range 0.025 quantile
    stdev[l, k] <- strsplit(hyperparameters, ", ")[[1]][15]                     # extract stdev 0.975 quantile
    stdev[l, k] <- substr(stdev[l, k], 1, nchar(stdev[l, k]) - 1)} else {       # remove last character
      for (i in 1:n.fixed) {
        beta.mean[[i]][l, k] <- beta.sd[[i]][l, k] <- NA}
      p.range[l, k] <- strsplit(fixed.effects, ", ")[[1]][8]
      stdev[l, k] <- strsplit(fixed.effects, ", ")[[1]][15]
      stdev[l, k] <- substr(stdev[l, k], 1, nchar(stdev[l, k]) - 1)}
  
  for (i in 1:n.fixed) {
    beta.mean[[i]] <- apply(beta.mean[[i]], 1:2, as.numeric)
    beta.sd[[i]] <- apply(beta.sd[[i]], 1:2, as.numeric)}
  p.range <- apply(p.range, 1:2, as.numeric)
  stdev <- apply(stdev, 1:2, as.numeric)
  
  save(beta.mean, file = paste(RData, 'beta.mean.RData', sep = ''))
  save(beta.sd, file = paste(RData, 'beta.sd.RData', sep = ''))
  save(p.range, file = paste(RData, 'p.range.RData', sep = ''))
  save(stdev, file = paste(RData, 'stdev.RData', sep = ''))
  
  if (write == TRUE) {
    writeLines(fixed.effects, paste(directory, 'fixed_effects', l, '_k', k, '.txt', sep = ''))
    writeLines(hyperparameters, paste(directory, 'hyperparameters', l, '_k', k, '.txt', sep = ''))
    capture.output(summary(mod.mode), file = paste(directory, 'summary', l, '_k', k, '.txt', sep = ''))}
  
  return(invisible(NULL))}
