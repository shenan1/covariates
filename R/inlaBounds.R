#' Identify whether grid.mean is out of bounds.
#' 
#' @param grid.mean mean predictions on grid
#' 
#' @return 0 = in bounds; 1 = out of bounds.
inla.bounds <- function(grid.mean, l = 1, k, RData, lower.bound, upper.bound, train.data, count.limit) {
  
  load(paste(RData, 'l.count.RData', sep = ''))
  
  if (((min(grid.mean) > lower.bound*min(train.data, na.rm = T)) &              # if grid.mean within bounds or count.limit exceeded
       (max(grid.mean) < upper.bound*max(train.data, na.rm = T))) |
      (l.count[l, k] == count.limit)) {bound <- 0} else {
        bound <- 1
        print("Predictions out of bounds. Range:")
        print(range(grid.mean, na.rm = TRUE))
        l.count[l, k] <- l.count[l, k] + 1
        save(l.count, file = paste(RData, 'l.count.RData', sep = ''))}
  
  return(bound)}
