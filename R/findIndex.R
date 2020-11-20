#' Find nearest and neighbour indices
#' 
#' @param arbitrary.values
#' @param standard.values
#' 
#' @return Nearest and neighbour indices.
find.index <- function(arbitrary.values, i, standard.values) {
  index.nearest <- which.min(abs(arbitrary.values[i] - standard.values))
  if (arbitrary.values[i] < standard.values[index.nearest]){
    index.neighbour <- index.nearest - 1} 
  else {
    index.neighbour <- index.nearest + 1}
  return(c(index.nearest, index.neighbour))}
