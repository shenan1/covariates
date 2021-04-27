#' Find nearest and neighbour indices
#' 
#' @param arbitrary.value scalar
#' @param standard.values vector
#' 
#' @return Nearest and neighbour indices.
find.index <- function(arbitrary.value, standard.values) {
  index.nearest <- which.min(abs(c(arbitrary.value) - standard.values))
  if (arbitrary.value < standard.values[index.nearest]){
    index.neighbour <- index.nearest - 1} 
  else {
    index.neighbour <- index.nearest + 1}
  return(c(index.nearest, index.neighbour))}
