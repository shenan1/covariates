#' Find nearest and neighbour depth indices when predicting
#' 
#' @param cov.depth tem.depth, cur.depth, chl.depth ...
#' 
#' @return Nearest and neighbour depth indices.
find.depth.grid <- function(gldr.depth.mean, cov.depth) {
  depth.nearest <- which.min(abs(gldr.depth.mean - cov.depth))
  if (gldr.depth.mean < cov.depth[depth.nearest]){
    depth.neighbour <- depth.nearest - 1
  } else {
    depth.neighbour <- depth.nearest + 1
  }
  return(c(depth.nearest, depth.neighbour))
}
