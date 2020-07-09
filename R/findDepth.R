#' Find nearest and neighbour depth indices
#' 
#' @param dat.depth tem.depth, cur.depth, chl.depth ...
#' 
#' @return Nearest and neighbour depth indices when training
find.depth <- function(gldr.mean, i, dat.depth) {
  depth.nearest <- which.min(abs(gldr.mean$depth[i] - dat.depth))
  if (gldr.mean$depth[i] <= dat.depth[depth.nearest]){
    depth.neighbour <- depth.nearest - 1
  } else {
    depth.neighbour <- depth.nearest + 1
  }
  return(c(depth.nearest, depth.neighbour))
}
