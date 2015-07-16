#' Display image frames in a row
#' 
#' Given a seqence of frames, generate a single image with frames tiled in a row.
#' 
#' @param img an \code{Image}
#' @param lwd width of the grid lines between tiled images
#' @export
#' @importFrom abind asub
displayTiles = function(img, lwd = 10) {
  tiles = tile(img, nx = numberOfFrames(img, "render"), lwd = lwd, fg.col = "white")
  dt = dim(tiles) - 2*lwd
  tiles = asub(tiles, list(lwd+1:dt[1L], lwd+1:dt[2L]), c(1L, 2L))
  display(tiles)
}

tilesDim = function(dim, nf, lwd = 10) c( (dim[1]+lwd) * nf -lwd, dim[2])
