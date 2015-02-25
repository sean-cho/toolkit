#' Calculate cumulative midpoints
#' 
#' \code{midPoints} Calculate cumulative midpoints of a numeric vector.
#' 
#' @param x A numeric vector
#' 
#' @details Calculates the cumulative midpoints of a numeric vector. To
#'    be used in plotting functions, especially for Manhattan plots.
#' 
#' @return A numeric vector of length \code{length(x) - 1}
#' @export
#' 
#' @examples
#' midPoints(rep(1, 10))
#' 

midPoints <- function(x){
  dum <- c(0,cumsum(x))
  Ans <- dum[-length(dum)] + diff(dum)/2
  return(Ans)
}