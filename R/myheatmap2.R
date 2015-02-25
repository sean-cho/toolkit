#' Personalized heatmap.2
#' 
#' \code{myheatmap2} Runs \code{heatmap.2} with personalized color scheme.
#' 
#' @param x Numeric matrix of values to be plotted. Data.frame will be coerced.
#' @param col Defaults to a blue-black-yellow color scheme with 50 shades
#' @param ... Other arguments to be passed on to \code{heatmap.2}
#' 
#' @details Turns of \code{density.info} and \code{trace} and sets the
#' color scheme to blue-black-yellow. Other arguments can be passed onto
#' \code{heatmap.2}. Most commonly used are \code{ColSideCol} and 
#' \code{RowSideCol}.
#' 
#' @return An object of the class list from \code{heatmap.2}.
#' 
#' @seealso \code{\link[gplots]{heatmap.2}}
#' 
#' @importFrom gplots heatmap.2

myheatmap2 <- function(x, col = colorRampPalette(c("blue", "black", "yellow"))(50), ...) {
  if (!requireNamespace("gplots", quietly = TRUE)) {
    stop("Package gplots needed for this function. Please install it", call. = FALSE)
  }
  heatmap.2(as.matrix(x), density.info = "none", col = col, trace = "none", ...)
} 