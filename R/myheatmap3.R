#' Personalized heatmap.3
#' 
#' \code{myheatmap3} Runs \code{heatmap.3} with personalized color scheme.
#' 
#' @param x Numeric matrix of values to be plotted. Data.frame will be coerced.
#' @param col Defaults to a blue-black-yellow color scheme with 50 shades
#' @param ... Other arguments to be passed on to \code{heatmap.3}
#' 
#' @details Turns of \code{density.info} and \code{trace} and sets the
#' color scheme to blue-black-yellow. Other arguments can be passed onto
#' \code{heatmap.3}. Most commonly used are \code{ColSideCol} and 
#' \code{RowSideCol}.
#' 
#' @return An object of the class list from \code{heatmap.3}.
#' @export

myheatmap3 = function(x,col=colorRampPalette(c("blue","black","yellow"))(50),...){
  heatmap.3(as.matrix(x),density.info="none",
            col=col,
            trace="none",...)
}