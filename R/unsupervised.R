#' Unsupervised heatmap by standard deviation
#' 
#' \code{unsupervised} Performs unsupervised clustering on the most
#'    variable features.
#'    
#' @param x Numeric matrix with samples in columns and features in rows.
#' @param n Number of features
#' @param col Color for heatmap. Defaults to blue-black-yellow color scheme.
#' @param ... Passes arguments to \code{heatmap.2}
#' 
#' @details Finds the most variable features across all samples by standard
#'    deviation, clusters them, and plots it on a heatmap using the
#'    \code{heatmap.2} function.
#'    
#' @return A heatmap.2 object and plot.
#' @export
#'    
#' @seealso \code{\link[gplots]{heatmap.2}}

unsupervised <- function(x,n,col=colorRampPalette(c("blue","black","yellow"))(50),main = NULL,...){
  dumsd <- apply(x,1,sd,na.rm=TRUE)
  dumrows <- dim(x)[1]
  dum <- x[dumsd>=quantile(dumsd,(dumrows-n)/dumrows,na.rm=TRUE),]
  if(is.null(main)){
    Main = paste("Top ", round(n)," Probes\nN = ", ncol(x), sep="")
  } else {
    Main = main
  }
  myheatmap2(dum,main=Main,col = col,...)
}
