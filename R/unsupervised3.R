#' Unsupervised heatmap by standard deviation
#' 
#' \code{unsupervised.3} Performs unsupervised clustering on the most
#'    variable features using heatmap.3 which allows for multiple Row/ColSideCol.
#'    
#' @param x Numeric matrix with samples in columns and features in rows.
#' @param n Number of features
#' @param col Color for heatmap. Defaults to blue-black-yellow color scheme.
#' @param ... Passes arguments to \code{heatmap.3}
#' 
#' @details Finds the most variable features across all samples by standard
#'    deviation, clusters them, and plots it on a heatmap using the
#'    \code{heatmap.3} function.
#'    
#' @return A heatmap.3 object and plot.
#' @export

unsupervised.3 = function(x,n,col=colorRampPalette(c("blue","black","yellow"))(50),...){
  dumsd = apply(x,1,sd,na.rm=TRUE)
  dumrows = dim(x)[1]
  dum = x[dumsd>=quantile(dumsd,(dumrows-n)/dumrows,na.rm=TRUE),]
  myheatmap3(dum,main=paste("Top",round(n),"Probes",sep=" "),col = col,...)
}
