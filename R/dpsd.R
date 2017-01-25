#' Differential probes by standard deviation 
#' 
#' \code{dpsd} Find most variable features by standard deviation.
#' 
#' @param x Matrix of numbers with columns indicating samples and rows
#'    indicating features.
#' @param n Number of features to choose.
#' @param .order Logical. Return features in decreasing SD values.
#' 
#' @details Identifies the most variable features across samples by
#'    standard deviation (sd). The \code{n} features with highest sd
#'    is returned as a matrix.
#'    
#' @return A matrix of nrow = n features with highest sd.
#' @export
#' 
#' @examples
#' x1 <- matrix(rnorm(1000), nrow = 50)
#' rownames(x1) = paste('LowSD', 1:50, sep='_')
#' x2 <- matrix(rnorm(1000, sd = 3), nrow = 50)
#' rownames(x2) = paste('HighSD', 1:50, sep='_')
#' dpsd(rbind(x1,x2),20)
#' 

dpsd <- function(x, n, .order = TRUE){
  dumsd <- apply(x, 1, sd, na.rm=TRUE)
  if(.order){
    ix <- order(dumsd, decreasing = TRUE)
    x <- x[ix,]
    dumsd <- dumsd[ix]
  }
  dumrows <- dim(x)[1]
  dum <- x[dumsd >= quantile(dumsd, (dumrows-n)/dumrows, na.rm=TRUE),]
  return(dum)
}