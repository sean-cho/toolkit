#' Table to matrix
#' 
#' \code{t2m} Convert \code{table} results to matrix
#' 
#' @param x Result from \code{table}
#' 
#' @details Converts \code{table} into matrix.
#' 
#' @return A matrix.
#' @export
#' 
#' @examples
#' x1 <- sample(c('a','b'),20,replace = TRUE)
#' x2 <- sample(c('0','1'),20,replace = TRUE)
#' t2m(table(x1,x2))

t2m <- function(x) {
  if (class(x) != "table") 
    stop("Object is not table")
  if (length(dim(x)) == 1) {
    warning("Object has 1 dimension. Result will be returned as array.")
  }
  dmdim <- dim(x)
  dm <- matrix(x)[, 1]
  dim(dm) <- dmdim
  dmrn <- rownames(x)
  rownames(dm) <- dmrn
  if (length(dim(x)) != 1) {
    dmcn <- colnames(x)
    colnames(dm) <- dmcn
  }
  return(dm)
} 