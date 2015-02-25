#' Plot color matrix
#' 
#' \code{colmat} Plot color matrix for factors/categorical data.
#' 
#' @param x A matrix with \strong{samples by rows} and 
#'    \strong{variables by columns}. See details.
#' @param colframe Logical. Is the dataset already in colors? Default: FALSE
#' @param transpose Logical. Transpose matrix? Default: TRUE
#' @param rowLab Logical. Label rows? Default: TRUE
#' @param colLab Logical. Label columns? Default: TRUE. Printed above plot.
#' @param pal Palette used for when \code{colframe = FALSE}. See details.
#' @param lcol Line color. Default: 'black'
#' @param rowLine \code{line} adjustment for y-axis. See details.
#' @param colLine \code{line} adjustment for x-axis. See details.
#' @param las \code{las} for plot. Default: 2. See \code{\link[graphics]{par}}.
#' @param colSize Size parameter for column labels. See details.
#' 
#' @details Input can either be a matrix where rows are samples and columns
#'    are variables of factors or colors. For a matrix of factors, define 
#'    \code{colFrame = FALSE}, \code{makecolors} with palette \code{pal} is
#'    used to color the matrix. If \code{colFrame = TRUE}, the colors are 
#'    plotted verbatim.
#'    
#'    \code{rowLine} and \code{colLine} are \code{line} arguments for y-lab
#'    and x-lab respectively. See \code{\link[graphics]{par}}. \code{colSize} 
#'    is the \code{cex.axis} argument for x-lab.
#'    
#' @return Plot of a colored table.
#' @export
#' 
#' @examples
#' # Factor variables
#' x1 <- matrix(sample(c('Positive','Negative'),50,replace = TRUE),25,2,
#'              dimnames = list(paste('Sample',1:25,sep='_'),
#'              c('Mutation_1','Mutation_2')))
#' x2 <- sample(c('Papillary','Medullary','Mucinous'),25,replace = TRUE)
#' x <- cbind(x1,Type = x2); rm(x1,x2)
#' colmat(x)
#' 
#' # Color variables
#' x <- matrix(sample(c('skyblue3' , 'tan3'), 75, replace = TRUE), 25, 3, 
#'             dimnames = list(paste('Sample',1:25,sep='_'),
#'                             c('Var1','Var2','Var3')))
#' colmat(x)
#' 
#' @seealso \code{\link{makecolors}}, \code{\link[graphics]{par}}

colmat <- function(x, colframe = FALSE, transpose = TRUE, rowLab = TRUE, colLab = TRUE, pal = NULL, 
                   lcol = "black", rowLine = -2, colLine = -1, las = 2, colSize = 0.7) {
  
  # Set up
  
  if (transpose) {
    dat <- t(x)
  } else {
    dat <- x
  }
  
  if (is.null(pal)) {
    pal <- cbbPalette[c(2:8, 1)]
  }
  if (!(colframe)) {
    datcol <- t(makecolors(t(dat), pal = pal)$Colors)
  } else {
    datcol <- dat
  }
  
  # Define plotting regions
  
  nrowsep <- nrow(dat) + 1
  rowsep <- 1 - seq(0, 1, length.out = nrowsep)
  ncolsep <- ncol(dat) + 1
  colsep <- seq(0, 1, length.out = ncolsep)
  
  # Plots
  
  plot.new()
  for (j in 1:nrow(dat)) {
    for (i in 1:ncol(dat)) {
      rect(colsep[i], rowsep[j], colsep[i + 1], rowsep[j + 1], col = datcol[j, i], border = lcol)
    }
  }
  
  # Y-axis labels
  
  if (rowLab) {
    rowLab <- rownames(dat)
    rowLabpos <- rowsep[-1] - (diff(rowsep)[1]/2)
    axis(2, at = rowLabpos, labels = rowLab, tick = FALSE, las = las, line = rowLine, font.axis = 2)
  }
  if (colLab) {
    colLab <- colnames(dat)
    colLabpos <- colsep[-1] - (diff(colsep)[1]/2)
    axis(3, at = colLabpos, labels = colLab, tick = FALSE, las = las, line = colLine, cex.axis = colSize)
  }
} 