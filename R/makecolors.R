#' Make color labels for factors/categorical data.
#' 
#' \code{makecolors} Generates color labels and legends for factors.
#' 
#' @param object A matrix of vector that can be coerced into factors.
#' @param pal A vector of colors.
#' @param keep.order Logical. Keep original order of non-factor strings.
#' @details The function accepts either a vector that can be coerced into 
#' factors or a matrix where each \strong{column} is used to generate color 
#' labels.
#' 
#' Can also be generate unique \code{pch} labels (see \bold{Examples}).
#' 
#' @return A list containing:
#'    \describe{
#'      \item{Legend:}{A unique vector/color pair for easy legend plotting.}
#'      \item{Colors:}{A matrix or vector of colors for plotting.}
#'    }
#' 
#' @import RColorBrewer
#' @export makecolors
#' 
#' @examples
#' x <- factor(sample(c('a','b','c'), 30, replace = TRUE))
#' colset <- makecolors(x)
#' xnum <- jitter(as.numeric(x))
#' plot(xnum, col = colset$Colors, pch = 16)
#' legend('topright', legend = names(colset$Legend), col = colset$Legend, pch = 16)
#' 
#' pchset <- makecolors(x, pal = c(16,17,18))$Colors
#' plot(xnum, col = colset$Colors, pch = pchset)

makecolors = function(object,pal=cbbPalette,keep.order=TRUE) {
  require(RColorBrewer)
  if(is.null(dim(object))){
    ANS = list()
    CLfactor = inherits(object,"factor")
    if(keep.order & CLfactor) {dum.order = levels(object)}
    object = as.character(object)
    tempPal = pal
    obj.unique = unique(object)
    obj.colors = tempPal[1:length(obj.unique)]
    names(obj.colors) = obj.unique
    color.set = obj.colors[object]
    names(color.set) = object
    if(keep.order & CLfactor) {obj.colors = obj.colors[dum.order]}
    ANS$Legend = obj.colors
    ANS$Colors = color.set
    return(ANS)
  }
  if(is.null(dim(object))==FALSE){
    color.set.mat = matrix(NA,nrow(object),ncol(object))
    legend.list = list()
    tempPal = pal
    for(i in 1:ncol(object)){
      object.dum = apply(object,2,as.character)
      obj.unique = unique(object.dum[,i])
      obj.colors = tempPal[1:length(obj.unique)]
      names(obj.colors) = obj.unique
      color.set.mat[,i] = obj.colors[object.dum[,i]]
      legend.list[[i]] = obj.colors
    }
    colnames(color.set.mat) = colnames(object)
    rownames(color.set.mat) = rownames(object)
    names(legend.list) = colnames(object)
    ANS = list(Legend=legend.list,Colors=color.set.mat)
    return(ANS)
  }
}

