#' Boxplots with points
#' 
#' \code{pointbox} Plot boxplots with points (scatterplot).
#' \code{pointbox_label} Plot boxplots with points and labels each point.
#' 
#' @param value Numeric variable for N measurements.
#' @param grp Classification of measurements.
#' @param b_col Boxplot color. Default: NULL (black)
#' @param s_col Scatterplot color. Default: NULL (black). If specified, length of N, 
#'    as each point can be colored separately.
#' @param s_pch Point type. Default: 16, filled circle.
#' @param s_jitter Jitter factor for points.
#' @param showN Logical. Show number of samples in each classification.
#' @param heightN Manually set height for N if showN = TRUE.
#' @param stat_test Logical. Perform statistical test? Currently, only one-sided
#'    anova is implemented.
#' @param y_range Manually specify y-axis range.
#' @param s_outline Logical. Outline points.
#' @param ... Arguments passed on to par. See \code{\link[graphics]{par}}.
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
#' @seealso \code{\link[graphics]{par}}

pointbox <- function(value, grp, 
                     b_col = NULL, s_col = NULL,
                     s_pch = 16, s_jitter = NULL,
                     showN = TRUE, heightN = NULL,
                     stat_test = TRUE, y_range = NULL,
                     s_outline = TRUE, ...){
  
  y_min <- min(value, na.rm = TRUE)
  y_max <- max(value, na.rm = TRUE)
  if(showN) y_max <- max(value) + abs(max(value))*0.1
  if(showN | is.null(y_range)) y_range <- c(y_min, y_max)
  
  BP <- boxplot(value ~ grp, col = b_col, pch = NA, ylim = y_range, ...)
  
  .mapper <- 1:length(BP$names)
  names(.mapper) <- BP$names
  x_loc <- .mapper[as.character(grp)]
  x_loc_plot <- jitter(x_loc)
  
  if(is.null(s_col)) s_col <- scales::alpha('black', 0.5)
  points(x = x_loc_plot, y = value, pch = s_pch, col = s_col)
  if(s_outline) points(x = x_loc_plot, y = value, pch = 1, col = 'black')
  
  if(is.null(heightN)) heightN = y_max
  
  if(showN) text(x = 1:length(BP$n), y = heightN, pos = 1, 
                 labels = paste0('n = ', BP$n))
  
  if(stat_test){
    test_res <- signif(summary(aov(value ~ grp))[[1]][,'Pr(>F)'][1], 2)
    title(main = paste0('One-way ANOVA p-value = ', test_res), font.main = 1, 
          cex.main = 0.8, line = 0.5)
  }
}

pointbox_label <- function(value, grp, 
                           b_col = NULL, s_col = NULL,
                           s_pch = 16, s_jitter = NULL,
                           showN = TRUE, heightN = NULL,
                           stat_test = TRUE, y_range = NULL,
                           ...){
  
  y_min <- min(value, na.rm = TRUE)
  y_max <- max(value, na.rm = TRUE)
  if(showN) y_max <- max(value) + abs(max(value))*0.1
  if(showN | is.null(y_range)) y_range <- c(y_min, y_max)
  
  BP <- boxplot(value ~ grp, col = b_col, pch = NA, ylim = y_range, ...)
  
  .mapper <- 1:length(BP$names)
  names(.mapper) <- BP$names
  x_loc <- .mapper[as.character(grp)]
  
  if(is.null(s_col)) s_col <- scales::alpha('black', 0.5)
  points(x = jitter(x_loc), y = value, pch = s_pch, col = s_col)
  
  if(is.null(heightN)) heightN = y_max
  
  if(showN) text(x = 1:length(BP$n), y = heightN, pos = 1, 
                 labels = paste0('n = ', BP$n))
  
  if(stat_test){
    test_res <- signif(summary(aov(value ~ grp))[[1]][,'Pr(>F)'][1], 2)
    title(main = paste0('One-way ANOVA p-value = ', test_res), font.main = 1, 
          cex.main = 0.8, line = 0.5)
  }
}