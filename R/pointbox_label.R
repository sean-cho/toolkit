#' Boxplots with points and labels
#' 
#' \code{pointbox_label} Plot boxplots with points and labels each point.
#' 
#' @param value Numeric variable for N measurements.
#' @param grp Classification of measurements.
#' @param label_names Character vector of length value. Use NA to hide specific names.
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
#' @details Colors for boxes and points are defined separately to allow for
#'    fine-tuned user control.
#'    
#' @return Boxplot with horizontally jittered points.
#' @export
#' 
#' @examples
#' # Make data
#' d <- c(rnorm(n = 10, mean = 1, sd = 0.5),
#'        rnorm(n = 10, mean = 5, sd = 0.5))
#' l <- paste0('L', 1:20)
#' g <- c(rep('a', 10), rep('b', 10))
#' pointbox_label(value = d, grp = g, label_names = l)
#' 
#' @seealso \code{\link[graphics]{par}}


pointbox_label <- function(value, grp, label_names = NULL,
                           b_col = NULL, s_col = NULL,
                           s_pch = 16, s_jitter = NULL,
                           showN = TRUE, heightN = NULL,
                           stat_test = TRUE, y_range = NULL,
                           ...){
  
  y_min <- min(value, na.rm = TRUE)
  y_max <- max(value, na.rm = TRUE)
  if(showN) y_max <- max(value) + abs(max(value))*0.25
  if(showN | is.null(y_range)) y_range <- c(y_min, y_max)
  
  BP <- boxplot(value ~ grp, col = b_col, pch = NA, ylim = y_range, ...)
  
  .mapper <- 1:length(BP$names)
  names(.mapper) <- BP$names
  x_loc <- .mapper[as.character(grp)]
  x_pos <- jitter(x_loc, amount = s_jitter)
  
  if(is.null(s_col)) s_col <- scales::alpha('black', 0.5)
  points(x = x_pos, y = value, pch = s_pch, col = s_col)
  
  if(is.null(heightN)) heightN = y_max
  
  if(showN) text(x = 1:length(BP$n), y = heightN, pos = 1, 
                 labels = paste0('n = ', BP$n))
  
  if(!is.null(label_names)){
    if(length(label_names) != length(value)){
      stop('Label names and values do not have the same lengths.')
    }
    side <- ifelse(x_loc == 1, 4, 2)
    text(x = x_pos, y = value, pos = side, labels = label_names)
  }
  
  if(stat_test){
    if(length(unique(grp)) == 2){
      test_res <- signif(t.test(value ~ grp)$p.value, 2)
      title(main = paste0('Welch\'s corrected t-test p-value = ', test_res), font.main = 1, 
            cex.main = 0.8, line = 0.5)
    } else {
      test_res <- signif(summary(aov(value ~ grp))[[1]][,'Pr(>F)'][1], 2)
      title(main = paste0('One-way ANOVA p-value = ', test_res), font.main = 1, 
            cex.main = 0.8, line = 0.5) 
    }
  }
}