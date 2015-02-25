#' Quick and dirty significance tests
#' 
#' \code{summarizett} Perform significance tests as defined across columns in 
#'    a data.frame/matrix.
#' 
#' @param dat \code{Data.frame}/\code{matrix} with dsamples as rows and
#'    variables as columns.
#' @param phenotype Vector of phenotypes. See details.
#' @param maxgroups Maximum number of groups for contigency table test.
#' @param sig P-value cut-off for making a significant call.
#' @param adjmethod Method for p-value correction. Default: 'bonferroni'.
#' @param num.test \code{wilcox.test} (default) or \code{t.test}
#' @param cl.test \code{fisher.test} (default) or \code{chisq.test}
#' 
#' @details This function allows the user to quickly analyze a set of variables
#'    against a set of samples with 2 or more phenotypes. Since a \code{t.test}
#'    or \code{wilcox.test} is used for continuous variables, variables
#'    with >2 phenotypes are restricted only to ordinal/categorical variables.
#'    
#'    Input is a data.frame (preferably) or matrix where columns correspond
#'    to variables and rows correspond to samples. Depending on the class
#'    of the column, a different test is used. If the class is numeric, 
#'    the defined \code{num.test} is used. If the class is factor/character,
#'    \code{cl.test} is used instead.
#'    
#'    Adjusted p-values are calculated using defined \code{adjmethod}.
#' 
#' @return A data.frame with the following columns:
#'    \describe{
#'      \item{pval:}{p-value of the specific test.}
#'      \item{sigp:}{p < \code{sig}.}
#'      \item{adjp:}{adjusted p-value.}
#'      \item{sigadj:}{adjp < \code{sig}}
#'    }
#'    
#' @examples
#' pheno <- factor(sample(c('case','control'), 50, replace = TRUE))
#' 
#' dat <- matrix(rnorm(100),nrow = 50, ncol = 2)
#' dat <- cbind(dat, jitter(as.numeric(pheno)))
#' dat <- cbind(dat, sample(c('Pos','Neg'),50,replace = TRUE))
#' dat <- as.data.frame(dat, stringsAsFactors = FALSE)
#' colnames(dat) <- paste('Var',1:4,sep='_')
#' 
#' # Run summarizett. Var_3 is significant.
#' summarizett(dat, pheno)
#' 

summarizett <- function(dat, phenotype, maxgroups = 5, sig = 0.05, 
                       adjmethod = "bonferroni", num.test = wilcox.test, 
                       cl.test = fisher.test) {
  if (length(phenotype) == 1) {
    if (is.na(match(phenotype, colnames(dat)))) 
      stop("Phenotype not found in column names")
    pheno <- as.vector(dat[, phenotype])
    dat <- dat[, !colnames(dat) %in% phenotype]
    phenotype <- pheno
  }
  if (length(unique(phenotype)) == 1) 
    stop("Phenotype must have 2 or more levels")
  ct <- apply(dat, 2, function(x) length(unique(x)))
  gcl <- suppressWarnings(apply(dat, 2, 
                               function(x) !any(is.na(as.numeric(as.vector(x))))))
  dat <- dat[, (ct != 1 & ct <= maxgroups) | gcl]
  dm <- apply(dat, 2, tabletest, phenotype = phenotype, num.test = num.test, 
             cl.test = cl.test, collapse = TRUE)
  ans <- data.frame(pval = signif(dm, 3), sigp = dm <= sig, 
                   adjp = signif(p.adjust(dm, method = adjmethod), 3), 
                   sigadj = signif(p.adjust(dm, method = adjmethod), 3) <= sig)
  return(ans)
} 