#' Read dictionary
#' 
#' \code{read_dict} Read .dict files generated using Picard tools as seqinfo objects.
#' 
#' @param dict .dict file containing reference information to read.
#' @param genome Genome name. ['hs37d5']
#' 
#' @details Reads an input .dict file as a seqinfo object to be used in
#' GenomicRanges.
#' 
#' @import dplyr GenomicRanges
#'    
#' @return A seqinfo object to be used with GenomicRanges.
#' @export
#' 
#' @examples
#' x1 <- matrix(rnorm(1000), nrow = 50)
#' rownames(x1) = paste('LowSD', 1:50, sep='_')
#' x2 <- matrix(rnorm(1000, sd = 3), nrow = 50)
#' rownames(x2) = paste('HighSD', 1:50, sep='_')
#' dpsd(rbind(x1,x2),20)
#' 

read_dict <- function(dict, genome = 'hs37d5'){
  # Reads genome.dict to populate seqinfo field in GRanges object
  # Since ExAC only scores autosomal chromosomes, this will restrict to those
  require(dplyr)
  require(GenomicRanges)
  d <- read.delim(dict, stringsAsFactors = FALSE, header = FALSE, skip = 1,
                  col.names = c('SQ', 'SN', 'LN', 'M5', 'FILE')) %>%
    mutate(
      SN = gsub('SN:', '', SN),
      LN = gsub('LN:', '', LN)
    )
  s <- with(d, Seqinfo(seqnames = SN, seqlengths = as.numeric(LN), 
                       isCircular = rep(FALSE, nrow(d)), genome = genome))
  return(s)
}