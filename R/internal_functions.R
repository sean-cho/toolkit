tabletest <- function(measurement, phenotype, collapse = FALSE, 
                      num.test = wilcox.test, cl.test = fisher.test) {
  ix <- complete.cases(measurement)
  cl <- suppressWarnings(!any(is.na(as.numeric(as.vector(measurement)))))
  measurement <- measurement[ix]
  phenotype <- phenotype[ix]
  if (cl) {
    measurement <- as.numeric(as.vector(measurement))
    ans <- num.test(measurement ~ phenotype)
  } else {
    measurement <- as.vector(measurement)
    ans <- cl.test(measurement, phenotype)
  }
  if (collapse) 
    return(ans$p.value) else return(ans)
} 