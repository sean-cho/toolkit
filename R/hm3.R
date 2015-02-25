myheatmap3 <- function(x,col=colorRampPalette(c("blue","black","yellow"))(50),...){
  heatmap.3(as.matrix(x),density.info="none",
            col=col,
            trace="none",...)
}

unsupervised.3 <- function(x, n, 
                           col = colorRampPalette(c("blue","black","yellow"))(50), 
                           ...){
  dumsd <- apply(x, 1, sd, na.rm=TRUE)
  dumrows <- dim(x)[1]
  dum <- x[dumsd>=quantile(dumsd, (dumrows-n)/dumrows, na.rm=TRUE),]
  myheatmap3(dum, main=paste("Top", round(n), "Probes", sep = " "), 
             col = col, ...)
}