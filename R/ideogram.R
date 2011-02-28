##----------------------------------------------------------------##
##                         ideogram
##----------------------------------------------------------------##
getIdeogram <- function(species=NULL,graphics=FALSE,subchr=NULL){
  if(is.null(species)){
    choices <- ucscGenomes()[,1]
    res <- menu(choices,title="Please specify genome")
    cat("Loading...")
    gr <- GRangesForUCSCGenome(as.character(choices[res]))
    cat("Done")
  }else{
    gr <- GRangesForUCSCGenome(species)
  }
  ## validate chr, keep less chromosomes, more useful for visualization.
  if(!is.null(subchr))
    gr <- validateChr(gr,subchr)
  ## better order them too.
  gr <- sortChr(gr)
}


