library(visnab)
## library(qtbase)
## library(qtpaint)
## Stacked view
chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
gr <- getIdeogram("hg19",subchr=chrmodel,cytobands=FALSE)
sv <- StackedView(gr)
## Tracks view
sv$pars$seqnameChanged$connect(function(){
  tks$pars$seqname <- sv$pars$seqname
})
tks$show()
sv$show()

