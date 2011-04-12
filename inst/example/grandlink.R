library(visnab)
library(qtbase)
library(qtpaint)
## Stacked view
chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
gr <- getIdeogram("hg19",subchr=chrmodel,cytobands=FALSE)
sv <- StackedView(gr)
## Tracks view
load("~/Datas/rdas/cds.rda")
cdsi <- IntervalView(cds)
cyto <- getIdeogram("hg19",subchr=chrmodel,cytobands=TRUE)
tks <- TracksView(cdsi,ideogram=cyto)
tks$show()
sv$show()



sv$pars$seqnameChanged$connect(function(){
  tks$pars$seqname <- sv$pars$seqname
})
