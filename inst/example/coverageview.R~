bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqGm12878R1x75dAlignsRep1V2.bam"
library(visnab)
## library(devtools)
## load_all("~/Codes/gitrepos/visnab")
data("genesymbol", package = "biovizBase")

obj <- CoverageView(bamfile)

## all in one GUI, with search ability from genesymbol for human
obj$widget(genesymbol)
## please try to use ctrl + "+" to zoom in , and ctrl + "-" to zoom out and ctrl + "0" to get back
## you could try to type a gene
## you can just envoke a single view without GUI
obj$show()
range(obj) <- genesymbol["RBM17"]
range(obj) <- "chr11"

obj$setDragMode("NoDrag")
obj$setDragMode("ScrollHandDrag")
obj$setDragMode("RubberBand")           #no meaning here yet

obj$pars$zoomLevel <- c(1e8, 1e7, 0)
