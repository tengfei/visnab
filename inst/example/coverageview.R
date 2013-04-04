## demo start
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqGm12878R1x75dAlignsRep1V2.bam"
library(visnab)
## library(Rsamtools)
## library(MutableRanges)
## library(biovizBase)
## library(qtpaint)
## library(scales)
data("genesymbol", package = "biovizBase")
## started with estiamted coverage
obj <- CoverageView(bamfile)
## all in one GUI, with search ability from genesymbol for human
obj$widget(genesymbol)
## 1. please try to use ctrl + "+" to zoom in , and ctrl + "-" to zoom out and ctrl + "0" to get back
## 2. in search bar, type "chr2" , enter to switch to another chromsome
## 3 try to type a gene

## we defined 4 room levels
## 1. level 1: fast estimated overview for all chromsomes, so after 4 seconds construction time, switch between chromosomes is fast
## 2. level 2: change from segemnt to histogram
## 3  level 3: accurate coverage
## 4. level 4: short reads
obj$ylim



## ### don't have to try
## you can just envoke a single view without GUI
## obj$show()
## range(obj) <- genesymbol["RBM17"]
## range(obj) <- "chr11"

## ## command line works
## obj$setDragMode("NoDrag")
## obj$setDragMode("ScrollHandDrag")
## obj$setDragMode("RubberBand")           #no meaning here yet

