library(gWidgets)
library(qtbase)
library(qtpaint)
library(plumbr)
library(reshape)
library(Biobase)
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/R')
library(GenomicFeatures)
load('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/covtest.rda')

gInitialize()

gGUI()

## save(hotRegion,r11,r22,
##      chromRangeDf,cytobands,r1,r2,spe.lst,
##      txobj,objExonLst,objTxLst,mysum9,
##      file='~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/covtest3.rda')

##showWin()


library(chipseq)
data(cstest)
str(cstest)

ranges(cstest[[1]])
