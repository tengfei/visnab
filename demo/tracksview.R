library(Rsamtools)
library(rtracklayer)
library(visnab)
visenv$themes[["default"]][["bg.col"]] <- "white"

## ## ucscGenomes()                           #
gr <- GRangesForUCSCGenome("hg19",'chr1')
setwd('~/Datas/Data/rna-seq/rna95')
## save(gr,file='gr.rda')
## load('gr.rda')
fls <- "./rna95.sorted.bam"
newbam <- ShortReadView(file=fls,which=gr,lower=10,cutbin=30)
## visplot(newbam,lower=10,cutbin=30)

owd <- getwd()
setwd("~/Datas/rda/")
load('hg19kg.rda')
load('hg19rna.rda')
setwd(owd)
require(BSgenome.Hsapiens.UCSC.hg19)
test <- GenomeRefView(Hsapiens)
tks <- TracksView(hg19kg,hg19rna,test,newbam)
visplot(tks,seqname="chr1")

