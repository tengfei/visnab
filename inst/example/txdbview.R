library(visnab)
library(qtbase)
library(qtpaint)
library(GenomicFeatures)
options(warn=2)
## hg19kg <- makeTranscriptDbFromUCSC(genome="hg19",tablename="knownGene")
## saveFeatures(hg19kg,file="~/Datas/transDb/hg19kg.sqlite")
txdb <- loadFeatures(file="~/Datas/transDb/hg19kg.sqlite")
obj <- TxdbView(txdb,geom="full",seqname="chr1")
obj$show()
sort(ls(obj$rootLayer))
obj$rootLayer$close()

obj$show()
geom(obj)
geom(obj) <- "dense"
geom(obj) <- "full"
obj$createView(seqname="chr1")
seqnames(obj) <- "chr5"

## demo used for this case
samplefile <- system.file("extdata", "UCSC_knownGene_sample.sqlite",
                          package="GenomicFeatures")
txdb <- loadFeatures(samplefile)

obj <- TxdbView(txdb,geom="full")
obj$show()

obj$createView(geom="dense")
geom(obj) <- "dense"

