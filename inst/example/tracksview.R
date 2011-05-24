## library(Rsamtools)                      
## library(GenomicRanges)
## library(MutableRanges)
## library(rtracklayer)
## library(visnab)
## library(qtbase)
## library(qtpaint)
## library(plumbr)
## library(MutableRanges)
library(visnab)
data(hg19Ideogram)
data(hg19IdeogramCyto)
obj.chrom <- SingleChromView(hg19IdeogramCyto)
obj.scale <- ScaleView(track = hg19Ideogram, rescale = "geometry")
library(BSgenome.Hsapiens.UCSC.hg19)
obj.seq <- SeqView(track = Hsapiens, rescale = "geometry")
library(GenomicFeatures)
txdb <- loadFeatures(file="~/Datas/transDb/hg19kg.sqlite")
obj.txdb <- TxdbView(txdb,rescale = "geometry")
file <- "~/Datas/seqs/rna-seq/rna95.sorted.bam"
obj.cov <- CoverageView(file,rescale = "geometry")
obj.ali <- AlignmentView(file,rescale = "geometry")
test <- TracksView(obj.chrom,obj.scale, obj.seq,
                   obj.txdb,obj.cov, obj.ali)

test$show()
range(obj.ali)
range(test) <- c(113215208, 113215334)
geom(obj.txdb) <- "dense"
geom(obj.txdb) <- "full"

obj.ov <- StackedView(hg19Ideogram)
obj.ov$show()

load(file = "~/Datas/rdas/grs.rda")
obj.ov+selectedRangesModel(grs, color = "red")
obj.ov+selectedRangesModel(grs, color = "groups")
