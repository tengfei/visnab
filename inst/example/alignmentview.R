## Alignment
library(visnab)
library(Rsamtools)
library(rtracklayer)
library(qtbase)
library(qtpaint)
setwd('~/Datas/seqs/rna-seq')
## gr <- GRangesForUCSCGenome("hg19")
chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
gr <- getIdeogram("hg19",subchr=chrmodel,cytobands=FALSE)

bam <- scanBam("rna95.sorted.bam", param=ScanBamParam(which = gr2))
bam <- scanBam(file,param=ScanBamParam(which=gr[1]))
bam[[1]]

save(bam,file="~/Datas/rdas/bam.rda")
load("~/Datas/rdas/bam.rda")
obj <- AlignmentView(bam,title="Alignment")
obj$show()

## gr <- GRangesForUCSCGenome("hg19",'chr1')
gr <- getIdeogram("hg19",cyto=FALSE)
file <- "~/Datas/seqs/rna-seq/rna95.sorted.bam"
obj <- AlignmentView(file,gr)
obj$pars$seqname <- "chr2"


bam <- scanBam(file, param=ScanBamParam(which = gr[1]))
str(bam[[1]])
## save(bam,file="~/Datas/rdas/bam.rda")
load("~/Datas/rdas/bam.rda")
rm(bam)
obj <- AlignmentView(bam,title="align")
obj$pars$seqname
print(obj)


## for pileup
library(ShortRead)

fls <- "./rna95.sorted.bam"


pileup <- function(reads, bases = DNA_BASES) {
  seqs <- sread(reads)
  rc <- strand(reads) == "-"
  seqs[rc] <- reverseComplement(seqs[rc])
  counts <- mclapply(bases, function(base) {
    mindex <- vmatchPattern(base, seqs)
    counts <- tabulate(unlist(startIndex(mindex), use.names=FALSE) + 
                       rep(position(reads)-1L, countIndex(mindex)))
  })
  names(counts) <- bases
  maxpos <- max(elementLengths(counts))
  counts <- lapply(counts, function(x) c(x, rep(0L, maxpos - length(x))))
  counts <- do.call(rbind, counts)
  colnames(counts) <- as.character(seq(ncol(counts)))
  counts
}

