## Function for counting bases at each position:
## 'reads' is an AlignedRead object
library(GenomicRanges)
library(rtracklayer)
library(Rsamtools)
library(ShortRead)
gr <- GRangesForUCSCGenome("hg19",'chr1')
setwd('~/Datas/Data/rna-seq')
## save(gr,file='gr.rda')
load('gr.rda')
## bam <- scanBam("data/bowtie.output.more.bam", param=ScanBamParam(which = gr))

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
