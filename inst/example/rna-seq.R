library(Rsamtools)
library(rtracklayer)


gr <- GRangesForUCSCGenome("hg19", "chr1")
## library(BSgenome.Hsapiens.UCSC.hg19)
## gr2 <- GRangesForBSGeome('hg19')

c150sort <- sortBam("~/Data/chip-seq/chip150.bam","~/Data/chip-seq/chip150.sorted" )
c150idx <- indexBam("~/Data/chip-seq/chip150.sorted.bam","~/Data/chip-seq/chip150.sorted.bam" )
c150 <- scanBam("~/Data/chip-seq/chip150.sorted.bam")


r95m <- scanBam("~/Data/rna-seq/rna95.sorted.bam", param=ScanBamParam(which = gr))

r95sort <- sortBam("~/Data/rna-seq/rna95.bam","~/Data/rna-seq/rna95.sorted")
r95indx <- indexBam("~/Data/rna-seq/rna95.sorted.bam","~/Data/rna-seq/rna95.sorted.bam")
r95 <- scanBam("~/Data/rna-seq/rna95.sorted.bam")


## alternative:

## plot things with R

bam <- c150[[1]]

bamrd <- RangedData(IRanges(bam$pos, width=75), strand = bam$strand,
                    mseqname = bam$mrnm, mranges = IRanges(bam$mpos, width=75),
                    isize = bam$isize, space = bam$rname)

summary(bam$isize)
hist(bamrd$isize, breaks = 200, xlim=c(0, 300))
## plot leads to these cutoffs for isize:
bamrd <- subset(bamrd, isize > 170)

