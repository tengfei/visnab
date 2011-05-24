library(visnab)
library(Rsamtools)
library(MutableRanges)
library(qtbase)
library(qtpaint)
library(plumbr)

file <- "~/Datas/seqs/rna-seq/rna95.sorted.bam"
obj <- CoverageView(file)
obj$show()
range(obj) <- c(35655585, 35659900)
range(obj) <- "chr3"
range(obj) <- GRanges(seqnames="chr2", IRanges(35655585, 35659900))

## prepare a hints data
hd <- scanBamHeader(file)
seqname <- sort(names(hd[[1]]$targets))[1]
data(hg19Ideogram)
lst <- lapply(1:length(hg19Ideogram),function(i){
  gr <- hg19Ideogram[i]
  print(system.time(bam <- scanBam(file, param = ScanBamParam(which = gr,
                         what = c("pos", "qwidth")))))
  bam <- bam[[1]]
  ir <- GRanges(seqnames=seqname,
                ranges=IRanges(start=bam$pos, width=bam$qwidth))
  covg <- coverage(ir)
})

covlst <- lst
names(covlst) <- seqnames(hg19Ideogram)
save(covlst, file= "~/Datas/rdas/covlst.rda")

