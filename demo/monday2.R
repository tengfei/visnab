library(Rsamtools)
library(rtracklayer)
library(IRanges)
library(visnab)
visenv$themes[["default"]][["bg.col"]] <- "white"
param <- ScanBamParam(which=which, what=what)
bam <- scanBam(file, param=param)

## ucscGenomes()                           #
gr <- GRangesForUCSCGenome("hg19",'chr1')
setwd('~/Datas/Data/rna-seq')
## save(gr,file='gr.rda')
load('gr.rda')
## bam <- scanBam("data/bowtie.output.more.bam", param=ScanBamParam(which = gr))
fls <- "./rna95.sorted.bam"
newbam <- AlignmentView(file=fls,which=gr)
visplot(newbam,lower=10,cutbin=30)
bam <- scanBam("./rna95.sorted.bam", param=ScanBamParam(which = gr))
bam.c <- countBam("./rna95.sorted.bam", param=ScanBamParam(which = gr))
bam <- bam[[1]]



## gr <- GRangesForUCSCGenome("dm3","chr4")
## ## gr <- GRangesForUCSCGenome("hg19", "chr1")
## setwd('~/Datas/genomeview/d_melanogaster')
## bam <- scanBam("SRR015086.dm3.chr4.sorted.bam", param=ScanBamParam(which = gr))

## alternative:
ucscGenomes()[,1]
## plot things with R
bam <- bam[[1]]

args(RangedData)
sum(!is.na(bam$mpos))

bam$pos[!is.na(bam$mpos)]
bamrd <- RangedData(IRanges(bam$pos, width=45), strand = bam$strand,
#                    mseqname = bam$mrnm,
                     mranges = IRanges(bam$mpos,width=45),
#                    isize = bam$isize,
                    space = bam$rname)
traceback()

hist(bamrd$isize, breaks = 200, xlim=c(0, 300))
## plot leads to these cutoffs for isize:
bamrd <- subset(bamrd, isize > 170)

##library(GenomicFeatures)
##kg <- loadFeatures("~/tracks/hg19/knownGenes.sqlite")
## FIXME: wanted to get tx_id here, but it just sat at 100% CPU for too long
##kge <- exons(kg, columns = c("exon_id", "tx_id"))
## Forget about GenomicFeatures and use rtracklayer:

library(rtracklayer)
session <- browserSession()
sort(trackNames(session))
kg <- track(session, "knownGene", "hg19")
save(kg, file="kg.rda")
## query <- ucscTableQuery(session, "knownGene", "hg19")
## tableName(query) <- "knownToLocusLink"
## eg <- getTable(query)
## save(eg, file="eg.rda")
load("kg.rda")
load("eg.rda")

exons <- blocks(kg)
kg$gene <- eg$value[match(kg$name, eg$name)]
names(kg)
names(bamrd)

kg <- kg[na.omit(match(names(bamrd),names(kg)))]
gene_regions <- split(as(ranges(kg), "GRanges"), kg$gene)
gene_regions

txcounts <- elementLengths(gene_regions)
txcounts
## for purposes of visualization, need some interesting genes
gene_regions <- gene_regions[txcounts > 2 & txcounts < 7]
## leaves us with 7467

## now for RPKM:

source("~/projects/pipelines/RNASeqPipeline/R/functions.R")

rpkmForBounds <- function(reads, bounds)
{
  if (is(reads, "GRangesList") || is(reads, "list"))
    do.call(DataFrame, mclapply(reads, rpkmForBounds, bounds))
  else {
    GeneCounts <- countOverlaps(bounds, reads)
    names(GeneCounts) <- names(bounds)
    rpkm(GeneCounts, bounds)
  }
}

rpkms <- rpkmForBounds(bamrd, gene_regions)
gene_regions <- gene_regions[rpkms > 50]
genetest <- gene_regions[sample(1:8408,100)]
## 6 genes

plotReadsOnGene <- function(gene, reads, exons) {
  ex <- subsetByOverlaps(exons, gene)
  any_ex <- reduce(ex)
  all_gaps <- gaps(any_ex, start(any_ex)[1])
  max_gap <- 0.0025 * width(range(any_ex))
  shrinkage <- pmax(0, width(all_gaps) - max_gap)
  shrinksum <- c(0L, cumsum(shrinkage))
  shrinkageCorrection <- function(x) {
    shift(x, -shrinksum[findInterval(start(x), end(all_gaps)) + 1L])
  }
  ex <- shrinkageCorrection(ex)
  extx <- split(ranges(ex), values(ex)$tx)
  introns <- stack(gaps(extx, unlist(start(range(extx)))), "tx", "ranges")
  
  values(ex)$y <- as.integer(factor(values(ex)$tx)) # each transcript by itself
  ex <- ex[,"y"]

  ## convert to RangesList here to use type = "within"
  rds <- subsetByOverlaps(reads, as(any_ex, "RangesList"), type = "within")
  rds <- rds[!is.na(findOverlaps(rds$mranges,
                                 ranges(any_ex), type = "within",
                                select = "arbitrary")),]

  pe1 <- shrinkageCorrection(as(ranges(rds), "GRanges"))
  pe2 <- shrinkageCorrection(GRanges(rds$mseqname, rds$mranges))
  rdends <- c(pe1, pe2)
  rdspans <- ranges(punion(pe1, pe2, fill.gap = TRUE))
  rdspans <- rdspans + 0.001 * width(range(any_ex))
  values(rdends)$y <- -disjointBins(rdspans)

  ranges <- c(rdends, ex)
  values(ranges)$feature <- rep(c("reads", "transcripts"),
                                c(length(rdends), length(ex)))

  rdgaps <- pgap(ranges(pe1), ranges(pe2))
  
  gps <- stack(RangesList(reads = rdgaps, transcripts = introns$ranges),
               "feature_gaps", "ranges")
  ##gps$ranges <- shrinkageCorrection(gps$ranges)
  gps$y <- gps$yend <- c(head(values(ranges)$y, nrow(rds)),
                         as.integer(as.factor(introns$tx)))
  
  p <- ggplot(as.data.frame(ranges))
  p <- p + geom_rect(aes(xmin = start, xmax = end, ymin = y - 0.4,
                         ymax = y + 0.4, fill = feature))
  p <- p + geom_segment(aes(x = ranges.start, xend = ranges.end, y = y,
                            yend = yend, colour = feature_gaps),
                   data = as.data.frame(gps))
  p + xlab("position") + ylab("y (artificial)")
}

plots <- lapply(gene_regions, plotReadsOnGene, bamrd, exons)

library(org.Hs.eg.db)

syms <- mget(names(plots), org.Hs.egSYMBOL)

for (eg in names(plots)) {
  p <- plots[[eg]] +
    opts(title = paste("Gene: ", eg, " (", syms[[eg]], ")", sep = ""))
  ggsave(paste("fig/reads-on-gene-", eg, ".pdf", sep=""), p)
}

## Old code for "fixing" gsnap's SAM output
if (FALSE) {
  ## Fix this SAM file

  sam <- read.table("sam_part_10.sam", sep = "\t", comment.char = "",
                    quote = "", skip = 24, stringsAsFactors=FALSE, fill=TRUE)
  sam <- sam[!is.na(sam$V2),]

  sam <- sam[order(sam$V4),]
  sam$V3 <- ifelse(sam$V3 == "*", "*", paste("chr", sam$V3, sep = ""))

  samFile <- file("sam_part_10_chr.sam", "w+")
  writeLines(sub("SN:", "SN:chr", readLines("sam_part_10.sam", 24)), samFile)
  write.table(sam, samFile, sep = "\t", quote = FALSE, col.names=FALSE,
              row.names=FALSE)
  close(samFile)
}
