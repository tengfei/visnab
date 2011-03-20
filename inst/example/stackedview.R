require(GenomicRanges)
require(rtracklayer)
require(visnab)

chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
sv <- StackedView("hg19",subchr=chrmodel)
print(sv)
