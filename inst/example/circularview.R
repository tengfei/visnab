## Example
require(visnab)
gr2 <- MutableGRanges(seqnames=Rle(c('Chrom1', 'Chrom2', 'Chrom3'),
                                     c(3, 3, 4)),
                        IRanges(1:10, width=5), strand='-',
                        score=101:110, GC = runif(10))

gr3 <- GRanges(seqnames=Rle(c('Chrom1', 'Chrom2', 'Chrom3'),
                                     c(3, 3, 4)),
                        IRanges(1:10, width=5), strand='-',
                        score=101:110, GC = runif(10))

setAs("GRanges","MutableGRanges",function(from) callNextMethod())


obj <- CircularView(list(gr3),model=gr3,tracksType="sector")
print(obj)

ls(obj@pars)
obj@pars$tracksColorThemeBackup
