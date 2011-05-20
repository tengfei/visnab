library(visnab)
library(qtbase)
library(qtpaint)
library(plumbr)
library(MutableRanges)
library(GenomicFeatures)
options(warn=0)
## hg19kg <- makeTranscriptDbFromUCSC(genome="hg19",tablename="knownGene")
## saveFeatures(hg19kg,file="~/Datas/transDb/hg19kg.sqlite")
txdb <- loadFeatures(file="~/Datas/transDb/hg19kg.sqlite")
## this takes about 40+ seconds, because functions like intronsBy* is slow
system.time(obj <- TxdbView(txdb))
## validation
obj$show()
geom(obj)
geom(obj) <- "dense"
geom(obj) <- "full"
aes(obj)
## seqnames(obj) <- "chr2"
range(obj)
range(obj) <- c(7e7,7e7+1000)
range(obj) <- IRanges(7e7,7e7+1000000)
range(obj) <- GRanges(seqnames="chr3", IRanges(500000, 600000))
## should return error message
range(obj) <- GRanges(seqnames="3", IRanges(500000, 600000))
range(obj) <- MutableRanges::MutableGRanges("chr4",IRanges(7e7,7e7+1000000))
## should return error message
range(obj) <- MutableRanges::MutableGRanges("4",IRanges(7e7,7e7+1000000))
range(obj) <- "chr5"
## selectedRangesModel
chrmodel <- paste("chr",c(1:22,"X","Y"), sep="")
test.gr <- GRanges(seqnames=sample(chrmodel,1000,replace=TRUE),
                   ranges=IRanges(start=as.integer(runif(1000,1,2e08)),
                     width=as.integer(rnorm(1000,1e6,100))))
elementMetadata(test.gr) <- data.frame(value=rnorm(1000),
                                       groups=factor(sample(1:9,1000,replace=TRUE)))

selectedRangesModel(obj) <- test.gr
range(obj) <- "chr2"
obj+selectedRangesModel(data = test.gr, color = "groups")

## color change

## "+.RangesModel"
