options(warn=2)
library(visnab)
library(BSgenome.Hsapiens.UCSC.hg19)
##session <- rtracklayer::browserSession('UCSC')
test <- GenomeRefView(Hsapiens)
refs <- visplot(test,"chr1",start=100000,end=100200)
## refs$view$show()
 data(txdb)
trps.l <- LinearView(trps,"hg19","transcripts","tx_name")
 exons.l <- LinearView(exons,"hg19","transcripts")
trpv <- visplot(trps.l,start=0,end=2000000)
## exonv <- visplot(exons.l,start=1000000,end=1000000+40000,show=FALSE)
st <- 100000L
sp <- 110000L
## na19240url <- "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/pilot_data/data/NA19240/alignment/NA19240.chrom6.SLX.maq.SRP000032.2009_07.bam"
## library(Rsamtools)
## which <- RangesList(`6` = IRanges(st,sp))
## ## param <- ScanBamParam(which = which)
## ## na19240bam <- scanBam(na19240url, param = param)
## bamtest <- ShortReadView(speciees="hg19", which=which,file=na19240url)
## names(bamtest) <- "chr6:100000-110000"
## object.size(bamtest)



## visplotDock(transcripts=trpv$view,
## visplotDock(transcripts=trpv$view,
##             exons=exonv$view,
##             reference=refs$view)
## refs$view$show()
## save(refs,file="~/Prolang/git/visnab/data/refs.rda")
## refs$view$show()
## short read
##library(Rsamtools)
## head(bamtest@track[[1]]$pos,30)
## which <- RangesList(seq1=IRanges(1000, 2000))
## bamFile <- system.file("extdata", "ex1.bam", package="Rsamtools")
## bamtest <- ShortReadView(speciees="hg19", which=which,file=bamFile)
## save(bamtest,file="~/Prolang/git/visnab/data/bamtest.rda")
## bamv <- visplot(bamtest,cutbin=30)
## Annotation

data(txdb)
trps.l <- LinearView(trps,"hg19","transcripts","tx_name")
exons.l <- LinearView(exons,"hg19","transcripts")
##visplot(trps.l,start=1000000,end=1000000+40000)
library(GenomicRanges)
library(GenomicFeatures)

## bamFile <- system.file("extdata", "ex1.bam", package="Rsamtools")

## which <- RangesList(chr12=ranges(getIdeogram('hg19'))[12])
## ##bamFile <- system.file("extdata", "ex1.bam", package="Rsamtools")
## bamFile <- "~/Data/chip-seq/chip150.sorted.bam"
## ##bamFile <- "~/Data/rna-seq/rna95.sorted.bam"
## bamtest <- ShortReadView(speciees="hg19", which=which,file=bamFile)
##debugonce(shortReadViewLayer)
##bamonline <- bamtest
data(bamonline)
grands <- GrandLinearView(list(test,exons.l,bamonline))
##save(grands,file="~/Prolang/git/visnab/data/grands.rda")
visplot(grands,seqname="chr6",start=st,end=sp)
##visplotDock(grands,seqname="chr1",start=st,end=sp,
            viewNames=c("Reference","Transcripts","RNA-seq"))
## args(qlayer)



gr <- GRanges(seqname="chr1",
              IRanges(start=start(bamtest@track[[1]]$seq@ranges),
                      width=width(bamtest@track[[1]]$seq@ranges)))

covv <- coverage(gr)
covv

## let's read another bam file

library(qtpaint)

fill_painter <- function(color) {
  function(item, painter) qdrawRect(painter, 0, 0, 1, 1, fill = color)
}

layout_layer <- function(Color, ...) {
  qlayer(figLayer, fill_painter(Color), limits = qrect(0, 0, 1, 1), ...)
}

scene <- qscene()
figLayer <- qlayer(scene)
layout <- figLayer$gridLayout()

titleLayer <- layout_layer("red", colSpan = 2)
yaxisLayer <- layout_layer("blue", row = 1)
plotLayer <- layout_layer("green", row = 1, col = 1)
xaxisLayer <- layout_layer("yellow", row = 2, col = 1)

## Set the maximum widths/heights:

layout$setRowMaximumHeight(0, 50)
layout$setColumnMaximumWidth(0, 50)
layout$setRowMaximumHeight(2, 50)

## Could use Preferred here, with stretch factor set to zero, but this
## breaks when a layer spans multiple cells. It is hard to say if this
## is a bug in Qt, since QGraphicsGridLayout is largely
## undocumented. QGridLayout says a stretch-zero row/column can grow
## when no other row/column can grow. Setting a fixed or minimum
## width/height probably sets undue restrictions on the layout. The
## user should be able to shrink the window down, even if it is not
## longer possible to draw the data effectively. Thus, it's probably
## best to set the maximum.

## It is probably cleaner to set the dimensions directly on the layer:
## titleLayer$setMaximumHeight(50)
## xaxisLayer$setMaximumHeight(50)
## yaxisLayer$setMaximumWidth(50)
## But this currently crashes due to a (reported) bug in Smoke.

qplotView(scene)




visplot(bamtest,seqname=NULL,start=180000000,end=192500000)
visplot(bamtest,seqname=NULL)
save(bamtest,file="~/Prolang/git/visnab/data/bamchr1.rda")
## shortrad
summaryFunction <- 
    function(seqname, bamFile, ...)
{
    param <- ScanBamParam(what=c('pos', 'qwidth'),
                          which=GRanges(seqname, IRanges(1, 1e7)),
                          flag=scanBamFlag(isUnmappedQuery=FALSE))
    x <- scanBam(bamFile, ..., param=param)[[1]]
    coverage(IRanges(x[["pos"]], width=x[["qwidth"]]))
}

s <- qscene()
l <- qlayer(s,function(layer,painter){
  
  qdrawRect(painter,(1:5)*10,5,(1:5)*10+10,10,fill=c("red","red","blue","green","yellow"),stroke=NULL)
  qdrawText(painter,letters[1:5],(1:5)*10,12,color=c("red","red","blue","green","yellow"))
  
},limits=qrect(0,0,200,20))
v <- qplotView(s)
v$show()




## let's paste a demo
## try to create a linear view?
## offer some general control
qlayer

library(qtpaint)
s <- qscene()
pfun1 <- function(layer,painter){
  qdrawCircle(painter,50,50,10,fill="red")
}
pfun2 <- function(layer,painter){
  qdrawRect(painter,10,10,70,70)
}
l <- qlayer(s)
l1 <- qlayer(l,pfun1)
l2 <- qlayer(l,pfun2)

  sroot <- qscene()
  lroot <- qlayer(sroot)
lroot$addLayer(l1,0)

v1 <- qplotView(s)$show()
v2 <- qplotView(s)$show()

combineTracks <- function(...){
  arg <- list(...)
  sroot <- qscene()
  lroot <- qlayer(sroot)
  lapply(1:length(arg),function(i){
    lroot$addLayer(arg[[i]],row=i-1)
  })
  v <- qplotView(sroot)
  v$show()
}

combineTracks(l1,l2)

## grand linear

library(GenomicFeatures)
bamRanges <- local({
    fl <- system.file("extdata", "CaffeineTxdb.sqlite", 
                      package="Rsamtools")
    transcripts(loadFeatures(fl))
})
bamRanges
slxMaq09 <- local({
    fl <- system.file("extdata", "slxMaq09_urls.txt", 
                      package="Rsamtools")
    readLines(fl)
})

slxMaq09

s <- qscene()
l <- qlayer(s,function(layer,painter){
  qdrawRect(painter,50,50,100,100)
  qdrawRect(painter,100,100,200,200,fill="red")
},limits=qrect(0,0,200,200))
v <- qplotView(s)
v$show()

v2 <- qplotView(s,rescale="none")
args(qplotView)
sort(ls(v2))
v2$setSceneRect(0,0,600,100)
v2$show()
as.matrix(s$sceneRect)
