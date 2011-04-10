library(Rsamtools)                      #
library(rtracklayer)
library(visnab)
library(qtbase)
library(qtpaint)

## ## ucscGenomes()                           #
## gr <- GRangesForUCSCGenome("hg19",'chr1')
## setwd('~/Datas/Data/rna-seq/rna95')
## save(gr,file='gr.rda')
## load('~/Datas/rda/gr.rda')
## fls <- "./rna95.sorted.bam"
## newbam <- AlignmentView(file=fls,which=gr,lower=10,cutbin=30)
## visplot(newbam,lower=10,cutbin=30)

library(visnab)
session <- browserSession()
cds <- track(session, "ccdsGene", "hg19",asRangedData=FALSE)
obj <- IntervalView(cds)
print(obj)

## save(cds,file="~/Datas/rdas/cds.rda")
load("~/Datas/rdas/cds.rda")

## kg.chr1 <- kg[seqnames(kg)=="chr1"]

## owd <- getwd()
## setwd("~/Datas/rda/")
## load('hg19kg.rda')
## load('hg19rna.rda')
## setwd(owd)
## require(BSgenome.Hsapiens.UCSC.hg19)
## test <- SeqView(Hsapiens)
library(qtbase)
library(qtpaint)
class(Qt$QWidget())
library(visnab)
gr <- getIdeogram("hg18")
data(kgsub)
int1 <- IntervalView(kg.chr1.sub,title="exons")
int2 <- IntervalView(kg.chr1.sub,title="Pseudo-exons")
obj.ref <- SeqView(gr)
load("~/Datas/rdas/bam.rda")
obj.ali <- AlignmentView(bam)
tks <- TracksView(obj.ref,int1,int2,obj.ali,ideogram=gr)
tks$show()
tks$view$show()

debug(getIdeogram)

gr <- getIdeogram("hg18")

## save(obj.ref,file="~/Datas/rdas/ref.rda")
load("~/Datas/rdas/ref.rda")
tks <- TracksView(obj.ref,int2,int1,obj.ali)
sort(ls(tks$view))
sort(ls(tks$view$viewport()))
as.matrix(tks$view$viewport()$contentsRect())
tks$view$mapToScene()
as.matrix(tks$view$sceneRect)
grep("scroll",sort(ls(tks$view$viewport())),value=TRUE)




## int3 <- IntervalView(cds,fill="black",title="Cds")

print(tks,rowHeights=c(50,50,50,50))

library(qtbase)
library(qtpaint)

## plot cytoband
library(visnab)
gr <- getIdeogram("hg19")
obj <- TracksView(int1,ideogram=gr)
print(obj)
obj@viewrange


args(qplotView)

## The Gpos100 class consists of the darkest staining bands, with
## the Gpos75, Gpos50 and Gpos25 classes containing progressively lighter
## staining G-positive bands. The Gneg class consists of the non-staining
## G-negative light bands.


scene <- qscene()
layer <- qlayer(scene,function(layer,painter){
  xrect <- seq(from=0,to=600,by=20)
  col=rep(c("red","green"),600/20/2)
  qdrawRect(painter,xrect,5,xrect+19,15,fill=col,stroke=NULL)
},limits=qrect(0,0,600,20),
                geometry=qrect(0,0,100,100))
qplotView(scene)$show()

library(qtpaint)
