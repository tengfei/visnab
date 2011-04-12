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
library(visnab)
gr <- getIdeogram("hg18")
data(kgsub)
int1 <- IntervalView(kg.chr1.sub,title="exons")
int2 <- IntervalView(kg.chr1.sub,title="Pseudo-exons")
load("~/Datas/rdas/cds.rda")
cdsi <- IntervalView(cds)
cdsi$show()
cdsi$pars$seqname <- "chr2"
obj.ref <- SeqView(gr)
load("~/Datas/rdas/bam.rda")
obj.ali <- AlignmentView(bam)
tks <- TracksView(obj.ref,int1,int2,obj.ali,ideogram=gr)
tks <- TracksView(cdsi,ideogram=gr)
tks$show()
tks$view$show()

tks$pars$seqname <- "chr4"

tks$createView()
tks$show()

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



##
library(qtbase)
library(qtpaint)

scene <- qscene()
layer <- qlayer(scene,function(layer,painter){
  qdrawCircle(painter,rnorm(100,50,20),rnorm(100,50,50),5,fill="blue")
},keyPressFun=function(layer,event){
  if(event$modifiers() == Qt$Qt$ControlModifier&&
     event$key() == Qt$Qt$Key_Equal)
    view$scale(2,1)
  if(event$modifiers() == Qt$Qt$ControlModifier&&
     event$key() == Qt$Qt$Key_Minus)
   view$scale(1/2,1)
  if(event$modifiers() == Qt$Qt$ControlModifier&&
     event$key() == Qt$Qt$Key_0)
   view$resetTransform()
},limits=qrect(0,0,100,100))
view <- qplotView(scene)
view$show()
args(qtpaint::qlayer)

trackWidget <- Qt$QWidget()
trackLayout <- Qt$QGridLayout()
trackWidget$setLayout(trackLayout)
trackWidget$layout()
trackWidget$layout() <- NULL
sort(ls(trackWidget$QGridLayout()))
