require(rtracklayer)
require(visnab)

session <- browserSession()
kg <- track(session, "knownGene", "hg19",asRangedData=FALSE)
kg.chr1 <- kg[seqnames(kg)=="chr1"]
vals <- values(kg.chr1)
vals.new <- vals[,c(1,6)]
values(kg.chr1) <- vals.new
kg.chr1.sub <- kg.chr1[sample(1:length(kg.chr1),1000)]
save(kg.chr1.sub,file="../data/kgsub.rda")

data(kgsub)


names(values(kg.chr1.sub))
obj <- IntervalView(kg.chr1.sub)
print(obj)

obj <- IntervalView(kg.chr1.sub,stroke=NA,fill="red")
print(obj)

## or
kgrm <- as(kg.chr1.sub,"MutableGRanges")
obj <- IntervalView(kgrm)
print(obj)
## try show different stuff

obj@pars$fill <- "red"                #doesn't work
values(obj@track)$.color
values(obj@track)$.color <- "red"     #work
obj@pars$bgColor <- "gray80"             #work
obj@pars$hoverColor <- "green"          #work
obj@pars$reset()                   #work
obj

library(visnab)
library(qtpaint)
library(qtbase)
load("~/Datas/rdas/cds.rda")
obj <- IntervalView(cds,title="cds")
obj$show()
print(obj)

obj <- IntervalView.gen$new()

addDefAttr


## MutableRanges
library(MutableRanges)
obj <- MutableGRanges(seqnames="chr1",IRanges(start=1:3,width=10))
obj$locationsChanged$connect(function(){
  print(.self$seqnames)
  print(i)
  print(j)
})

start(obj)[1] <- 2

library(visnab)
library(qtbase)
library(qtpaint)
load("~/Datas/rdas/cds.rda")
obj <- IntervalView(cds)
print(obj)
sort(ls(obj$rootLayer$gridLayout()))
sort(ls(obj$rootLayer$layout()))
sort(ls(obj$rootLayer$gridLayout()))
ls(obj$rootLayer$gridLayout()$itemAt(0,0))

obj$pars$seqname <- "chr5"
