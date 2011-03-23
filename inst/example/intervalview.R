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

## For demo we keep a small subset
## Specify what to show in the tooltips,
## the name should be one of the colnames of the value(obj)
## choose name slots to show in tooltip
names(values(kg.chr1.sub))
obj <- IntervalView(kg.chr1.sub,idname='name')
print(obj)

obj <- IntervalView(kg.chr1.sub,idname='blockCount',stroke=NA,fill="red")
print(obj)

## or
kgrm <- as(kg.chr1.sub,"MutableGRanges")
obj <- IntervalView(kgrm,idname='name')
print(obj)
## try show different stuff

obj@pars$fill <- "red"                #doesn't work
values(obj@track)$.color
values(obj@track)$.color <- "black"     #work
obj@pars$bgColor <- "gray80"             #work
obj@pars$hoverColor <- "green"          #work
obj@pars$reset()                   #work
obj

