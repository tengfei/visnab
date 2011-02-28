library(GenomicFeatures)
supportedUCSCtables()[1:4,]
rtracklayer:::ucscGenomes()
hg19 <- makeTranscriptDbFromUCSC(genome="hg19",
                                 tablename="knownGene")
saveFeatures(hg19,file="~/Prolang/git/visnab/data/hg19.sqlite")


samplefile <- system.file("extdata","hg19.sqlite",package="visnab")
txdb <- loadFeatures(samplefile)
trps <- transcripts(txdb)
exons <- exons(txdb)
save(trps,exons,file="~/Prolang/git/visnab/data/txdb.rda")

## start demo prototype

visWheelFun <- function(...){
  function(layer, event) {
  zoom_factor <- 2
  if(!inherits(try(event$delta()<0,TRUE),'try-error')){
    if (event$delta() < 0)
      zoom_factor <- 0.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    viewZoomTrack$setTransform(tform)
  }else{
    message('Unsolved Error here..')
  }
}
}


    ir <- IRanges(start=xlimZoom[1],end=xlimZoom[2])
    idm <- findOverlaps(ir,irexon)
    id <- idm@matchMatrix[,2]
    if(length(id)){
    binsexon <- binsexon[id]
    binmx <- max(binsexon,na.rm=TRUE)*10+5
  }



## library(rtracklayer)
## library(IRanges)
## library(GenomicRanges)
library(visnab)
session <- rtracklayer::browserSession('UCSC')
data(txdb)
trps.l <- LinearView(trps,"hg19","transcripts","tx_name")
exons.l <- LinearView(exons,"hg19","transcripts")
test <- visplot(trps.l)
exons.l@trackvisplotDock(visplot(trps.l)$view,visplot(exons.l)$view,size=c(1200,600),ver=TRUE)
sort(ls(test$view))
test$view$viewport()$contentRect

as.matrix(test$view$sceneRect)
trps





library(rtracklayer)
session <- browserSession()
kg <- track(session, "knownGene", "hg19")
save(kg, file="~/Prolang/git/visnab/data/kg.rda")
load(file="~/Prolang/git/visnab/data/kg.rda")
query <- ucscTableQuery(session, "knownGene", "hg19")
tableName(query) <- "knownToLocusLink"
eg <- getTable(query)
save(eg, file="~/Prolang/git/visnab/data/eg.rda")
##save(eg, file="eg.rda")

exons <- blocks(kg)
kg$gene <- eg$value[match(kg$name, eg$name)]
kg <- kg[names(bamrd)]
gene_regions <- split(as(ranges(kg), "GRanges"), kg$gene)

txcounts <- elementLengths(gene_regions)
## for purposes of visualization, need some interesting genes
gene_regions <- gene_regions[txcounts > 2 & txcounts < 7]


