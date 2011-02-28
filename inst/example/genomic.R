test <- read.table('~/Downloads/repeat.txt',header=FALSE)
test2 <- test[,c(5,2,3,4,7,8:10,27)]
colnames(test2) <- c('name','chr','start','end','strand','to.chr','other.start','other.end','fracMatch')
head(test2)
test2[test2$start==76280701,]
test2 <- subset(test2,chr%in%chr.model & to.chr%in%chr.model)
head(test2)
## write.csv(test2,file='~/Prolang/git/VisNAB/data/similarity.csv',row.names=FALSE)
sim <- read.csv(file='~/Prolang/git/VisNAB/data/similarity.csv',header=TRUE)
library(GenomicRanges)
sim.gr <- GRanges(seqnames=sim$chr,
                  ranges=IRanges(start=sim$start,
                    end=sim$end),
                  strand=sim$strand)
elementMetadata(sim.gr) <- sim[,5:8]






mut <- read.table('~/Prolang/git/cranvas/Tengfei/eos/data/mutation.mut',sep='\t',header=TRUE)
scores <- read.table('~/Prolang/git/cranvas/Tengfei/eos/data/scores.txt',header=TRUE,sep='\t')
seg <- read.table('~/Prolang/git/cranvas/Tengfei/eos/data/segment.seg',header=TRUE,sep='\t')
##gct <- read.table('~/Prolang/git/cranvas/Tengfei/eos/data/gbm.gct',header=TRUE,sep='\t',skip=2,quote="")

## gct.gr <- map2granges(gct)
library(GenomicRanges)
library(IRanges)
mut.gr <- GRanges(seqnames=mut$chr,ranges=IRanges(start=mut$start,end=mut$end),strand='*')
elementMetadata(mut.gr) <- mut[,-c(1:3)]
##elementMetadata(mut.gr) <- mut[,'gene']

sc.gr <- GRanges(seqnames=as.character(scores$Chromosome),ranges=IRanges(start=scores$Start,end=scores$End),strand='*')
elementMetadata(sc.gr) <- scores[,-c(2:4)]
seg.gr <- GRanges(seqnames=as.character(seg$Chromosome),ranges=IRanges(start=seg$Start.Position,end=seg$End.Position),strand='*')
elementMetadata(seg.gr) <- seg[,-c(2:4)]



load('~/Prolang/git/cranvas/Tengfei/eos/data/cytobands.rda')
obj <- cytobands[['Homo sapiens']]
md.gr <- GRanges(seqnames=obj$chr,
                 ranges=IRanges(start=obj$start,end=obj$end),
                 strand='*')
chr.model <- unique(as.character(seqnames(md.gr)))

elementMetadata(md.gr) <- obj[,4:6]

library(GenomicFeatures)
hg19 <- loadFeatures('~/Prolang/git/cranvas/Tengfei/eos/data/hg19.sqlite')
trps <- transcripts(hg19)
exs <- exons(hg19)
cdss <- cds(hg19)

sourceDir('~/Prolang/git/cranvas/Tengfei/eos/R')

sc.gr <- addPrefix(sc.gr,'chr')
seg.gr <- addPrefix(seg.gr,'chr')
snpGr


trps <- validateChr(trps,md.gr)
exs <- validateChr(exs,md.gr)
cdss <- validateChr(cdss,md.gr)



grandLst <- list(transcripts=trps,
                 exons=exs,
                 cds=cdss,
                 mutation=mut.gr,
                                        #                        cpn=seg.gr,
                 score=sc.gr,
                 model=md.gr,
                 link=sim.gr)
names(grandLst)
trps <- as.data.frame(grandLst[[1]])
idx <- runif(1000,1,73660)
trps <- trps[idx,]
trps <- trps[,-6]
head(trps)
args(runif)
head(trps)
dim(trps)
muts <- as.data.frame(grandLst[[4]])
library(plyr)
muts2 <- ddply(muts,.(seqnames,start,end,width),summarise,
               score=length(start))

mddf <- as.data.frame(reduce(grandLst[[6]]))
sim <- grandLst[[7]]
idx <- order(values(sim)$fracMatch,decreasing=TRUE)[1:500]
sim <- sim[idx]
simdf <- as.data.frame(sim)
head(simdf)
grandLst[[7]] <- sim

mddf
muts
muts2
dim(muts)
head(muts2)
unique(muts$patient)

grandLst <- list(transcripts=trps,
                 mutation=muts2,
                 model=mddf,
                 link=simdf)
head(grandLst[[4]])
names(grandLst)
save(grandLst,md.gr,file='~/Prolang/git/cranvas/Tengfei/eos/data/grandLst.rda')
save(grandLst,md.gr,file='~/Prolang/git/cranvas/Tengfei/eos/data/grandDfLst.rda')



sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
library(qtpaint)
sourceDir('~/Prolang/git/VisNAB/R')
library(GenomicRanges)
sourceDir('~/Prolang/git/VisNAB/R')
load('~/Prolang/git/VisNAB/data/grandLst.rda')
names(grandLst)

sim <- grandLst[[7]]
idx <- order(values(sim)$fracMatch,decreasing=TRUE)[1:500]
sim <- sim[idx]
grandLst[[7]] <- sim



isValidatedChr(grandLst,md.gr)
md.gr.s <- reduce(md.gr)
grandLst[[6]] <- md.gr.s
## make sure it is ordered 
## md.gr.s <- md.gr.s[orderChr(seqnames(md.gr.s))]
names(grandLst)


testeos3 <- CircularView(grandLst[c(6,1,5,7)],
                      model=md.gr.s,
                      tracksType=c('sector','segment','point','linkLine'),
                      tracksOrder=c(0.1,1,0.5,0.05),
                      isPlotted=list(NA,NA,3,NA),
                      tracksColorTheme=c('gray90','blue','red','blue'))



testeos3@pars$tracksColorTheme[2] <- 'default'
testeos@pars$tracksColorTheme[1] <- 'black'
testeos@pars$tracksColorTheme[5] <- 'pink'
testeos3@pars$tracksColorTheme[5] <- 'default'
testeos3@pars$tracksColorTheme[1] <- 'purple'
## testeos@pars$tracksType[1] <- 'segment'

visnab <- visplot(testeos3)
traceback()
qupdate(visnab$scene)

length(testeos@tracks[[5]])

str(testeos@tracks)
testeos@tracks[[3]]

testeos <- CircularView(grandLst[c(6,1,2)],
                     md.gr.s,
                     tracksType=c('sector','segment','segment'),
                     tracksOrder=c(0,1,2))

testeos <- CircularView(grandLst[c(6,2)],
                     md.gr.s,
                     tracksType=c('sector','segment'),
                     tracksOrder=c(0,1))
testeos@tracksType[2] <- 'line'



## for james
library(visnab)
clength <- c(161108518,140689454,127865071,124429929,125642737,122646612,111948904,
             116941315,108100207,106310658,110261590,85442768,84433115,81409064,84800091,
             76519030,65948816,65317834,75862687,69307001,61892535,53331164,65017658,44044338,
             51861200,46105694,52131757,77906053,48749334,88516663)

bos <- GRanges(seqnames=paste('chr',c(1:15,17:26,28:29,16,27,30),sep=''),
               ranges=IRanges(start=0,end=clength))
## snpbos <- read.csv("~/Prolang/R/james/epistat.csv",header=TRUE)
snpbos <- read.csv("~/Data/james/newExample.csv",header=TRUE)
colnames(snpbos)[1:4] <- c('chr','start','to.chr','other.start')
snpbos$end <- snpbos$start
snpbos$other.end <- snpbos$other.start
snpGr <- GRanges(seqnames=snpbos$chr,
                 IRanges(start=snpbos$start,
                         end=snpbos$end))
values(snpGr) <- snpbos[,c(-1,-2,-8)]
snpGr <- addPrefix(snpGr,'chr')
## replace
snpGr <- replaceChr(snpGr,"chr30","chrX")
bos <- replaceChr(bos,"chr30","chrX")
save(bos,snpGr,file='~/Prolang/git/visnab/data/bos.rda')

library(visnab)
data(bos)
## A) adding the genome coordinates to the chromosomes in 5 or 10 Mbps
## segments,(done)
## B) plotting the data for p< -9, then plotting the data
## for p<10-8, p<10-7, each in a separate graph,
idx <- values(snpGr)$P_value<1e-8
snpGr.p9 <- snpGr[idx]
ps <- values(snpGr.p9)$P_value
tst <- values(snpGr.p9)$Test
cols <- cscale(ps,gradient_2_pal(low="red",high="yellow"))
cols <- alpha(cols,0.3)
cols2 <- dscale(tst,brewer_pal(pal='Set2'))
lst <- list(bos=bos,scale=bos,snpGr=snpGr.p9,test=snpGr.p9)
grv.p9 <- CircularView(lst,lst$bos,
                 tracksType=c('sector','scale','link','segment'),
                 tracksOrder=c(1,2,-2,1.5),
                 tracksColorTheme=list('gray90','gray90',cols,cols2),
                       tracksWidth=c(20,30,40,30))
grv8 <- grv.p9
save(grv.p9,file='~/Prolang/git/visnab/data/grv7.rda')
save(grv8,file='~/Prolang/git/visnab/data/grv8.rda')

library(visnab)
## put these as demo
data(grv8)
grd <- visplot(grv8)
visenv$zoomobj <- grd
visZoom(grd)



