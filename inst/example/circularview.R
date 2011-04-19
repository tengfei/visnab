## Example
require(visnab)
library(qtbase)
library(qtpaint)

## scene <- qscene()
## N <- 10000
## paths <- lapply(1:N,function(i){
##   qglyphQuadCurve(c(rnorm(1,-10,4),rnorm(1,-10,4)),
##                   c(0,0),
##                   c(rnorm(1,10,4),rnorm(1,10,4)))
## })
## layer <- qlayer(scene,function(layer,painter){
##   qdrawPath(painter,paths,stroke="black")
## },limits=qrect(-10,-10,10,10))
## qplotView(scene)$show()

## chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
## gr <- getIdeogram("hg19",subchr=chrmodel,cytobands=FALSE)
## obj <- CircularView(list(gr,gr),model=gr,tracksType=c("sector","scale"))
## obj$createView()
## obj$view$show()


back <- read.csv(file="/home/tengfei/Datas/james/AllplotingFormatedData/BackFatAlldata_pairwise_sig.csv")
back.gr <- GRanges(seqnames=paste("chr",back$Chr1,sep=""),
                   IRanges(start=back$locus1.btau4.0.position,
                           end=back$locus1.btau4.0.position))
colnames(back)[3] <- "to.chr"
back$to.chr <- paste("chr",back$to.chr,sep="")
colnames(back)[4] <- "to.start"
back$to.end <- back$to.start
values(back.gr) <- data.frame(back[,3:8])
idx <- order(values(back.gr)$P_value,decreasing=FALSE)[1:10000]
back.gri <- back.gr[idx]


## chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
## prepare the chromosome which need to be plotted
chrmodel <- unique(c(unique(as.character(seqnames(back.gr))),unique(values(back.gr)$to.chr)))
gr <- getIdeogram("bosTau4",subchr=chrmodel,cytobands=FALSE)
obj <- CircularView(list(back.gri,gr,back.gri,back.gri,gr),model=gr,
                    tracksType=c("link","sector","point","bar","scale"))

obj$createView()
obj$view$show()

## create link color
library(scales)
col.dt <- values(obj$tracks[[1]])$Effect
## cols <- cscale(col.dt,seq_gradient_pal("blue","red"),trans=log2_trans())
cols <- cscale(col.dt,seq_gradient_pal("blue","red"))
cols <- cscale(col.dt,div_gradient_pal("blue","white","red"))
cols <- alpha(cols,0.01)
values(obj$tracks[[1]])$.color <- cols
col.dt <- values(obj$tracks[[4]])$Test
cols <- dscale(col.dt,brewer_pal())
values(obj$tracks[[4]])$.color <- cols
qupdate(obj$scene)

## plot only 



mr <- MutableGRanges(seqname=c("chr1","chr2"),
                     IRanges(start=c(100,200),width=50),
                     .color=rep("red",2))


## painter need to be fixed later
pfunLine <- quote({
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  ## idx <- obj@pars$isPlotted[[n]]
  x <- ms+mw/2
  y <- values(gr)[,idx]
  ylim=c(min(y)*0.8,max(y)*1.2)
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  s <- sign(tracksOrder[n])
  y <- y/(diff(ylim))*(wsub*0.8)+wsub*0.1
  if(s<0) y <- wsub-y
  xy <- polar2xy(l+y,x)
  ## need to rescale y
  paintFun <- function(layer,painter){
    ## drawbg(painter,obj,l+w*(n-1)+skip*(n-1)+y,w)
    m2gm <- map2global(obj,obj@pars$model)
    mwm <- m2gm$width
    msm <- m2gm$start
    ## compute the position
    paths <- lapply(1:length(obj@pars$model),function(i){
      sa <- msm[i]
      sl <- mwm[i]
      paths <- qglyphSector(0,0,length=l,
                            width=wsub,
                            startAngle=sa,sweepLength=sl)
    })
    qdrawPath(painter,paths,fill='gray80',stroke=NA)
    seqlen <- pretty(c(l,l+wsub))
    paths <- lapply(seqlen,function(r){
      qglyphArc(0,0,r=r,0,360)          
    })
    qdrawPath(painter,paths,fill=NA,stroke='white')
    sp <- as.character(seqnames(gr))
    idx <- seq_len(length(sp))
    by(idx,sp,function(idx){
      st <- x[idx]
      idx <- idx[order(st)]
      if(is(cols,"mutaframe"))
        cols <- as.character(cols[,,drop=TRUE])
      cols <- getColor(obj@pars$tracksColorTheme[[n]],length(gr),tp)
      qdrawLine(painter,xy$x[idx],xy$y[idx],stroke='black')
    })
  }})

  ## ===================================
  ## Texts
  ## ===================================
  ## painter function for text
  ## ## compute the position
  ## m2g <- map2global(obj,gr)
  ## mw <- m2g$width
  ## ms <- m2g$start
  ## ## compute the position
  ## mp <- ms+mw/2
  ## lv <- values(gr)$.level
  ## if(tracksOrder[n]<0){
  ##   lv <- max(lv)+1-lv
  ## }
  ## wsub <- widthunit[n]
  ## skipsub <- wsub*0.2
  ## xy1 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1),mp)
  pfunText  <- function(layer,painter){
    idx <- !(mp>90 & mp<270)
    cols <- getColor(tracksColorTheme[[n]],length(mp),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    ##qantialias(painter) <- FALSE
    if(length(cols)>1){
      qdrawText(painter,as.character(seqnames(gr))[idx],
                xy1$x[idx],xy1$y[idx],halign='left',valign='center',
                rot=mp[idx],color=cols[idx])
      qdrawText(painter,as.character(seqnames(gr))[!idx],
                xy1$x[!idx],xy1$y[!idx],halign='right',valign='center',
                rot=mp[!idx]-180,color=cols[!idx])
    }else{
      qdrawText(painter,as.character(seqnames(gr))[idx],
                xy1$x[idx],xy1$y[idx],halign='left',valign='center',
                rot=mp[idx],color=cols)
      qdrawText(painter,as.character(seqnames(gr))[!idx],
                xy1$x[!idx],xy1$y[!idx],halign='right',valign='center',
                rot=mp[!idx]-180,color=cols)
    }
  }



## test
mr <- MutableGRanges(seqname=c("chr1","chr2"),
                     IRanges(start=c(100,200),width=50),
                     .color=rep("red",2))
scene <- qscene()
layer <- qlayer(scene,function(layer,painter){
  qdrawRect(painter,start(mr),10,end(mr),20,fill=values(mr)$.color)
},limits=qrect(50,0,300,40))
qplotView(scene)$show()
mr$elementMetadataChanged$connect(function(){
  qupdate(scene)
})

values(mr)$.color[1] <- "blue"

library(qtbase)
library(qtpaint)

test.gen <- setRefClass("testobj",fields=list(track="list"))
mr <- MutableGRanges(seqname=c("chr1","chr2"),
                     IRanges(start=c(100,200),width=50),
                     .color=rep("red",2))
mr
class(addLevels(mr))
obj <- test.gen$new(track=list(mr))
scene <- qscene()
layer <- qlayer(scene,function(layer,painter){
  qdrawRect(painter,start(obj$track[[1]]),10,end(obj$track[[1]]),20,fill=values(obj$track[[1]])$.color)
},limits=qrect(50,0,300,40))
qplotView(scene)$show()
lapply(obj$track,function(mr){
mr$elementMetadataChanged$connect(function(){
  print("test")
  qupdate(scene)
})
})


values(mrl[[1]])$.color[1] <- "blue"
values(obj$track[[1]])$.color[1] <- "blue"

