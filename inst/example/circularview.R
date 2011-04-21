## Example
library(qtpaint)
library(qtbase)
library(scales)
require(visnab)
options(warn=2)

james_pair<- function(file){
  back <- read.csv(file=file)
  idx <- which(back$Chr2==30)
  back$Chr2[idx] <- "X"
  back.gr <- GRanges(seqnames=paste("chr",back$Chr1,sep=""),
                     IRanges(start=back$locus1.btau4.0.position,
                             end=back$locus1.btau4.0.position))
  colnames(back)[3] <- "to.chr"
  back$to.chr <- paste("chr",back$to.chr,sep="")
  colnames(back)[4] <- "to.start"
  back$to.end <- back$to.start
  values(back.gr) <- data.frame(back[,3:8])
  values(back.gr)$Effect <- abs(values(back.gr)$Effect)
  back.gr
}

james_single <- function(file){
  back.sig <- read.csv(file=file)
  idx <- which(back.sig$Chr==30)
  back.sig$Chr[idx] <- "X"
  back.sig.gr <- GRanges(seqnames=paste("chr",back.sig$Chr,sep=""),
                         IRanges(start=back.sig$locus.btau4.0.position,
                                 end=back.sig$locus.btau4.0.position))

  values(back.sig.gr) <- data.frame(back.sig[,3:5])
  values(back.sig.gr)$Effect <- abs(values(back.sig.gr)$Effect)
  back.sig.gr
}

file1="/home/tengfei/Datas/james/Iron_pairwise_sig.csv"
back.gr <- james_pair(file1)

file2="/home/tengfei/Datas/james/Iron_single_sig.csv"
back.sig.gr <- james_single(file2)
## idx <- order(values(back.sig.gr)$P_value,decreasing=FALSE)[1:100]
## back.sig.gr <- back.sig.gr[idx]
## chrmodel <- paste("chr",c(1:23,"X","Y"),sep="")
## prepare the chromosome which need to be plotted
## pass GRanges/MutableGRanges object, return unique chromosome names

chrmodel <- chrAll(back.gr,back.sig.gr)

gr <- getIdeogram("bosTau4",subchr=chrmodel,cytobands=FALSE)
## gr <- getIdeogram("bosTau4",cytobands=FALSE)
## obj <- CircularView(list(back.gr,gr,back.sig.gr[,"Effect"],back.gr,gr),model=gr, tracksType=c("link","sector","point","bar","scale"))

obj <- CircularView(list(back.gr,back.sig.gr[,"Effect"],back.gr,gr,gr),model=gr, tracksType=c("link","point","bar","sector","scale"))

obj$createView()
obj$view$show()

length(back.gr)


## create link color

## col.dt <- values(obj$tracks[[1]])$Effect
## cols <- cscale(col.dt,seq_gradient_pal("blue","red"),trans=log2_trans())
## cols <- cscale(col.dt,seq_gradient_pal("blue","red"))
## cols <- cscale(col.dt,div_gradient_pal("blue","white","red"))

setTheme <- function(obj,bgColor="white",sectorFill="gray80",
                     linkColor="blue",barColor="black",
                     pointColor="red",pointAlpha=0.3,linkAlpha=0.05,
                     scaleColor="gray10"){
  require(scales)
  require(qtbase)
  require(qtpaint)
  bgcol <- bgColor
  qcol <- col2qcol(bgcol,1)
  obj$scene$setBackgroundBrush(qbrush(qcol))
  values(obj$tracks[[4]])$.color <- sectorFill
  cols <- alpha(linkColor,linkAlpha)
  values(obj$tracks[[1]])$.color <- cols
  ## col.dt <- values(obj$tracks[[3]])$Test
  ## cols <- dscale(col.dt,brewer_pal())
  values(obj$tracks[[3]])$.color <- "black"
  values(obj$tracks[[2]])$.color <- alpha(pointColor,pointAlpha)
  values(obj$tracks[[5]])$.color <- scaleColor
  qupdate(obj$scene)
}

setTheme(obj,bgColor="white",sectorFill="gray30",
                     linkColor="blue",barColor="black",
                     pointColor="red",pointAlpha=0.9,linkAlpha=0.01,
                     scaleColor="black")

setTheme(obj,bgColor="black",sectorFill="gray10",
                     linkColor="white",barColor="gray80",
                     pointColor="red",pointAlpha=0.9,linkAlpha=0.04,
                     scaleColor="gray80")

## 

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
