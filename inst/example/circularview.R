## Example
require(visnab)
library(qtbase)
library(qtpaint)
library(scales)
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

## obj$createView()

setTheme(obj,bgColor="white",sectorFill="gray30",
                     linkColor="blue",barColor="black",
                     pointColor="red",pointAlpha=0.9,linkAlpha=0.01,
                     scaleColor="black")
obj$show()

length(back.gr)
visnabGUI(obj)

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



setTheme(obj,bgColor="black",sectorFill="gray10",
                     linkColor="white",barColor="gray80",
                     pointColor="red",pointAlpha=0.9,linkAlpha=0.04,
                     scaleColor="gray80")

args(qdrawText)

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

## Now do sth fun add a command line filter to 


CircularViewWindow(obj)$show()
layout <- Qt$QGridLayout()
sort(ls(layout))
wts <- Qt$QWidget()
sort(ls(wts))
