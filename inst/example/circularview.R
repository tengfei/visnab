## Example
library(MutableRanges)
library(visnab)
options(warn=2)
 library(qtbase)
 library(qtpaint)

james_pair<- function(file){
  back <- read.csv(file=file)
  cat("NA rows: ",nrow(back)-nrow(na.omit(back)))
  idx <- which(back$Chr2==30)
  back$Chr2[idx] <- "X"
  idx <- which(back$Chr1==30)
  back$Chr1[idx] <- "X"
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
  cat("NA rows: ",nrow(back.sig)-nrow(na.omit(back.sig)))
  back.sig <- na.omit(back.sig)
  idx <- which(back.sig$Chr==30)
  back.sig$Chr[idx] <- "X"
  back.sig.gr <- GRanges(seqnames=paste("chr",back.sig$Chr,sep=""),
                         IRanges(start=back.sig$locus.btau4.0.position,
                                 end=back.sig$locus.btau4.0.position))
  values(back.sig.gr) <- data.frame(back.sig[,3:5])
  values(back.sig.gr)$Effect <- abs(values(back.sig.gr)$Effect)
  back.sig.gr
}

setTheme <- function(obj,bgColor="white",sectorFill="gray80",
                     linkColor="blue",barColor="black",
                     pointColor="red",pointAlpha=0.3,linkAlpha=0.02,
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
  ## qupdate(obj$scene)
}

## file1="/home/tengfei/Datas/james/Iron_pairwise_sig.csv"
## back.gr <- james_pair(file1)

## file2="/home/tengfei/Datas/james/Iron_single_sig.csv"
## back.sig.gr <- james_single(file2)

files <- list.files("/home/tengfei/Datas/james/ForPlotting",full.name=TRUE)
## files.all <- grep("[A|a]ll[D|d]ata",files,value=TRUE)
## files.all
files.single <- grep("[S|s]ingle",files,value=TRUE)
files.pair <- grep("pairwise",files,value=TRUE)

files.single
files.pair <- files.pair[-7]
files.pair
## gr <- getIdeogram("bosTau4",cytobands=FALSE)
## grbosideo <- gr
## save(grbosideo, file = "~/Datas/rdas/grbosideo.rda")
load("~/Datas/rdas/grbosideo.rda")
env <- new.env()
sapply(c(1:2),function(i){
  back.gr <- james_pair(files.pair[i])
  back.sig.gr <- james_single(files.single[i])
  chrmodel <- chrAll(back.gr,back.sig.gr)
  gr <- grbosideo[seqnames(grbosideo) %in% chrmodel]
  obj.cir <- CircularView(list(back.gr,back.sig.gr[,"Effect"],back.gr,gr,gr),
                          model=gr, tracksType=c("link","point","bar","sector","scale"))
  setTheme(obj.cir,bgColor="white",sectorFill="gray30",
           linkColor="blue",barColor="black",
           pointColor="red",pointAlpha=0.9,linkAlpha=0.01,
           scaleColor="black")
  assign(paste("obj",i,sep=""),obj.cir,envir=env)
})

visnabGUI(env$obj1)

setTheme(env$obj1,bgColor="black",sectorFill="gray10",
                     linkColor="white",barColor="gray80",
                     pointColor="red",pointAlpha=0.9,linkAlpha=0.04,
                     scaleColor="gray80")

mr <- env$obj1$tracks[[1]]
idx <- as.character(seqnames(mr)) == elementMetadata(mr)$to.chr
elementMetadata(env$obj1$tracks[[1]])$.color[idx] <- "red"
elementMetadata(env$obj1$tracks[[1]])$.color[idx] <- "white"


setTheme(env$obj1,bgColor="white",sectorFill="gray30",
           linkColor="blue",barColor="black",
           pointColor="red",pointAlpha=0.9,linkAlpha=0.01,
           scaleColor="black")

visnabGUIlist(env)




