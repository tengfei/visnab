## ------------------------------------------------------------
## Utils for GenomicaRanges
## ------------------------------------------------------------


setMethod('addLevels','GenomicRanges',function(obj,...){
  obj.lst <- split(obj,as.character(seqnames(obj)))
  lv <- unname(lapply(obj.lst,function(x){
    values(x)$.level <- as.numeric(disjointBins(ranges(x)))
    x
  }))
  obj <- do.call("c",lv)
})

setMethod('removePrefix','GenomicRanges',function(obj,rm.prefix){
  seqnames(obj) <- gsub(rm.prefix,'',as.character(seqnames(obj)))
  obj
})

## setMethod('removePrefix','MutableGRanges',function(gr,rm.prefix){
##   seqnames(gr) <- gsub(rm.prefix,'',as.character(seqnames(gr)))
##   gr
## })


setMethod('addPrefix','GenomicRanges',function(obj,add.prefix){
  seqnames(obj) <- paste(add.prefix,as.character(seqnames(obj)),sep='')
  vl <- values(obj)
  if('to.chr' %in% names(vl)){
    values(obj)$to.chr <- paste(add.prefix,values(obj)$to.chr,sep='')
  }
  obj
})


setMethod('validateChr',c('GenomicRanges'),
          function(obj,model,...){
            if(inherits(class(model),'GenomicRanges'))
              chrset <- unique(as.character(seqnames(model)))
            else
              chrset <- model
            chr <- as.character(seqnames(obj))
            obj <- obj[chr %in% chrset]
            return(obj)
          })

isValidatedChr <- function(grl,model){
  if(is(grl,'list')){
    chrset <- unique(as.character(seqnames(model)))
    allLst <- lapply(seq_len(length(grl)),function(i) {
      idx <- unique(as.character(seqnames(grl[[i]]))) %in% chrset
      if(all(idx)==FALSE){
        message(paste("Chromosome names of",names(grl)[i],"is invalid"))
        message(paste(unique(as.character(seqnames(grl[[i]])))[!idx],' '),'is invalid')
      }
      all(idx)
    })
    return(all(unlist(allLst)))
  }
  if(is(grl,'GenomicRanges')){
    chrset <- unique(as.character(seqnames(model)))
    idx <- unique(as.character(seqnames(grl))) %in% chrset
    return(all(idx))
  }
}

containLetters <- function(obj,only=FALSE){
  obj <- as.character(obj)
  obj <- tolower(obj)
  obj <- unlist(strsplit(obj,""))
  if(!only){
    res <- any(obj%in%letters)
    return(res)
  }else{
    res <- all(obj%in%letters)
    return(res)
  }
}


setMethod("sortChr","GenomicRanges",function(obj,model=NULL,prefix="chr"){
  idx <- orderChr(as.character(seqnames(obj)),model,prefix)
  obj[idx]
})


setMethod("sortChr","factor",function(obj,model=NULL,prefix="chr"){
  sortChr(as.character(obj),model,prefix)
})

setMethod("sortChr","character",function(obj,model=NULL,prefix="chr"){
  chr <- as.character(obj)
  chrtemp <- gsub(prefix,'',chr)
  df <- data.frame(chr=chrtemp,id=seq_len(length(chrtemp)))
  idx <- apply(df,1,function(x) !containLetters((x['chr'])))
  if(is.null(model)){
    idxs <- c(df[idx,'id'][order(as.numeric(as.character(df$chr[idx])))],
              df[!idx,'id'][order(as.character(df$chr[!idx]))])
    idxs <- as.numeric(idxs)
    return(chr[idxs])
  }else{
    chrmodel <- as.character(model)
    chrmodel <- gsub(prefix,'',chrmodel)
    idx <- match(chrtemp,chrmodel)
    idx <- order(idx)
    return(chr[idx])
  }
})


setMethod("orderChr","GenomicRanges",function(obj,model=NULL,prefix="chr"){
  idx <- orderChr(as.character(seqnames(obj)),model,prefix)
  idx
})

setMethod("orderChr","character",function(obj,model=NULL,prefix='chr'){
  chr <- as.character(obj)
  chrtemp <- gsub(prefix,'',chr)
  df <- data.frame(chr=chrtemp,id=seq_len(length(chrtemp)))
  idx <- apply(df,1,function(x) !containLetters((x['chr'])))
  if(is.null(model)){
    idxs <- c(df[idx,'id'][order(as.numeric(as.character(df$chr[idx])))],
              df[!idx,'id'][order(as.character(df$chr[!idx]))])
    idxs <- as.numeric(idxs)
    return(idxs)}else{
      chrmodel <- as.character(model)
      chrmodel <- gsub(prefix,'',chrmodel)
      idx <- match(chrtemp,chrmodel)
      idx <- order(idx)
      return(idx)
    }
})


setMethod("replaceChr","GenomicRanges",function(obj,from,to){
  new.chr <- replaceChr(as.character(seqnames(obj)),from,to)
  seqnames(obj) <- new.chr
  dts <- values(obj)
  if("to.chr" %in% names(dts)){
    dts$to.chr <- replaceChr(dts$to.chr,from,to)
    values(obj) <- dts
  }
  obj
})

setMethod("replaceChr","character",function(obj,from,to){
  idx <- from == obj
  obj[idx] <- to
  return(obj)
})



xy2polar <- function(x,y){
  angle <- atan(y/x)/pi*180
  radius <- sqrt(x^2+y^2)
  data.frame(radius=radius,angle=angle)
}

visZoom <- function(obj,scale.factor=c(4,4)){
  ## this obj must contain list of scene, viewU, layer
  visenv$new.view <- qplotView(obj$scene)
  visenv$new.view$show()
  visenv$new.view$scale(scale.factor[1],scale.factor[2])
}

setHover <- function(obj,layer.name){
  layers <- obj$layer
  nms <- names(layers)
  layers[[layer.name]]$setAcceptsHoverEvents(TRUE)
  for(i in setdiff(nms,layer.name)){
    layers[[i]]$setAcceptsHoverEvents(FALSE)
  }
}


setHoverNext <- function(obj){
  layers <- obj$layer
  nms <- names(layers)
  idx <- unlist(unname(lapply(layers,function(x) x$acceptsHoverEvents())))
  if(sum(idx)>1|sum(idx)==0){
    idx <- rep(FALSE,length(idx))
    idx[1] <- TRUE
    idxs <- 1
  }
  if(sum(idx)==1){
    idxs <- which(idx==TRUE)
    if(idxs==length(idx)){
      idxs <- 1
    }else{
      idxs <- idxs+1
    }
  }
  lapply(layers,function(x){
    x$setAcceptsHoverEvents(FALSE)
  })
  layers[[idxs]]$setAcceptsHoverEvents(TRUE)
  message(nms[idxs])
}

col2qcol <- function(color,alpha=1){
  cols <- col2rgb(alpha(color,alpha),TRUE)
  qcolor(unname(cols[1,1]),
         unname(cols[2,1]),
         unname(cols[3,1]),
         unname(cols[4,1]))
}

baseColor <- function(base,pal=brewer_pal(pal="Set1")){
  cols <- dscale(base,brewer_pal(pal="Set1"))
  obj <- list()
  for(i in 1:length(base))
    obj[[base[i]]] <- cols[i]
  class(obj) <- "ColorList"
  obj
}

reduceChr <- function(obj){
  grl <- split(obj,seqnames(obj))
  lst <- lapply(names(grl),function(nms){
    GRanges(seqnames=nms,IRanges(0,max(end(grl[[nms]]))))
  })
  ngr <- do.call('c',lst)
  sortChr(ngr)
}

## ------------------------------------------------------------
## Utils for MutableGRanges
## ------------------------------------------------------------
## Add extra attributes to an MutableRanges object
## This is going to be naming routines in visnab.
## Specific signal should be bound to MR object.
setGeneric("addAttr",function(obj,...) standardGeneric("addAttr"))

setMethod("addAttr","MutableGRanges",function(obj,...){
  lst <- list(...)
  nms <- names(lst)
  df <- elementMetadata(obj)
  nms.exist <- colnames(df)
  idx <- rep(FALSE,length(nms))
  lst <- lapply(lst,function(x) rep(x,nrow(df)))
  sapply(seq_along(nms),function(i){
    if((nms[i] %in% nms.exist)&&(identical(as.character(lst[[nms[i]]]),as.character(df[,nms[i]]))))
      idx[i] <<- TRUE
  })
  lst <- lst[!idx]
  dfex <- as.data.frame(do.call(cbind,lst),stringsAsFactors=FALSE)
  ## New attributes, haven't check selection in plumbr yet
  df.nw <- c(df,as(dfex,"DataFrame"))
  df.nw
  obj
  elementMetadata(obj)  <- df.nw
  ## elementMetadata(obj)  <- df
  obj
})

## setGeneric("addDefAttr",function(obj,...) standardGeneric("addDefAttr"))


## ------------------------------------------------------------
## Utils for GenomicRanges
## ------------------------------------------------------------
## Should output to a nice tooltip format
setGeneric("getTooltipInfo",function(obj,...) standardGeneric("getTooltipInfo"))
## Suppose any hiden name is not for shown
setMethod("getTooltipInfo","GenomicRanges",function(obj,i,...){
  df <- values(obj)[i,,drop=FALSE]
  nms <- colnames(df)
  nms <- grep("^[^\\.]",nms,value=TRUE)
  tips <- "\n"
  for(nm in nms){
    tips <- paste(tips,paste(nm," : ",df[,nm],"\n",sep=""),sep="")
  }
  tips
})



##----------------------------------------------------------------##
##                         ideogram
##----------------------------------------------------------------##
getIdeogram <- function(species=NULL,subchr=NULL,cytobands=TRUE){
  if(!(exists("session")&&extends(class(session),"BrowserSession")))
    session <- browserSession()
  if(is.null(species)){
    choices <- ucscGenomes()[,1]
    res <- menu(choices,title="Please specify genome")
    species <- as.character(choices[res])
  }
  if(cytobands){
    message("Loading...")
    query <- ucscTableQuery(session,"cytoBand",species)
    tableName(query) <- "cytoBand"
    df <- getTable(query)

    gr <- GRanges(seqnames=df$chrom,
                  IRanges(start=df$chromStart,end=df$chromEnd))

    values(gr) <- df[,c("name","gieStain")]
    message("Done")
    ## validate chr, keep less chromosomes, more useful for visualization.
  }else{
    gr <- GRangesForUCSCGenome(species)
  }
  if(!is.null(subchr))
    gr <- validateChr(gr,subchr)
  ## better order them too.
  gr <- sortChr(gr)
  gr
}


chrAll <- function(...){
  lst <- list(...)
  chr.lst <- lapply(lst,function(gr){
    chrs <- unique(as.character(seqnames(gr)))
    if("to.chr" %in% names(values(gr))){
      chrs2 <- unique(as.character(values(gr)$to.chr))
      chrs <- unique(c(chrs,chrs2))
    }
    chrs
  })
  chrs <- sortChr(unique(unlist(chr.lst)))
  return(chrs)
}

## Interactive indicator
## start to record

IMessageStart <- function(geometry=qrect(0,0,10,100),leaf=20,freq=0.05){
  .indicatorScene <<- qscene()
  .indicatorLayer <<- qlayer(.indicatorScene)
  .indicatorView <<- qplotView(.indicatorScene)
  .messageLayer <- qlayer(.indicatorLayer,paintFun=function(layer,painter){
    if(exists(".message"))
      qdrawText(painter,.message,0,0,color="black")
  },col=1,rowSpan=3)
  gr <- GRanges(seqnames=paste("chr",1:leaf),
                ranges=IRanges(start=rep(1,leaf),
                  end=rep(10,leaf)))
  obj <- CircularView(list(gr),tracksType="sector",model=gr,scene=.indicatorScene,
                      view=.indicatorView,rootLayer=.indicatorLayer,col=0,row=0,
                      .sectorText=FALSE,
                      tracksWidth=80)
  obj$show()
  layout <- .indicatorLayer$gridLayout()
  layout$setRowPreferredHeight(0,10)
  layout$setColumnPreferredWidth(0,10)
  layout$setColumnPreferredWidth(1,100)

  ## colorchange
  ## if(.indicatorFlag){
  if(TRUE){
    for(idx in 1:leaf){
      values(obj$tracks[[1]])$.color[idx] <- "black"
      Sys.sleep(freq)
      values(obj$tracks[[1]])$.color[idx] <- "white"
      Sys.sleep(freq)
    }
  }
}



##
## IMessageStart(freq=0.0001)
## .indicatorScene$setBackgroundBrush(qbrush(col2qcol("black",0)))
## .indicatorScene$setBackgroundBrush(qbrush(qcolor(255,255,0,0)))

## .indicatorScene$setBackgroundBrush(qbrush(col2qcol("lightgray"), Qt$Qt$VerPattern))

IMessage <- function(..., scene=.indicatorScene,
                     view=.indicatorView,
                     rootLayer=.indicatorLayer,
                     leaf=7){
  ## fun with circular view, pseudo "chromosome"
  ## should support RangedData later
  .message <<- paste(...)
}

## IMessage()


## pileup function
## this should be one of the filter function? or compute method for bam file?
## A second take on the tally stuff using Rsamtools

pileupAsGRanges <- function(bams, regions,
                            samBases = c(A = 2, C = 3, G = 5, T = 9),...) {
  pileupFiles <- PileupFiles(bams)
  pileupParams <- PileupParam(which = regions, ...)

  bamNames <- names(bams)
  if (is.null(bamNames))
    bamNames <- bams
  pileupFun <- function(x) {
    seq <- x$seq[samBases,,]
    ## dim(seq) <- c(nrow(seq), ncol(seq) * dim(seq)[3])
    rownames(seq) <- names(samBases)
    GRanges(rep(names(x$seqnames), each = length(bams)),
            IRanges(rep(x$pos, each = length(bams)), width = 1),
            "+",
            t(seq),
            depth = colSums(seq), bam = bamNames)
  }
  gr <- do.call(c, applyPileups(pileupFiles, pileupFun, param = pileupParams))
  ## strand(gr) <- rep(strand(regions), width(regions) * length(bams))
  rc <- strand(gr) == "-"
  if(sum(rc) > 0)
    values(gr)[rc, DNA_BASES] <-
      values(gr)[rc, as.character(complement(DNAStringSet(DNA_BASES)))]
  gr
}

pileupGRangesAsVariantTable <- function(gr, genome, DNA_BASES, mismatchOnly = FALSE) {
  refBases <- unlist(as.character(BSgenome::getSeq(genome, gr)), use.names=FALSE)
  ## df <- as.data.frame(values(gr))
  if(missing(DNA_BASES)){
    .reservedNames <- c("depth","bam")
    DNA_BASES <- setdiff(colnames(elementMetadata(gr)), .reservedNames)
  }
  counts <- as.matrix(as.data.frame(values(gr)[,DNA_BASES]))
  refCounts <- counts[cbind(seq(nrow(counts)), match(refBases, DNA_BASES))]
  variantsForBase <- function(base) {
    baseCounts <- counts[,base, drop=TRUE]
    keep <- baseCounts > 0
    count <- baseCounts[keep]
    gr <- GRanges(seqnames(gr)[keep], ranges(gr)[keep], strand(gr)[keep],
                  ref = refBases[keep], read = base, count = baseCounts[keep],
                  count.ref = refCounts[keep],
                  values(gr)[keep, setdiff(colnames(values(gr)), DNA_BASES)])
    if(mismatchOnly)
      gr <- gr[values(gr)$read != values(gr)$ref]
    gr
  }
  lst <- lapply(colnames(counts), variantsForBase)
  ## browser()
  ## lens <- lapply(lst, length)
  ## N <- sum(unlist(lens))
  ## gr <- GRanges()
  ## seqs <- Reduce("+", unlist(lens), acc = TRUE)
  ## seqs
  ## lapply(seq_along(seqs), function(i) {
  ##   gr[]
  ## })
  res <- do.call("c", lst)  ## why this is slow
  return(res)
}





## general utils used in createView function
setDislayWidgets <- function(obj, dragMode = TRUE){
  obj$scene <- qscene()
  obj$view <- qplotView(obj$scene,rescale = obj$rescale)
  obj$view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
  obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600))
}
setBgColor <- function(obj){
  bgcol <- obj$pars$bgColor
  bgalpha <- obj$pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  obj$scene$setBackgroundBrush(qbrush(qcol))
}

