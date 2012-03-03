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
## genAttr <- function(N, ...){
##     lst <- list(...)
##     nms <- names(lst)
##     lst <- lapply(nms, function(attr){
##       val <- lst[[attr]]
##       rep(val, N)
##     })
##     names(lst) <- nms
##     do.call(data.frame, lst)
##   }


setMethod("addAttr","SimpleMutableGRanges",function(obj,...){
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

setMethod("addAttr","GRanges",function(obj,...){
  lst <- list(...)
  nms <- names(lst)
  sapply(nms, function(nm){
    elementMetadata(obj)[,nm] <<- lst[[nm]]
  })
  obj
})

## ## setMethod("addAttr","MutableGRanges",function(obj,...){
## ##   lst <- list(...)
## ##   nms <- names(lst)
## ##   sapply(nms, function(nm){
## ##     elementMetadata(obj)[,nm] <- lst[[nm]]
## ##   })
## ##   obj
## ## })

## setGeneric("addDefAttr",function(obj,...) standardGeneric("addDefAttr"))


## ------------------------------------------------------------
## Utils for GenomicRanges
## ------------------------------------------------------------
## Should output to a nice tooltip format
setGeneric("getTooltipInfo",function(obj,...) standardGeneric("getTooltipInfo"))
## Suppose any hiden name is not for shown
setMethod("getTooltipInfo","GenomicRanges",function(obj,i,...){
  df <- values(obj)[i,,drop=FALSE]
  df$chrom <- as.character(seqnames(obj))[i]
  df$start <- start(obj)[i]
  df$end <- end(obj)[i]
  nms <- colnames(df)
  nms <- grep("^[^\\.]",nms,value=TRUE)
  tips <- "\n"
  for(nm in nms){
    tips <- paste(tips,paste(nm," : ",df[,nm],"\n",sep=""),sep="")
  }
  tips
})

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

GCcontent <- function(files, regions){
  grl <- pileupAsGRanges(files, regions)
  if(sum(values(gr)$depth))
      gcc <- (sum(values(gr)$G)+sum(values(gr)$C))/sum(values(gr)$depth)
  values(region)$GCcontent <- gcc
  region
}

## ## utils to generate pair-end
pspanGR <- function(file, region, sameChr = TRUE, isize.cutoff = 170){
  ## FIXME: move unmated?
  bam <- scanBam(file, param=ScanBamParam(which = region),
                       flag = scanBamFlag(hasUnmappedMate = FALSE))
  bam <- bam[[1]]
  bamrd <- GRanges(bam$rname, IRanges(bam$pos, width = bam$qwidth),
                      strand = bam$strand,
                      mseqname = bam$mrnm,
                      mstart = bam$mpos,
                      isize = bam$isize)
  ## why negative?sometime
  bamrd <- bamrd[abs(bam$isize) >= isize.cutoff]
  if(sameChr){
    idx <- as.character(seqnames(bamrd)) == values(bamrd)$mseqname 
    bamrd <- bamrd[idx]
  }
  if(length(bamrd)){
  p1 <- GRanges(seqnames(bamrd),
                ranges(bamrd))
  p2 <- GRanges(values(bamrd)$mseqname,
                IRanges(values(bamrd)$mstart, width = 75))
  pspan <- punion(p1, p2, fill.gap = TRUE)
  pgaps <- pgap(ranges(p1), ranges(p2))
  return(list(pspan = pspan, pgaps = pgaps, p1 = p1, p2 = p2))
}else{
  return(NULL)
}
}

