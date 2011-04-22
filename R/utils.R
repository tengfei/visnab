## ------------------------------------------------------------
## Utils for GenomicaRanges
## ------------------------------------------------------------

setGeneric('removePrefix',function(gr,...) standardGeneric('removePrefix'))

setMethod('removePrefix','GenomicRanges',function(gr,rm.prefix){
  seqnames(gr) <- gsub(rm.prefix,'',as.character(seqnames(gr)))
  gr
})

setMethod('removePrefix','MutableGRanges',function(gr,rm.prefix){
  seqnames(gr) <- gsub(rm.prefix,'',as.character(seqnames(gr)))
  gr
})


setGeneric('addPrefix',function(gr,...) standardGeneric('addPrefix'))

setMethod('addPrefix','GenomicRanges',function(gr,add.prefix){
  seqnames(gr) <- paste(add.prefix,as.character(seqnames(gr)),sep='')
  vl <- values(gr)
  if('to.chr' %in% names(vl)){
    values(gr)$to.chr <- paste(add.prefix,values(gr)$to.chr,sep='')
  }
  gr
})

setMethod('addPrefix','MutableGRanges',function(gr,add.prefix){
  seqnames(gr) <- paste(add.prefix,as.character(seqnames(gr)),sep='')
  vl <- values(gr)
  if('to.chr' %in% names(vl)){
    values(gr)$to.chr <- paste(add.prefix,values(gr)$to.chr,sep='')
  }
  gr
})


setGeneric('validateChr',function(gr,...) standardGeneric('validateChr'))

setMethod('validateChr',c('GenomicRanges'),
          function(gr,model,...){
            if(inherits(class(model),'GenomicRanges'))
              chrset <- unique(as.character(seqnames(model)))
            else
              chrset <- model
            chr <- as.character(seqnames(gr))
            gr <- gr[chr %in% chrset]
            return(gr)
          })

setMethod('validateChr',c('MutableGRanges'),
          function(gr,model,...){
            if(inherits(class(model),'MutableGRanges'))
              chrset <- unique(as.character(seqnames(model)))
            else
              chrset <- model
            chr <- as.character(seqnames(gr))
            gr <- gr[chr %in% chrset]
            return(gr)
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

setGeneric("sortChr",function(obj,...) standardGeneric("sortChr"))

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


setGeneric("orderChr",function(obj,...) standardGeneric("orderChr"))

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


setGeneric("replaceChr",function(obj,...) standardGeneric("replaceChr"))

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




## ## need to drop this stuff and 
## scaleColors <- function(obj,low="red",mid="white",high="yellow",alpha=1){
##   cols <- cscale(obj,gradient_2_pal(low="red",high="yellow"))
##   cols <- alpha(cols,alpha)
##   cols <- mutaframe(.color=cols)
##   cols
## }


## map2granges <- function(gct){
##   idx <- seq_len(nrow(gct))
##   chrlist <- lapply(idx,function(i){
##     probe <- gct[i,1]
##     chr <- hgu133aCHR[[as.character(probe)]]
##     chrstart <- hgu133aCHRLOC[[as.character(probe)]]
##     chrend <- hgu133aCHRLOCEND[[as.character(probe)]]
##     chrstrand <- ifelse(sign(chrstart)<0,'-','+')
##     data.frame(id=i,chr,chrstart,chrend,chrstrand)
##   })
##   chr <- do.call('rbind',chrlist)
##   gr <- GRanges(seqnames=chr$chr,ranges=IRanges(start=abs(chr$chrstart),end=abs(chr$chrend)),strand=chr$chrstrand)
##   elementMetadata(gr) <- gct[chr$id,]
##   gr
## }


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



splitDNA <- function(dna){
  dnas <- toString(dna)
  dnas.split <- unlist(strsplit(dnas,""))
  return(dnas.split)
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
  ## Check if column exists
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
  elementMetadata(obj) <- c(df,dfex)
  ## should record attached attr
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
    chrs <- unique(as.character(seqnames(back.gr)))
    if("to.chr" %in% names(values(back.gr))){
      chrs2 <- unique(as.character(values(gr)$to.chr))
      chrs <- unique(c(chrs,chrs2))
      }
    chrs
  })
  chrs <- sortChr(unique(unlist(chr.lst)))
  return(chrs)
}
