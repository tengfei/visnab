##----------------------------------------------------------------------------##
##             For class "SeqView"
##----------------------------------------------------------------------------##

setClass("SeqView",contains="GraphicPars",
         representation(track="BSgenome",
                        strand="characterOrNULL"))



##----------------------------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------------------------##

SeqView <- function(obj,...){
  pars <- GraphicPars(...)@pars
  new("SeqView",track=obj, pars=pars, strand="+")
}


##----------------------------------------------------------------------------##
##             visplot method
##----------------------------------------------------------------------------##

setMethod("visplot","SeqView",function(obj,seqname,start=NULL,
                                             end=NULL,width=NULL,show=TRUE,
                                             scene=NULL,view=NULL,rootLayer=NULL,
                                             row=0L,col=0L){
  seqname <- "chr1"
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(is.null(start) | is.null(end)){
    start <- 0
    end <- seqlengths(Hsapiens)[[seqname]]
  }
  zoomLevels <- c(500,50)
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  h <- 10
  wheelZoom <- function(layer, event) {
    zoom_factor <- 1.5
    if(event$delta()<0)
      zoom_factor <- 1/1.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    view$setTransform(tform)
  }
  
  pfun <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ## draw scale
    scaleUnit <- as.integer(diff(xlimZoom))/5L
    xscale <- as.integer(xlimZoom[1]+scaleUnit*(0:5))
    N <- length(xscale)
    qdrawSegment(painter,start,-h/2,end,-h/2,stroke="white")
    qdrawSegment(painter,xscale[-N],-h/2-h/9,xscale[-N],-h/2+h/9,stroke="white")
    idx <- (1L:as.integer(N/2))*2L
    qdrawText(painter,xscale[idx],
              xscale[idx],
              -h/2-h/9,"center","top",color="white")
    qdrawText(painter,xscale[idx-1],xscale[idx-1],-h/2+h/9,"center","bottom",
              color="white")

    ## level1:draw gray bars
    if(diff(xlimZoom)>zoomLevels[1]){
      qdrawRect(painter,start,h/2,end,h/2+h/9,fill='gray80',stroke=NULL)
    }
    
    ## level3:draw colored text
    if(diff(xlimZoom)<zoomLevels[1]&diff(xlimZoom)>zoomLevels[2]){
      dna <- obj@track[[seqname]]
      dna <- subseq(dna,start=xlimZoom[1],end=xlimZoom[2])
      dnas.split <- splitDNA(dna)
      dnacol <- baseColor(splitDNA("ACTG"))
      idx <- match(dnas.split,names(dnacol))
      cols <- unname(unlist(dnacol[idx]))
      w <- as.integer(diff(xlimZoom))
      x_pos <- as.integer(xlimZoom[1]+(1:w)-1)
      qdrawText(painter,dnas.split,x_pos,h/2,"center","bottom",
                color=cols)
    }}
  layer <- qlayer(rootLayer,paintFun=pfun,
                  limits=qrect(start,h/2-h/9-h/4,end,h/2+h/9+h/4),
                  wheelFun=wheelZoom,row=row,col=col)

  if(show)
    view$show()
  return(list(scene=scene,view=view,layer=layer))
})





