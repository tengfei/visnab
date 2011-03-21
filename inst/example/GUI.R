visGUI <- function(obj,s.scatter){
  GUI <- function(){
  require(gWidgetsRGtk2)
  ## obj <- obj
  pframe <- obj@mutaframe
  df <- as.data.frame(pframe)
  ## obj@mutaframe$asumsDiff <<- abs(as.numeric(as.character(df$asums1))-as.numeric(as.character(df$asums2)))
  state <<- FALSE
  filter.num <<- 1
  cbox.filters <<- c('None','Top Different Region','Top Similiar Region','Top Density Max Difference')
  options('guiToolkit'='RGtk2')
    ## ----------------------------------------------
  ## CAll BACK
  ## ---------------------------------------------
  defH <- function(h,...) print('Hello World')
  viewUCSC.cb <- function(h,...){
    require(rtracklayer)
    viewUCSC()
  }
  viewUCSC <- function(){
    session <- browserSession('UCSC')
    currentChrom <- zoomObj@chr
    ir <- IRanges(start=zoomStarts,end=zoomEnds)
    targets <- GenomicData(ir,chrom=currentChrom,genome='mm9')
    track(session,'targets') <- targets
    browserView(session,range(targets),pack='targets')
  }
  ## Just a simple control panel, so we can load file and send interactive command
  ## filter 1:
  sliderHandler.topDiff <- function(h,...){
    filt <- svalue(h$obj)
    obj@mutaframe$fill<<-'black'
    obj@mutaframe$isSelected <<- FALSE
    id <- order(asumsDiff,decreasing=TRUE)[1:filt]
    obj@mutaframe$fill[id]<<-'blue'
    obj@mutaframe$isSelected[id] <<- TRUE
    qupdate(s.scatter)
    qupdate(s.bird)
    hgroup.table[,]<<-as.data.frame(obj@mutaframe)[id,]
  }

  sliderHandler.topCommon <- function(h,...){
    filt <- svalue(h$obj)
    obj@mutaframe$fill<<-'black'
    obj@mutaframe$isSelected <<- FALSE
    id <- order(asumsDiff,decreasing=FALSE)[1:filt]
    obj@mutaframe$fill[id]<<-'blue'
    obj@mutaframe$isSelected[id] <<- TRUE
    qupdate(s.scatter)
    qupdate(s.bird)
    hgroup.table[,]<<-as.data.frame(obj@mutaframe)[id,]
  }


  clearAllHandler <- function(h,...){
    ##   if(exists('vgroup.slider.first')){
    ##   svalue(vgroup.slider.first) <- 0
    ## }
    ## if(filter.num>1){
    ##   eapply(envExpand,function(x){
    ##     svalue(x)<-0
    ##   })
    ## }
    rm(list=ls(envExpand),envir=envExpand)
    filter.num<<-1
    obj@mutaframe$fill<<-'black'
    obj@mutaframe$isSelected <<- FALSE
    obj@mutaframe$isHighlighted<<-FALSE
    if(l.wide.state){
      l.wide$close()
      l.wide.state<<- FALSE
    }
    ##  qupdate(layerIsland)
    qupdate(viewZoomTrack)
    qupdate(viewZoomChrom)
    qupdate(v.scatter)
    qupdate(s.bird)
    isChromSelected <<- TRUE
    ## delete(vgroup.f1,vgroup.table)
    ## vgroup.table <<- gdf(as.data.frame(pframe),cont=vgroup.f1,expand=TRUE)
    toplefty<<-bottomrighty<<-posstart<<-posend<<-NULL
  }

  closeHandler <- function(h,...){
    ## mywindow$close()
    dispose(win)
  }

  createFilter.cb <- function(h,...){
    ## i <- i+1
    ## names <- paste('vg.fg.f1.comp',i,sep='')
    ## assign(names,ggroup(horizontal=TRUE,cont=vg.fg.f1.expandcomp))
    vg.fg.f1.comp2 <<- ggroup(horizontal=TRUE,cont=vg.fg.f1.expandcomp)
    cbox.major <<- gcombobox(cbox.filters,cont=vg.fg.f1.comp2,handler=cboxExpandFilter)
    sliderGroupExp<<-ggroup(horizontal=TRUE,cont=vg.fg.f1.comp2,expand=TRUE)
    topDiff <- gslider(0,nrow(obj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
  }

  cboxExpandFilter <- function(h,...){
    delete(vg.fg.f1.comp2,sliderGroupExp)
    sliderGroupExp<<-ggroup(horizontal=TRUE,cont=vg.fg.f1.comp2,expand=TRUE)
    name <- svalue(h$obj)
    if(name=='Top Different Region'){
      topDiff <- gslider(0,nrow(obj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
      attr(topDiff,'name') <- 'topDiff'
      assign('topDiff',topDiff,env=envExpand)
      filter.num <<- filter.num+1
    }
    if(name=='Top Similiar Region'){
      topSim <- gslider(0,nrow(obj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
      attr(topSim,'name') <- 'topSim'
      assign('topSim',topSim,env=envExpand)
      filter.num <<- filter.num+1
    }
    if(name=='Top Smoothed Max Diff'){
      topSMD <- gslider(0,nrow(obj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
      attr(topSMD,'name') <- 'topSmoothMaxDiff'
      assign('topSmoothMaxDiff',topSMD,env=envExpand)
      filter.num <<- filter.num+1
    }
    if(name=='Top Density Max Difference'){
      topTDMD <- gslider(0,nrow(obj@mutaframe),by=0.1,cont=sliderGroupExp,expand=TRUE)
      attr(topTDMD,'name') <- 'topDensityMaxDiff'
      assign('topDensityMaxDiff',topTDMD,env=envExpand)
      filter.num <<- filter.num+1
    }
  }

  removeExpand.cb <- function(h,...){
    delete(vg.fg.f1.expand,vg.fg.f1.expandcomp)
    vg.fg.f1.expandcomp<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.expand)
    clearAllHandler()
  }

  hideFilter <- function(h,...){
    delete(vg.fg,vg.fg.f1)
  }

  expandFilter <- function(h,...){
    add(vg.fg,vg.fg.f1,expand=TRUE)
  }

  changeFilterState.cb <- function(h,...){
    if(state){
      hideFilter()
      svalue(expandIcon) <- rightArrow
    }else{
      expandFilter()
      svalue(expandIcon) <- downArrow
    }
    state<<-!state
  }

  changeFilterState <- function(){
    if(state){
      hideFilter()
      svalue(expandIcon) <- rightArrow
    }else{
      expandFilter()
      svalue(expandIcon) <- downArrow
    }
    state<<-!state
  }

  applyFilterHandler <- function(h,...){
    obj@mutaframe$fill<<-rep('black',nrow(obj@mutaframe))
    obj@mutaframe$isSelected <<- rep(FALSE,nrow(obj@mutaframe))
    if(filter.num>0){
      ## nms <- svalue(cbox.major)
      ## nms <- switch(nms,
      ##               'Top Different Region'='topDiff',
      ##               'Top Similiar Region'='topSim',
      ##               'Top Smoothed Max Diff'='topSmoothMaxDiff',
      ##               'Top Density Max Different'='topDensityMaxDiff')
      filt <- eapply(envExpand,function(x){
        df <- data.frame(name=attr(x,'name'),svalue=svalue(x),stringsAsFactors=FALSE)
      })
      filt <- do.call('rbind',filt)
      ##    filt <- rbind(filt,c(nms,svalue(vgroup.slider.first)))
      nms <- filt$name
    }else{
      filt <- NULL
      nms <- svalue(cbox.major)
    }
    idgroup <- lapply(nms,getIDbyFilter,filt)
    if('Top Density Max Difference' %in% nms){
      l.wide<<- qlayer(s.bird,paintFun=wideRange,cache=TRUE)
      l.wide.state<<-TRUE
    }
    if(length(idgroup)>1&filter.num>1){
      if(svalue(cbox.logic)=='AND'){
        id <- interGroupID(idgroup,'AND')
      }else{
        id <- interGroupID(idgroup,'OR')
      }
    }else{
      id <- idgroup
      id <- as.numeric(unlist(id))
    }
    if(length(id)>0){
      obj@mutaframe$fill[id]<<-'blue'
      obj@mutaframe$isSelected[id] <<- TRUE
      if(TRUE){
        df <- as.data.frame(obj@mutaframe)
        df <- df[order(df$start,decreasing=FALSE),]
        obj@mutaframe$marker<<-0
      }
      qupdate(s.scatter)
      qupdate(s.bird)
      ## delete(vgroup.f1,vgroup.table)
      ## vgroup.table <<- gdf(as.data.frame(obj@mutaframe)[id,],cont=vgroup.f1,expand=TRUE)
    }
  }

  ## without data frame update
  applyFilterHandler.nodf <- function(h,...){
    obj@mutaframe$fill<<-'black'
    obj@mutaframe$isSelected <<- FALSE
    if(filter.num>1){
      nms <- svalue(cbox.major)
      filt <- eapply(envExpand,function(x){
        df <- data.frame(name=attr(x,'name'),svalue=svalue(x),stringsAsFactors=FALSE)
      })
      filt <- do.call('rbind',filt)
      nms <- c(filt$name,nms)
    }else{
      filt <- NULL
      nms <- svalue(cbox.major)
    }
    idgroup <- lapply(nms,getIDbyFilter,filt)
    if(length(idgroup)>1&filter.num>1){
      hotden<<-NULL
      if(svalue(cbox.logic)=='AND'){
        id <- interGroupID(idgroup,'AND')
      }else{
        id <- interGroupID(idgroup,'OR')
      }
    }else{
      id <- idgroup
      id <- as.numeric(unlist(id))
    }
    if(id>0){
      obj@mutaframe$fill[id]<<-'blue'
      obj@mutaframe$isSelected[id] <<- TRUE
      qupdate(s.scatter)
      qupdate(s.bird)
    }
  }

  ## call back for 'Apply' button of scatter plot
  scatterApply.cb <- function(h,...){
    x.new.ori.nm <- svalue(checkX)
    y.new.ori.nm <- svalue(checkY)
    obj@pars$xy<<-c(x.new.ori.nm,y.new.ori.nm)
    summary <- obj@mutaframe
    px.ori<<-summary[[x.new.ori.nm]]
    py.ori<<-summary[[y.new.ori.nm]]
    mxx <- max(px.ori)
    mnx <- min(px.ori)
    mxy <- max(py.ori)
    mny <- min(py.ori)
    ## relative location
    scale <- obj@pars$scale
    mar <- obj@pars$mar
    px<<-(px.ori-mnx)/(mxx-mnx)*scale+mar[2]
    py<<-(py.ori-mny)/(mxy-mny)*scale+mar[3]
    pymax <<- max(py)
    pxmax <<- max(px)
    qupdate(s.scatter)
  }

  getIDbyFilter <- function(name,filt){
    if(filter.num>0){
      va.idx <- which(filt$name==name)
      va <- filt$svalue[va.idx]
      order(obj@mutaframe$den,decreasing=TRUE)[1:va]
      id <- switch(name,
                   topDiff=order(obj@mutaframe$asumsDiff,decreasing=TRUE)[1:va],
                   topSim=order(obj@mutaframe$asumsDiff,decreasing=FALSE)[1:va],
                   topSmoothMaxDiff=order(obj@mutaframe$smoothMaxDiff,decreasing=TRUE)[1:va],
                   topDensityMaxDiff=order(obj@mutaframe$den,decreasing=TRUE)[1:va]
                   )
    }else{
      ##   va <- svalue(vgroup.slider.first)
      ##   if(name=='Top Different Region'){
      ##     id <- order(obj@mutaframe$asumsDiff,decreasing=TRUE)[1:va]
      ##   }
      ##   if(name=='Top Similiar Region'){
      ##     id <-  order(obj@mutaframe$asumsDiff,decreasing=FALSE)[1:va]
      ##   }
      ##   if(name=='Top Smoothed Region'){
      ##     id  <- order(obj@mutaframe$smoothMaxDiff,decreasing=TRUE)[1:va]
      ##   }
      ##   if(name=='Top Density Max Difference'){
      ##     id <- order(obj@mutaframe$den,decreasing=TRUE)[1:va]
      ##   }
      message('No filter selected!')
    }
    id
  }

  ## intersect for group
  interGroupID <- function(sets.list,logic=c('AND','OR')){
    myfun <- switch(logic,
                    AND=function(sets.list){
                      temp <- sets.list[[1]]
                      for(i in 2:(length(sets.list))){
                        temp <- intersect(sets.list[[i]],temp)
                      }
                      temp
                    },
                    OR=function(sets.list){
                      temp <- sets.list[[1]]
                      for(i in 2:(length(sets.list))){
                        temp <- union(sets.list[[i]],temp)
                      }
                      temp
                    })
    id <- myfun(sets.list)
    id
  }
  win <<- gwindow('Control Panel',width=600,height=400)
  vgroup <<- ggroup(horizontal=FALSE,cont=win)
  mbl <- list(
              File=list(
                openFile=list(handler=defH,icon='open'),
                quit=list(handler=closeHandler,icon='cancel')
                )
              )
  tbl <- list(
              open=list(handler=defH,icon='open'),
              quit=list(handler=closeHandler,icon='cancel'),
              clear=list(handler=clearAllHandler,icon='clear')
              )
  mb <- gmenu(mbl,cont=vgroup)
  tb <- gtoolbar(tbl,cont=vgroup)
  nb <- gnotebook(cont=vgroup,expand=TRUE)
  vgroupMajor <- ggroup(horizontal=FALSE,cont=nb,expand=TRUE,label='Filter and Data')
  envExpand <<- new.env(emptyenv())
  rightArrow <<- system.file('images/1rightarrow.gif',package='gWidgets')
  downArrow <<- system.file('images/1downarrow.gif',package='gWidgets')
  expandFilterGroup <- ggroup(horizontal=TRUE,cont=vgroupMajor)
  expandIcon <<- gimage(rightArrow,cont=expandFilterGroup)
  expandLabel <<- glabel('Filter',cont=expandFilterGroup)
  addHandlerClicked(expandIcon,handler=changeFilterState.cb)
  addHandlerClicked(expandLabel,handler=changeFilterState.cb)
  ## create filter first
  vg.fg <<- ggroup(horizontal=FALSE,cont=vgroupMajor,expand=FALSE)
  vg.fg.f1 <<- gframe('Filter',cont=vg.fg,horizontal=FALSE)
  hideFilter()
  vg.fg.f1.hbox <<- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  vg.fg.f1.addButtonG <<-ggroup(horizontal=FALSE,cont=vg.fg.f1.hbox)
  vg.fg.f1.filter<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.hbox,expand=TRUE)
  ## first filter fist
  addFilterButton <<- gbutton('  +  ',cont=vg.fg.f1.addButtonG,handler=createFilter.cb, expand=FALSE)
  ## vg.fg.f1.comp <<- ggroup(horizontal=TRUE,cont=vg.fg.f1.filter,expand=TRUE)
  ## vg.fg.f1.comp.c<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=FALSE)
  ## vg.fg.f1.comp.s<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=TRUE)
  ## ## combo box
  ## cbox.major <<- gcombobox(cbox.filters,cont=vg.fg.f1.comp.c,handler=function(h,...){
  ##   delete(vg.fg.f1.comp,vg.fg.f1.comp.s)
  ##   vg.fg.f1.comp.s<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=TRUE)
  ##   va <- nrow(pframe)
  ##   va.by=1
  ##   vgroup.slider.first <<- gslider(0,va,by=va.by,cont=vg.fg.f1.comp.s,expand=TRUE)    })
  ## logic
  vg.fg.f1.expand <<- ggroup(horizontal=FALSE,cont=vg.fg.f1.filter)
  vg.fg.f1.expandcomp<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.expand)
  vg.fg.f1.logic <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  springLogic <- ggroup(horizontal=TRUE,cont=vg.fg.f1.logic,expand=TRUE)
  text.logic <- glabel('Logic',cont=vg.fg.f1.logic)
  cbox.logic <<- gcombobox(c('AND','OR'),cont=vg.fg.f1.logic)
  vg.fg.f1.button <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  sprintFilterButton <- ggroup(horizontal=TRUE,cont=vg.fg.f1.button,expand=TRUE)
  applyFilterButton <- gbutton('Apply',cont=vg.fg.f1.button,
                               handler=applyFilterHandler)
  clearFilterButton <- gbutton('Clear',cont=vg.fg.f1.button,
                               handler=removeExpand.cb)
  vg.fg.f1.ucsc <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  ucscButton <- gbutton('View in UCSC Genome Browser',cont=vg.fg.f1.ucsc,handler=viewUCSC.cb)
  vgroup.noInUse <<- ggroup(horizontal=FALSE,cont=vg.fg,expand=TRUE)
  vgroup.f1 <<- gframe('Data Table',cont=vgroupMajor,expand=TRUE)
  vgroup.table <<- gdf(df,cont=vgroup.f1,expand=TRUE)
  ##scatterPlot control panel
  scatterG <- ggroup(horizontal=FALSE,cont=nb,label='Scatter plot')
  nm <- names(pframe)[!names(pframe)%in%c('fill','isSelected')]
  scatterXYG <- ggroup(horizontal=TRUE,cont=scatterG)
  scatterXG <- ggroup(horizontal=FALSE,cont=scatterXYG)
  scatterXLabel <- glabel('X',cont=scatterXG)
  checkX <<- gcombobox(nm,cont=scatterXG)
  svalue(checkX) <- obj@pars$xy[1]
  scatterYG <- ggroup(horizontal=FALSE,cont=scatterXYG)
  scatterYLabel <- glabel('Y',cont=scatterYG)
  checkY <<- gcombobox(nm,cont=scatterYG)
  svalue(checkY) <- obj@pars$xy[2]
  scatterButtonG <- ggroup(horizontal=TRUE,cont=scatterG)
  scatterButton <- gbutton('Apply',cont=scatterButtonG,handler=scatterApply.cb)
  addSpring(scatterButtonG)
  ## showWin()
}
}





