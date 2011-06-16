## set default theme and load them to options
## refer to zzz.R
.DefaultOpts <- function(){
  def <- VisnabView <- list(bgColor = "white",
                            fgColor = "black",
                            textColor = "black",
                            color = I("red"),
                            fill = "black",
                            stroke = "black",
                            alpha = 1,
                            bgAlpha = 1,
                            gridBgColor = "gray80",
                            gridColor = "white",
                            hoverColor = "blue",
                            xlimZoom = NULL,
                            ylimZoom = NULL,
                            seqname = NULL,
                            seqlength = NULL,
                            geom = NULL,
                            cpal = blackred_pal(),
                            dpal = brewer_pal(),
                            tipsID = NULL)

  CircularView <- def
  IntervalView <- update_opts(geom = c("full","dense"), data = def)
  CoverageView <- update_opts(geom = c("total"), data = def)
  AlignmentView <- update_opts(geom = c("oneside","twoside"), data = def)
  SeqView <- def
  ScaleView <- update_opts(geom = c("twoside"), data = def)
  SingleChromView <- update_opts(geom = c("cytoband"), data = def)
  StackedView <- def
  TxdbView <- update_opts(geom = c("full","dense"), data = def)
  TracksView <- def
  return(list(VisnabView = VisnabView,
              CircularView = CircularView,
              IntervalView = IntervalView,
              AlignmentView = AlignmentView,
              CoverageView = CoverageView,
              SeqView = SeqView,
              ScaleView = ScaleView,
              SingleChromView = SingleChromView,
              StackedView = StackedView,
              TxdbView = TxdbView,
              TracksView = TracksView))
}


update_opts <- function(..., data){
  new <- list(...)
  if(length(new)==1)
    if(is.list(new[[1]]))
      new <- new[[1]]
  if(!is.list(data))
    stop("data need to be a list\n")
  nms.new <- names(new)
  nms.old <- names(data)
  idx <-  nms.new %in% nms.old
  nms.diff <- nms.new[!idx]
  ## checking names
  for(i in seq_along(nms.diff)){
    cat("variable",nms.diff[i], "not exists\n")
  }
  new.exist <- new[idx]
  ## FIXME: simply replace, not checking types yet
  for(i in seq_along(new.exist)){
    nm <- names(new.exist)[i]
    data[[nm]] <- new.exist[[i]]
  }
  return(data)
}


