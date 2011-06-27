## set default theme and load them to options
## refer to zzz.R
.DefaultOpts <- function(){
  def <- VisnabView <- list(bgColor = "white",
                            fgColor = "black",
                            textColor = "black",
                            color = I("red"),
                            fill = "black",
                            stroke = "black",
                            alpha = new("NumericWithRange", min = 0, max = 1, 1),
                            bgAlpha = new("NumericWithRange", min = 0, max = 1, 1),
                            ## gridBgColor = "gray80",
                            ## gridColor = "white",
                            hoverColor = "blue",
                            xlimZoom = numeric(),
                            ylimZoom = numeric(),
                            xlim = numeric(),
                            ylim = numeric(),
                            geom = new("Enum"),
                            cpal = new("CPalEnum", "identity"),
                            dpal = new("DPalEnum", "brewer"))


  CircularView <- def
  IntervalView <- update_opts(geom = new("IntervalViewGeomEnum", "full"), data = def)
  CoverageView <- update_opts(geom = new("CoverageViewGeomEnum", "full"), data = def)
  AlignmentView <- update_opts(geom = new("AlignmentViewGeomEnum", "full"), data = def)
  SeqView <- def
  ScaleView <- update_opts(geom = new("ScaleViewGeomEnum", "full"), data = def)
  SingleChromView <- update_opts(geom = new("SingleChromViewGeomEnum", "full"),
                                 data = def)
  StackedView <- def
  TxdbView <- update_opts(geom = new("TxdbViewGeomEnum", "full"), data = def)
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


