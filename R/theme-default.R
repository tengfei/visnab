## set default theme and load them to options
## refer to zzz.R
.DefaultOpts <- function(){
  def <- VisnabView <- list(bgColor = "gray80",
                            fgColor = "black",
                            textColor = "black",
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
                            geom = NULL,
                            cpal = NULL,
                            dpal = NULL)
  CircularView <- def
  IntervalView <- update_opts(geom = c("full","dense"), data = def)
  AlignmentView <- def
  SeqView <- def
  ScaleView <- update_opts(geom = c("twosides"), data = def)
  SingleChromView <- def
  StackedView <- def
  TxdbView <- update_opts(geom = c("full","dense"), data = def)
  TracksView <- def
  return(list(VisnabView = VisnabView,
              CircularView = CircularView,
              IntervalView = IntervalView,
              AlignmentView = AlignmentView,
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


