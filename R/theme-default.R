## set default theme and load them to options
## refer to zzz.R
.DefaultOpts <- function(){
  ## create some list used for GUI
  ## parinfo used for showing actual name, easy to read
  parinfolst <- list(bgColor = "Background color",
                     fgColor = "Frontground color",
                     textColor = "Text color",
                     color = "General color",
                     fill = "Fill color",
                     stroke = "Stroke color",
                     alpha = "Alhpha blending(glyphs if any)",
                     bgAlpha = "Alpha blending(Background)",
                     hoverColor = "Color(when mouse hover)",
                     xlimZoom = "Viewport Range(x-scale)",
                     ylimZoom = "Viewport Range(y-scale)",
                     xlim = "Limits on x-scale",
                     ylim = "Limits on y-scale",
                     geom = "Geometry",
                     cpal = "Palletes(Continuous variables)",
                     dpal = "Palletes(Continuous variables)")

  ## tooltipinfo used for showing tooltip, wihch is descriptive
  tooltipinfolst <- list(bgColor = "no tool tip defined yet",
                         fgColor = "no tool tip defined yet",
                         textColor = "no tool tip defined yet",
                         color = "no tool tip defined yet",
                         fill = "no tool tip defined yet",
                         stroke = "no tool tip defined yet",
                         alpha = "no tool tip defined yet",
                         bgAlpha = "no tool tip defined yet",
                         hoverColor = "no tool tip defined yet",
                         xlimZoom = "no tool tip defined yet",
                         ylimZoom = "no tool tip defined yet",
                         xlim = "no tool tip defined yet",
                         ylim = "no tool tip defined yet",
                         geom = "no tool tip defined yet",
                         cpal = "no tool tip defined yet",
                         dpal = "no tool tip defined yet")

  ## exposed decide which parameters exposed to users
  exposedlst <- list(bgColor = TRUE,
                     fgColor = TRUE,
                     textColor = TRUE,
                     color = TRUE,
                     fill = TRUE,
                     stroke = TRUE,
                     alpha = TRUE,
                     bgAlpha = TRUE,
                     hoverColor = TRUE,
                     xlimZoom = FALSE,
                     ylimZoom = FALSE,
                     xlim = FALSE,
                     ylim = FALSE,
                     geom = TRUE,
                     cpal = TRUE,
                     dpal = TRUE)


  def <- VisnabView <- list(bgColor = "white",
                            fgColor = "black",
                            textColor = "black",
                            color = I("red"),
                            fill = "black",
                            stroke = "black",
                            alpha = new("NumericWithRange", min = 0, max = 1, 1),
                            bgAlpha = new("NumericWithRange", min = 0, max = 1, 1),
                            hoverColor = "blue",
                            xlimZoom = numeric(),
                            ylimZoom = numeric(),
                            xlim = numeric(),
                            ylim = numeric(),
                            geom = new("Enum"),
                            cpal = new("CPalEnum", "identity"),
                            dpal = new("DPalEnum", "brewer"),
                            parinfo = parinfolst,
                            tooltipinfo = tooltipinfolst,
                            exposed = exposedlst)


  CircularView <- def
  IntervalView <- update_opts(geom = new("IntervalViewGeomEnum", "full"), data = def)
  CoverageView <- update_opts(geom = new("CoverageViewGeomEnum", "total"), data = def)
  AlignmentView <- update_opts(geom = new("AlignmentViewGeomEnum", "full"), data = def)
  SeqView <- update_opts(geom = new("SeqViewGeomEnum", "default"), data = def)
  ScaleView <- update_opts(geom = new("ScaleViewGeomEnum", "twoside"), data = def)
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


