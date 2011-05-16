## set default theme and load them to options
## refer to zzz.R
.DefaultOpts <- function(){
  VisnabView <- list(bgColor = "gray80",
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
                     cpal = NULL,
                     dpal = NULL)
  CircularView <- list()
  IntervalView <- list(geom = c("full","dense"))
  AlignmentView <- list()
  SeqView <- list()
  StackedView <- list()
  TxdbView <- list()
  TracksView <- list()
  return(list(VisnabView = VisnabView,
              CircularView = CircularView,
              IntervalView = IntervalView,
              AlignmentView = AlignmentView,
              SeqView = SeqView,
              StackedView = StackedView,
              TxdbView = TxdbView,
              TracksView = TracksView))
}


