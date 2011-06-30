#library(qtbase)
#library(GenomicRanges)

#load("genesymbol.rda")

#gr1 <-
#  GRanges(seqnames =
#          Rle(c("chr1", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
#          ranges =
#          IRanges(1:10, end = 7:16, names = head(letters, 10)),
#          strand =
#          Rle(strand(c("-", "+", "*", "+", "-")),
#              c(1, 2, 2, 3, 2)),
#          score = 1:10,
#          GC = seq(1, 0, length=10))

qsetClass("GRangesViewer", Qt$QWidget, function(gr, ref = NULL, parent = NULL)
{
  super(parent)

  this$setMinimumSize(550,350)
  this$setWindowTitle("Simple Genome Viewer")

  #this$view <- WIDGET FROM TENGFEI
  this$tv <- Qt$QTableView()
  this$proxy <- Qt$QSortFilterProxyModel()
  tv$sortingEnabled <- TRUE


  this$sb <- SearchBar(gr, ref)
  this$submit <- Qt$QPushButton("Submit")

  # initiate parse when user clicks submit (will eventually trigger table
  # filtering)
  qconnect(submit, "clicked", function() {
    sb$parseSearchString(sb$text, gr, ref)
  })

  # update table when search range updates
  qconnect(sb, "rangeChanged", function() {
    ret <- sb$getSearchRange()
    if(!is.null(ret)) {
      filterTable(gr, ret)
    }
  })
  
  sbLyt <- Qt$QHBoxLayout()
  sbLyt$addWidget(sb)
  sbLyt$addWidget(submit)

  windowLyt <- Qt$QVBoxLayout()
  #windowLyt$addWidget(view)
  windowLyt$addWidget(tv)
  windowLyt$addLayout(sbLyt)
  setLayout(windowLyt)

})

qsetMethod("filterTable", GRangesViewer, function(gr, grInterval) {
  grFilter <- subsetByOverlaps(gr, grInterval)

  filterModel <- qdataFrameModel(as.data.frame(grFilter))
  proxy$setSourceModel(filterModel)
  tv$setModel(proxy)
  
})

