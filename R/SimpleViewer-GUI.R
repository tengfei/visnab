qsetClass("SimpleViewer", Qt$QWidget, function(view, gr = NULL, ref = NULL,
                                                parent = NULL, searchBar = TRUE)
{
  super(parent)

  this$setMinimumSize(550,350)
  this$setWindowTitle("Simple Genome Viewer")

  this$view <- view
  #this$tv <- Qt$QTableView()
  #this$proxy <- Qt$QSortFilterProxyModel()
  #tv$sortingEnabled <- TRUE

  this$sb <- SearchBar(gr, ref)
  this$submit <- Qt$QPushButton("Submit")

  # initiate parse when user clicks submit (will eventually trigger table
  # filtering)
  qconnect(submit, "clicked", function() {
    sb$parseSearchString(sb$text, gr, ref)
  })

  qconnect(sb, "rangeChanged", this$rangeChanged)

  # update table when search range updates
  #qconnect(sb, "rangeChanged", function() {
  #  ret <- sb$getSearchRange()
  #  if(!is.null(ret)) {
  #    filterTable(gr, ret)
  #  }
  #})
  
  sbLyt <- Qt$QHBoxLayout()
  sbLyt$addWidget(sb)
  sbLyt$addWidget(submit)

  windowLyt <- Qt$QVBoxLayout()
  windowLyt$addWidget(view)
  #windowLyt$addWidget(tv)
  if(searchBar)
    windowLyt$addLayout(sbLyt)
  setLayout(windowLyt)
  windowLyt
})

#qsetMethod("filterTable", GRangesViewer, function(gr, grInterval) {
#  grFilter <- subsetByOverlaps(gr, grInterval)
#
#  filterModel <- qdataFrameModel(as.data.frame(grFilter))
#  proxy$setSourceModel(filterModel)
#  tv$setModel(proxy)
#  
#})

qsetSignal("rangeChanged", SimpleViewer)

qsetMethod("getSearchRange", SimpleViewer, function() {
  sb$getSearchRange()
})

qsetMethod("setValue", SimpleViewer, function(val) {
  sb$setText(val)
})

