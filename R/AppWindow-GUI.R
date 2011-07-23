qsetClass("DataViewer", Qt$QTableView, function(grl, parent = NULL)
{
  super(parent)

  #this$setMinimumSize(550,350)
  
  this$sortingEnabled <- TRUE

})

# Going to need to rework this function now that the data will be stored
# in a Genomic Ranges List. Will also need to figure out what kinds of
# computations/filtering I will do, and what tools in the MutableRanges
# package will do.
qsetMethod("filterTable", DataViewer, function(grInterval) {
  grFilter <- subsetByOverlaps(gr, grInterval)

  filterModel <- qdataFrameModel(as.data.frame(grFilter))
  proxy$setSourceModel(filterModel)
  tv$setModel(proxy)
  
})

# could potentially use for getting name of an R object from user (would
# have used simple convenience function provided by QInputDialog$getText,
# but wanted to include a completer for the line edit. It might also make
# sense to use a list instead of a text entry widget... I think we would
# only accept GRanges objects from the workspace, but I really don't know
# at this point
## qsetClass("WSDialog", Qt$QDialog, function(parent = NULL) {
##   super(parent)
##   setWindowTitle("Load file from R Workspace")

##   this$le <- Qt$QLineEdit()
##   this$comp <- Qt$QCompleter(ls(1))
##   le$setCompleter(comp)

##   editLyt <- Qt$QHBoxLayout()
##   editLyt$addWidget(Qt$QLabel("Object Name:"))
##   editLyt$addWidget(le)
  
##   buttonBox <- Qt$QDialogButtonBox(Qt$QMessageBox$Cancel |
##                                    Qt$QMessageBox$Ok)

##   qconnect(buttonBox, "accepted", accept)
##   qconnect(buttonBox, "rejected", reject)

##   lyt <- Qt$QVBoxLayout()
##   lyt$addLayout(editLyt)
##   lyt$addWidget(buttonBox)
##   setLayout(lyt)
## })


