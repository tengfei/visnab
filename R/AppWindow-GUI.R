if(FALSE) {

source("~/gene/visnabGUI/SearchBar.R")

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

load("~/gene/genesymbol.rda")
visnabGUI <- function() {
  w <- Qt$QMainWindow()
  w$setMinimumSize(800,600)
  w$setWindowTitle("Visnab GUI")

  # List of Genomic Ranges
  grList <- GenomicRangesList()

  ### Actions ###
  exitAction <- Qt$QAction("Exit", w)
  qconnect(exitAction, "triggered", w$hide)
  exitAction$setStatusTip("Exit Program")

  loadWSAction <- Qt$QAction("Load from workspace", w)
  loadWSAction$setStatusTip("Load a dataset from your R workspace")

  loadFileAction <- Qt$QAction("Load from file", w)
  qconnect(loadFileAction, "triggered", function() {
    nameFilter <- paste("BAM files (*.bam *.bai)",
                 "vCard files (*.vcf)",
                 "All files (*.*)", 
                 sep=";;")

    filename <- Qt$QFileDialog$getOpenFileName(NULL, "Open File...",
                                               getwd(),nameFilter)
    print(filename)
  })
  loadFileAction$setStatusTip("Load a dataset from a file on your local machine")

  loadURLAction <- Qt$QAction("Load from URL", w)
  loadURLAction$setStatusTip("Load a dataset from a website (requires internet connection)")

  ### Menubar ###
  menubar <- Qt$QMenuBar()
  w$setMenuBar(menubar)
  
  fileMenu <- Qt$QMenu("File")
  menubar$addMenu(fileMenu)

  fileMenu$addAction(loadWSAction)
  fileMenu$addAction(loadFileAction)
  fileMenu$addAction(loadURLAction)
  fileMenu$addSeparator()
  fileMenu$addAction(exitAction)

  ### Toolbar ###
  searchWid <- Qt$QWidget()
  sb <- SearchBar(grList,genesymbol)
  searchSubmit <- Qt$QPushButton("Search")
  
  qconnect(searchSubmit, "clicked", function() {
    sb$parseSearchString(sb$text)
  })

  searchLyt <- Qt$QHBoxLayout()
  searchLyt$addWidget(sb)
  searchLyt$addWidget(searchSubmit)
  searchWid$setLayout(searchLyt)

  toolbar <- Qt$QToolBar()
  w$addToolBar(toolbar)

  toolbar$addWidget(searchWid)
  
  ### Status bar ###
  statusbar <- Qt$QStatusBar()
  w$setStatusBar(statusbar)

  ### Data Viewer ###
  dview <- DataViewer()

  # update table when search range updates
  qconnect(sb, "rangeChanged", function() {
    ret <- sb$getSearchRange()
    if(!is.null(ret)) {
      dview$filterTable(ret)
    }
  })

  ### Make window visible ###
  w$show()
}

# could potentially use for getting name of an R object from user (would
# have used simple convenience function provided by QInputDialog$getText,
# but wanted to include a completer for the line edit. It might also make
# sense to use a list instead of a text entry widget... I think we would
# only accept GRanges objects from the workspace, but I really don't know
# at this point
qsetClass("WSDialog", Qt$QDialog, function(parent = NULL) {
  super(parent)
  setWindowTitle("Load file from R Workspace")

  this$le <- Qt$QLineEdit()
  this$comp <- Qt$QCompleter(ls(1))
  le$setCompleter(comp)

  editLyt <- Qt$QHBoxLayout()
  editLyt$addWidget(Qt$QLabel("Object Name:"))
  editLyt$addWidget(le)
  
  buttonBox <- Qt$QDialogButtonBox(Qt$QMessageBox$Cancel |
                                   Qt$QMessageBox$Ok)

  qconnect(buttonBox, "accepted", accept)
  qconnect(buttonBox, "rejected", reject)

  lyt <- Qt$QVBoxLayout()
  lyt$addLayout(editLyt)
  lyt$addWidget(buttonBox)
  setLayout(lyt)
})



obj <- IModeGroup()
if(length(obj$id()) < 1) obj$setId(obj$defaultId)

w <- Qt$QMainWindow()
mb <- Qt$QMenuBar()
w$setMenuBar(mb)

# create actions for each item in group
m <- Qt$QMenu(obj$text)

actionList <- list()
length(actionList) <- length(obj$items)
actionGroup <- Qt$QActionGroup(w)
actionGroup$setExclusive(obj$exclusive)

# put default ID in list first, then add separator
dID <- obj$defaultId
actionList[[dID]] <- Qt$QAction(obj$items[[dID]]$text, actionGroup)
actionList[[dID]]$setCheckable(TRUE)
actionList[[dID]]$setChecked(TRUE)
qconnect(actionList[[dID]], "triggered", function() obj$setId(dID))
actionGroup$addAction(actionList[[dID]])
m$addAction(actionList[[dID]])
m$addSeparator()

# add control panel for default ID to window
cp <- Qt$QDockWidget()
cp$setWidget(ModeControlPanel(obj$items[[dID]]$pars))
w$addDockWidget(Qt$Qt$LeftDockWidgetArea, cp)

sapply((1:length(obj$items))[-dID], function(i) {
  actionList[[i]] <<- Qt$QAction(obj$items[[i]]$text, actionGroup)
  actionList[[i]]$setCheckable(TRUE)
  qconnect(actionList[[i]], "triggered", function() obj$setId(i))
  actionGroup$addAction(actionList[[i]])
  m$addAction(actionList[[i]])
})

mb$addMenu(m)

# handler to change the control panel when id changes
qconnect(actionGroup, "triggered", function() {
  # ???
})

}
