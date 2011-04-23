
############################################################
## CircularView GUI
############################################################

##------------------------------------------------------------
## sliders for Cicurlar view
##------------------------------------------------------------

setGeneric("visnabGUI",function(obj,...) standardGeneric("visnabGUI"))

setMethod("visnabGUI","CircularView",function(obj,...){
  orientation <- Qt$Qt$Horizontal
  slider <- Qt$QSlider(orientation)
  slider$setFocusPolicy(Qt$Qt$StrongFocus)
  slider$setTickPosition(Qt$QSlider$TicksBothSides)
  slider$setTickInterval(10)
  slider$setSingleStep(1)

  scrollBar <- Qt$QScrollBar(orientation)
  scrollBar$setFocusPolicy(Qt$Qt$StrongFocus)

  dial <- Qt$QDial()
  dial$setFocusPolicy(Qt$StrongFocus)
  ## chain state together
  qconnect(slider,"valueChanged",scrollBar$setValue)
  qconnect(scrollBar,"valueChanged",dial$setValue)
  ## qconnect(dial,"valueChanged",slider$setValue)
  ## qconnect(dial,"valueChanged",valueChanged)
  qconnect(dial,"valueChanged",function(vals){
    slider$setValue(vals)
    vals <- vals/100
    cols <- alpha("blue",vals)
    values(obj$tracks[[1]])$.color <- cols
  })
  
  if(orientation == Qt$Qt$Horizontal){
    direction <- Qt$QBoxLayout$TopToBottom
  }else{
    direction <- Qt$QBoxLayout$LeftToRight
  }
  slidersLayout <- Qt$QBoxLayout(direction)
  slidersLayout$addWidget(slider)
  slidersLayout$addWidget(scrollBar)
  slidersLayout$addWidget(dial)
  slidersBox <- Qt$QGroupBox()
  slidersBox$setLayout(slidersLayout)  
  slidersBox$setGeometry(0,0,700,100)
  slider$setMinimum(0)
  scrollBar$setMinimum(0)
  dial$setMinimum(0)
  slider$setMaximum(100)
  scrollBar$setMaximum(100)
  dial$setMaximum(100)
  ## verticalSliders$setMaximum(100)
  slider$setValue(100)
  ## verticalSliders$setValue(100)
  ## layout <- Qt$QVBoxLayout()
  layout <- Qt$QGridLayout()
  layout$addWidget(obj$view,0,0)
  ## layout$addWidget(verticalSliders,1,0)
  layout$addWidget(slidersBox,1,0)
  layout$setRowStretch(0,1)
  layout$setRowStretch(1,0)
  grandWidget <- Qt$QWidget()
  grandWidget$setLayout(layout)
  grandWidget$setGeometry(0,0,800,900)
  ## obj$show()
  grandWidget$show()
})
