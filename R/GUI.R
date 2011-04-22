
############################################################
## CircularView GUI
############################################################

##------------------------------------------------------------
## sliders for Cicurlar view
##------------------------------------------------------------
setGeneric("visnabGUI",function(obj,...) standardGeneric("visnabGUI"))

setMethod("visnabGUI","CircularView",function(obj,...){

qsetClass("SlidersGroup",Qt$QGroupBox,function(orientation,title,obj,parent=NULL){
  super(title,parent)
  ## add three types of sliders
  this$slider <- Qt$QSlider(orientation)
  slider$setFocusPolicy(Qt$Qt$StrongFocus)
  slider$setTickPosition(Qt$QSlider$TicksBothSides)
  slider$setTickInterval(10)
  slider$setSingleStep(1)

  this$scrollBar <- Qt$QScrollBar(orientation)
  scrollBar$setFocusPolicy(Qt$Qt$StrongFocus)

  this$dial <- Qt$QDial()
  dial$setFocusPolicy(Qt$StrongFocus)
  ## chain state together
  qconnect(slider,"valueChanged",scrollBar$setValue)
  qconnect(scrollBar,"valueChanged",dial$setValue)
  qconnect(dial,"valueChanged",slider$setValue)
  ## qconnect(dial,"valueChanged",valueChanged)
  qconnect(dial,"valueChanged",function(vals){
    vals <- vals/100
    cols <- alpha("blue",vals)
    values(obj$tracks[[1]])$.color <- cols
  })

  if(orientation == Qt$Qt$Horizontal)
    direction <- Qt$QBoxLayout$TopToBottom
  else
    direction <- Qt$QBoxLayout$LeftToRight

  slidersLayout <- Qt$QBoxLayout(direction)
  slidersLayout$addWidget(slider)
  slidersLayout$addWidget(scrollBar)
  slidersLayout$addWidget(dial)
  setLayout(slidersLayout)
})

qsetMethod("setValue",SlidersGroup,function(value){
  ## initiate the cascade
  slider$setValue(value)
})

qsetMethod("setMinimum", SlidersGroup, function(value) {
  slider$setMinimum(value)
  scrollBar$setMinimum(value)
  dial$setMinimum(value)
})

qsetMethod("setMaximum", SlidersGroup, function(value) {
  slider$setMaximum(value)
  scrollBar$setMaximum(value)
  dial$setMaximum(value)
})

qsetSignal("valueChanged(int value)",SlidersGroup)

qsetClass("CircularViewWindow",Qt$QWidget,function(obj){
  obj$show()
  this$verticalSliders <- SlidersGroup(Qt$Qt$Vertical,"Vertical",obj)
  verticalSliders$setMinimum(0)
  verticalSliders$setMaximum(100)
  verticalSliders$setGeometry(0,0,700,100)
  ## layout <- Qt$QVBoxLayout()
  layout <- Qt$QGridLayout()
  layout$addWidget(obj$view,0,0)
  layout$addWidget(verticalSliders,1,0)
  layout$setRowStretch(0,1)
  layout$setRowStretch(1,0)
  setLayout(layout)
  setGeometry(0,0,800,900)
})
CircularViewWindow(obj)$show()

})
