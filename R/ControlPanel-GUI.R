# pass in GraphicPars() ??
# should this inherit from QDialog?
# submit button not currently necessary due to automatic updating
qsetClass("ControlPanel", Qt$QWidget, function(gp, parent = NULL) {
  super(parent)

  #this$submit <- Qt$QPushButton("Submit")
  this$reset <- Qt$QPushButton("Reset to Defaults")

  #qconnect(submit, "clicked", function() {
  #  sapply((l.col), function(i) {
  #    eval(parse(text=paste("gp$",i$getPar()," <- i$getValue()",sep="")))
  #  })
  #
  #  sapply(c(l.range,l.enum), function(i) {
  #    eval(parse(text=paste("values(gp$",i$getPar(),") <- i$getValue()",
  #                 sep="")))
  #  })
  #})

  qconnect(reset, "clicked", function() {
    gp$reset()
    sapply(c(l.col,l.range,l.enum), function(i) {
      i$setDefault()
    })
  })

  blyt <- Qt$QHBoxLayout()
  blyt$insertStretch(0,1)
  blyt$addWidget(reset)
  #blyt$addWidget(submit)

  lyt <- Qt$QVBoxLayout()

  this$l.col <- list()

  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                                 "character")], function(i) {
    l.col[[i]] <<- ColorParWidget(gp, i)
    lyt$addWidget(l.col[[i]])
  })

  this$l.range <- list()

  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                         "NumericWithRange")], function(i) {
    l.range[[i]] <<- RangeParWidget(gp, i)
    lyt$addWidget(l.range[[i]])
  })

  this$l.enum <- list()

  sapply(gp$output()$pars[gp$output()$exposed & (grepl("Enum",
                                         gp$output()$class))], function(i) {
    l.enum[[i]] <<- EnumParWidget(gp, i)
    lyt$addWidget(l.enum[[i]])
  })  

  lyt$addLayout(blyt)

  setLayout(lyt)
})

qsetMethod("setValue", ControlPanel, function(par, val) {
  c(l.col,l.range,l.enum)[[par]]$setValue(val)
})

# widget to handle changing colors
qsetClass("ColorParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initColor <- eval(parse(text=paste("gp$",par,sep="")))

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])
  this$parSwatch <- Qt$QPushButton()
  parSwatch$setAutoFillBackground(TRUE)
  parSwatch$setFocusPolicy(Qt$Qt$NoFocus)
  parSwatch$setStyleSheet(paste("background-color:",initColor,sep=""))
  this$parEdit <- Qt$QLineEdit(initColor)
  this$col <- Qt$QColorDialog()

  qconnect(parSwatch, "clicked", function() {
    col$show()
  })

  qconnect(col, "accepted", function() {
    #parSwatch$setStyleSheet(paste("background-color:",
    #  col$currentColor$name(),sep=""))
    #parEdit$setText(col$currentColor$name())
    setValue(col$currentColor$name())
  })

  qconnect(parEdit, "editingFinished", function() {
    setValue(parEdit$text)
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(parSwatch)
  lyt$addWidget(parEdit)

  setLayout(lyt)
})

qsetMethod("getPar", ColorParWidget, function() {
  par
})

qsetMethod("getValue", ColorParWidget, function() {
  parEdit$text
})

# also updates the gp object with new color
qsetMethod("setValue", ColorParWidget, function(clr) {
  if(Qt$QColor$isValidColor(clr)) {
    parSwatch$setStyleSheet(paste("background-color:",clr,sep=""))
    parEdit$setText(clr)
    eval(parse(text=paste("gp$",par," <- parEdit$text",sep="")))
  } else {
    parEdit$setText("")
    parLabel$setFocus(Qt$Qt$OtherFocusReason)
    parEdit$setPlaceholderText("Error: Invalid color entered")
  }
})

qsetMethod("setDefault", ColorParWidget, function() {
  clr <- eval(parse(text=paste("gp$",par,sep="")))
  parSwatch$setStyleSheet(paste("background-color:",clr,sep=""))
  parEdit$setText(clr)  
})

# widget for changing numeric values
qsetClass("RangeParWidget", Qt$QWidget, function(gp, par, parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par

  initVal <- eval(parse(text=paste("gp$",par,sep="")))
  this$minVal <- eval(parse(text=paste("gp$",par,"@min",sep="")))
  this$maxVal <- eval(parse(text=paste("gp$",par,"@max",sep="")))

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$spin <- Qt$QDoubleSpinBox()
  spin$setDecimals(3)
  spin$setRange(minVal, maxVal)
  spin$setSingleStep((maxVal - minVal)/100)
  spin$setValue(initVal)

  # slider -- only supports integers, so need to adjust values
  this$sl <- Qt$QSlider(Qt$Qt$Horizontal)
  sl$setMinimum(0)
  sl$setMaximum(100)
  sl$setSingleStep(1)
  sl$setValue(as.integer(100*initVal))

  # update slider when spinbox changes
  qconnect(spin, "valueChanged", function(val) {
    sl$setValue(as.integer(100*val))
  })
  # update spinbox when slider changes, and update the gp
  qconnect(sl, "valueChanged", function(val) {
    spin$setValue(val/100)
    eval(parse(text=paste("values(gp$",par,") <- spin$value",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(spin)
  lyt$addWidget(sl)

  setLayout(lyt)
})

qsetMethod("getPar", RangeParWidget, function() {
  par
})

qsetMethod("getValue", RangeParWidget, function() {
  spin$value
})

qsetMethod("setValue", RangeParWidget, function(val) {
  spin$setValue(val)
})

qsetMethod("setDefault", RangeParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  spin$setValue(val)
})

# widget to change levels from a class extending Enum
qsetClass("EnumParWidget", Qt$QWidget, function(gp, par, parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par

  initLvl <- eval(parse(text=paste("gp$",par,sep="")))
  
  this$levels <- eval(parse(text=paste("levels(gp$",par,")",sep="")))
  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$dropList <- Qt$QComboBox()
  sapply(levels, dropList$addItem)
  dropList$setCurrentIndex(which(levels == initLvl) - 1)

  # change gp when user changes level
  qconnect(dropList, "currentIndexChanged", function(idx) {
    eval(parse(text=paste("values(gp$",par,
                 ") <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(dropList)

  setLayout(lyt)
})

qsetMethod("getPar", EnumParWidget, function() {
  par
})

qsetMethod("getValue", EnumParWidget, function() {
  dropList$currentText
})

qsetMethod("setValue", EnumParWidget, function(val) {
  if(val %in% levels) dropList$setCurrentIndex(which(levels == val) - 1)
})

qsetMethod("setDefault", EnumParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  dropList$setCurrentIndex(which(levels == val) - 1)
})
