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
    sapply(c(l.col,l.char,l.range,l.enum,l.int), function(i) {
      i$setDefault()
    })
  })

  blyt <- Qt$QHBoxLayout()
  blyt$insertStretch(0,1)
  blyt$addWidget(reset)
  #blyt$addWidget(submit)

  lyt <- Qt$QVBoxLayout()

  # best way to check for a particular class
  #sapply(pars$output()$value, function(i) is(i,"SingleEnum"))

  # color widgets
  this$l.col <- list()

  sapply(gp$output()$pars[gp$output()$exposed &
                          (gp$output()$class == "QColor")], function(i) {
    l.col[[i]] <<- ColorParWidget(gp, i)
    lyt$addWidget(l.col[[i]])
  })

  # character widgets
  this$l.char <- list()

  sapply(gp$output()$pars[gp$output()$exposed &
                          (gp$output()$class == "character")], function(i) {
    l.char[[i]] <<- CharParWidget(gp, i)
    lyt$addWidget(l.char[[i]])
  })

  
  # numeric with range widgets
  this$l.range <- list()

  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                         "NumericWithRange")], function(i) {
    l.range[[i]] <<- RangeParWidget(gp, i, "double")
    lyt$addWidget(l.range[[i]])
  })

  # integer with range widgets
  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                         "IntegerWithRange")], function(i) {
    l.range[[i]] <<- RangeParWidget(gp, i, "int")
    lyt$addWidget(l.range[[i]])
  })  

  # integer widgets
  this$l.int <- list()

  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class %in%
    c("PositiveInteger","NonnegativeInteger","NegativeInteger",
      "NonpositiveInteger"))], function(i) {
    l.int[[i]] <<- IntParWidget(gp, i, substr(pars$output()$class[i],1,6))
    lyt$addWidget(l.int[[i]])
  })  
  
  # single enum widgets
  this$l.enum <- list()

  sapply(gp$output()$pars[gp$output()$exposed & 
          (sapply(pars$output()$value, function(i) is(i,"SingleEnum")))],
    function(i) {
      l.enum[[i]] <<- SingleEnumParWidget(gp, i)
      lyt$addWidget(l.enum[[i]])
  })  

  lyt$addLayout(blyt)

  setLayout(lyt)
})

qsetMethod("setValue", ControlPanel, function(par, val) {
  c(l.col,l.char,l.range,l.int,l.enum)[[par]]$setValue(val)
})

# widget to handle changing colors
qsetClass("ColorParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initColor <- eval(parse(text=paste("gp$",par,"$name()",sep="")))

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
    eval(parse(text=paste("values(gp$",par,") <- parEdit$text",sep="")))
  } else {
    parEdit$setText("")
    parLabel$setFocus(Qt$Qt$OtherFocusReason)
    parEdit$setPlaceholderText("Error: Invalid color entered")
  }
})

qsetMethod("setDefault", ColorParWidget, function() {
  clr <- eval(parse(text=paste("gp$",par,"$name()",sep="")))
  parSwatch$setStyleSheet(paste("background-color:",clr,sep=""))
  parEdit$setText(clr)  
})

# widget for changing numeric values
qsetClass("RangeParWidget", Qt$QWidget, function(gp, par, type,parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par; this$type <- type

  initVal <- eval(parse(text=paste("gp$",par,sep="")))
  this$minVal <- eval(parse(text=paste("gp$",par,"@min",sep="")))
  this$maxVal <- eval(parse(text=paste("gp$",par,"@max",sep="")))

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  if(type == "double") {
    this$spin <- Qt$QDoubleSpinBox()
    spin$setDecimals(3)
    spin$setSingleStep((maxVal - minVal)/100)
  } else {
    this$spin <- Qt$QSpinBox()
  }
  spin$setRange(minVal, maxVal)
  spin$setValue(initVal)

  # slider -- only supports integers, so need to adjust values
  this$sl <- Qt$QSlider(Qt$Qt$Horizontal)
  if(type == "double") {
    sl$setRange(0,100)
    sl$setValue(as.integer(100*initVal))
  } else {
    sl$setRange(minVal,maxVal)
    sl$setValue(initVal)
  }

  # update slider when spinbox changes
  qconnect(spin, "valueChanged", function(val) {
    if(type == "double") {
      sl$setValue(as.integer(100*val))
    } else {
      sl$setValue(val)
    }
  })
  # update spinbox when slider changes, and update the gp
  qconnect(sl, "valueChanged", function(val) {
    if(type == "double") {
      spin$setValue(val/100)
    } else {
      spin$setValue(val)
    }
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
qsetClass("SingleEnumParWidget", Qt$QWidget, function(gp, par, parent = NULL)
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

qsetMethod("getPar", SingleEnumParWidget, function() {
  par
})

qsetMethod("getValue", SingleEnumParWidget, function() {
  dropList$currentText
})

qsetMethod("setValue", SingleEnumParWidget, function(val) {
  if(val %in% levels) dropList$setCurrentIndex(which(levels == val) - 1)
})

qsetMethod("setDefault", SingleEnumParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  dropList$setCurrentIndex(which(levels == val) - 1)
})



# widget for changing integer values
qsetClass("IntParWidget", Qt$QWidget, function(gp, par, type, parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par; this$type <- type

  initVal <- eval(parse(text=paste("gp$",par,sep="")))

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$spin <- Qt$QSpinBox()
  if(type == "Negati") {
    spin$setMinimum(-999999)
    spin$setMaximum(-1)
  } else if(type == "Positi") {
    spin$setMinimum(1)
    spin$setMaximum(999999)
  } else if(type == "Nonneg") {
    spin$setMinimum(0)
    spin$setMaximum(999999)
  } else {
    spin$setMinimum(-999999)
    spin$setMaximum(0)
  }
  spin$setValue(initVal)

  # update gp when spinbox changes
  qconnect(spin, "valueChanged", function(val) {
    eval(parse(text=paste("values(gp$",par,") <- spin$value",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(spin)

  setLayout(lyt)
})

qsetMethod("getPar", IntParWidget, function() {
  par
})

qsetMethod("getValue", IntParWidget, function() {
  spin$value
})

qsetMethod("setValue", IntParWidget, function(val) {
  spin$setValue(val)
})

qsetMethod("setDefault", IntParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  spin$setValue(val)
})



# widget to handle changing character strings
qsetClass("CharParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initText <- eval(parse(text=paste("gp$",par,sep="")))

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  parLabel$setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$parEdit <- Qt$QLineEdit(initText)

  qconnect(parEdit, "editingFinished", function() {
    setValue(parEdit$text)
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(parEdit)

  setLayout(lyt)
})

qsetMethod("getPar", CharParWidget, function() {
  par
})

qsetMethod("getValue", CharParWidget, function() {
  parEdit$text
})

# also updates the gp object
qsetMethod("setValue", CharParWidget, function(txt) {
    parEdit$setText(txt)
    eval(parse(text=paste("gp$",par," <- parEdit$text",sep="")))
})

qsetMethod("setDefault", CharParWidget, function() {
  txt <- eval(parse(text=paste("gp$",par,sep="")))
  parEdit$setText(txt)  
})
