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
  #    eval(parse(text=paste("gp$",i$getPar()," <- i$getValue()",
  #                 sep="")))
  #  })
  #})

  qconnect(reset, "clicked", function() {
    gp$reset()
    sapply(l.wid, function(i) {
      i$setDefault()
    })
  })

  blyt <- Qt$QHBoxLayout()
  blyt$insertStretch(0,1)
  blyt$addWidget(reset)
  #blyt$addWidget(submit)

  olyt <- Qt$QVBoxLayout()
  lyt <- Qt$QFormLayout()
  lyt$setRowWrapPolicy(Qt$QFormLayout$WrapLongRows)

  # best way to check for a particular class
  #sapply(pars$output()$value, function(i) is(i,"SingleEnum"))

  this$l.lab <- list()
  this$l.wid <- list()
  
  # color widgets
  sapply(gp$output()$pars[gp$output()$exposed &
                          (gp$output()$class == "Color")], function(i) {
    l.wid[[i]] <<- ColorParWidget(gp, i)
    l.lab[[i]] <<- ParLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  sapply(gp$output()$pars[gp$output()$exposed & 
          (sapply(gp$output()$value, function(i) is(i,"ColorEnum")))],
    function(i) {
      l.wid[[i]] <<- ColorEnumParWidget(gp, i)
      l.lab[[i]] <<- ParLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # character widgets
  sapply(gp$output()$pars[gp$output()$exposed &
                          (gp$output()$class == "character")], function(i) {
    l.wid[[i]] <<- CharParWidget(gp, i)
    l.lab[[i]] <<- ParLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # numeric with range widgets
  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                      "NumericWithMin0Max1")], function(i) {
    l.wid[[i]] <<- RangeParWidget(gp, i, "double")
    l.lab[[i]] <<- ParLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # integer with range widgets
  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class ==
                                         "IntegerWithRange")], function(i) {
    l.wid[[i]] <<- RangeParWidget(gp, i, "int")
    l.lab[[i]] <<- ParLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # integer widgets
  sapply(gp$output()$pars[gp$output()$exposed & (gp$output()$class %in%
    c("PositiveInteger","NonnegativeInteger","NegativeInteger",
      "NonpositiveInteger"))], function(i) {
    l.wid[[i]] <<- IntParWidget(gp, i, substr(gp$output()$class[i],1,6))
    l.lab[[i]] <<- ParLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  
  
  # single enum widgets (not color or glyph enum)
  sapply(gp$output()$pars[gp$output()$exposed & 
                          (sapply(gp$output()$value, function(i) is(i,"SingleEnum"))) &
                          (sapply(gp$output()$value, function(i) !is(i,"ColorEnum"))) &
                          (sapply(gp$output()$value, function(i) !is(i,"GlyphEnum")))],
         function(i) {
           l.wid[[i]] <<- SingleEnumParWidget(gp, i)
           l.lab[[i]] <<- ParLabel(gp, i)
           lyt$addRow(l.lab[[i]], l.wid[[i]])
         })  

  # multiple enum widgets
  sapply(gp$output()$pars[gp$output()$exposed & 
          (sapply(gp$output()$value, function(i) is(i,"MultipleEnum")))],
    function(i) {
      l.wid[[i]] <<- MultEnumParWidget(gp, i)
      l.lab[[i]] <<- ParLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # glyph enum widgets
  sapply(gp$output()$pars[gp$output()$exposed & 
          (sapply(gp$output()$value, function(i) is(i,"GlyphEnum")))],
    function(i) {
      l.wid[[i]] <<- GlyphEnumParWidget(gp, i)
      l.lab[[i]] <<- ParLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })
  

  
  olyt$addLayout(lyt)
  olyt$addLayout(blyt)

  setLayout(olyt)
})

qsetMethod("setValue", ControlPanel, function(par, val) {
  l.wid[[par]]$setValue(val)
})

# widget to handle changing colors
qsetClass("ColorParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initColor <- eval(parse(text=paste("gp$",par,sep="")))

  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])
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
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
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


# widget to handle changing colors
qsetClass("ColorEnumParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initColor <- eval(parse(text=paste("gp$",par,sep="")))

  this$colors <- eval(parse(text=paste("levels(gp$",par,")",sep="")))
  
  this$dropList <- Qt$QComboBox()
  sapply(colors, function(i) {
    pmap <- Qt$QPixmap(30,20)
    pmap$fill(Qt$QColor(i))
    icon <- Qt$QIcon(pmap)
    dropList$addItem(icon,i)
  })
  dropList$setCurrentIndex(which(colors == initColor) - 1)
  dropList$setIconSize(Qt$QSize(40,20))  

  # change gp when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("gp$",par,
                 " <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(dropList)
  
  setLayout(lyt)
})

qsetMethod("getPar", ColorEnumParWidget, function() {
  par
})

qsetMethod("getValue", ColorEnumParWidget, function() {
  dropList$currentText
})

qsetMethod("setValue", ColorEnumParWidget, function(val) {
  if(val %in% colors) dropList$setCurrentIndex(which(colors == val) - 1)
})

qsetMethod("setDefault", ColorEnumParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  dropList$setCurrentIndex(which(colors == val) - 1)
})



# widget to handle changing colors
qsetClass("GlyphEnumParWidget", Qt$QWidget, function(gp, par, parent = NULL) {
  super(parent)
  this$gp <- gp; this$par <- par

  initLvl <- eval(parse(text=paste("gp$",par,sep="")))

  this$levels <- eval(parse(text=paste("levels(gp$",par,")",sep="")))
  
  this$dropList <- Qt$QComboBox()
  icons <- eval(parse(text=paste("icons(gp$",par,")",sep="")))
  sapply(seq_along(levels), function(i) {
    dropList$addItem(icons[[i]],levels[i])
  })
  dropList$setCurrentIndex(which(levels == initLvl) - 1)
  dropList$setIconSize(Qt$QSize(40,20))  

  # change gp when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("gp$",par,
                 " <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(dropList)
  
  setLayout(lyt)
})

qsetMethod("getPar", GlyphEnumParWidget, function() {
  par
})

qsetMethod("getValue", GlyphEnumParWidget, function() {
  dropList$currentText
})

qsetMethod("setValue", GlyphEnumParWidget, function(val) {
  if(val %in% levels) dropList$setCurrentIndex(which(levels == val) - 1)
})

qsetMethod("setDefault", GlyphEnumParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  dropList$setCurrentIndex(which(levels == val) - 1)
})



# widget for changing numeric values (general for any numeric range, but
# for now hard-coded for a 0-1 range)
qsetClass("RangeParWidget", Qt$QWidget, function(gp, par, type,parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par; this$type <- type

  initVal <- eval(parse(text=paste("gp$",par,sep="")))
  #this$minVal <- eval(parse(text=paste("gp$",par,"@min",sep="")))
  this$minVal <- 0
  #this$maxVal <- eval(parse(text=paste("gp$",par,"@max",sep="")))
  this$maxVal <- 1
  
  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

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


  qconnect(spin, "valueChanged(double)", function(val) {
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
    eval(parse(text=paste("gp$",par," <- spin$value",sep="")))
  })
  
  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
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
  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$dropList <- Qt$QComboBox()
  sapply(levels, dropList$addItem)
  dropList$setCurrentIndex(which(levels == initLvl) - 1)

  # change gp when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("gp$",par,
                 " <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
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


# widget to change levels from a class extending Enum with ability to
# select multiple levels
qsetClass("MultEnumParWidget", Qt$QWidget, function(gp, par, parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par

  initVal <- eval(parse(text=paste("gp$",par,sep="")))
  
  this$levels <- eval(parse(text=paste("levels(gp$",par,")",sep="")))
  this$currentVal <- levels %in% initVal
  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  lyt <- Qt$QGridLayout()

  this$bg <- Qt$QButtonGroup()
  bg$setExclusive(FALSE)

  # initiate buttons, and check those that are within the current value
  sapply(seq_along(levels), function(i) {
    button <- Qt$QCheckBox(levels[i])
    lyt$addWidget(button, floor((i-1)/4), (i-1) %% 4)
    bg$addButton(button, i)
    bg$button(i)$setChecked(currentVal[i])
  })

  # change gp when user changes level
  qconnect(bg, "buttonClicked(int)", function(id) {
    currentVal[id] <<- bg$button(id)$checked
    eval(parse(text=paste("gp$",par," <- levels[currentVal]",
                   sep="")))
  })

  #lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  #lyt$addWidget(dropList)

  setLayout(lyt)
})

qsetMethod("getPar", MultEnumParWidget, function() {
  par
})

qsetMethod("getValue", MultEnumParWidget, function() {
  levels[currentVal]
})

qsetMethod("setValue", MultEnumParWidget, function(val) {
  if(all(val %in% levels)) {
    currentVal <- levels %in% val
    sapply(seq_along(levels), function(i) {
      bg$button(i)$setChecked(currentVal[i])
    })
    eval(parse(text=paste("gp$",par," <- levels[currentVal]",sep="")))
  } else {
    stop("Error: one or more levels specified are not valid")
  }
})

qsetMethod("setDefault", MultEnumParWidget, function() {
  val <- eval(parse(text=paste("gp$",par,sep="")))
  currentVal <- levels %in% val
  sapply(seq_along(levels), function(i) {
    bg$button(i)$setChecked(currentVal[i])
  })
})

# widget for changing integer values (obselete at this point since there
# are not parameters of this type)
qsetClass("IntParWidget", Qt$QWidget, function(gp, par, type, parent = NULL)
{
  super(parent)
  this$gp <- gp; this$par <- par; this$type <- type

  initVal <- eval(parse(text=paste("gp$",par,sep="")))

  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

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
  qconnect(spin, "valueChanged(int)", function(val) {
    eval(parse(text=paste("gp$",par," <- spin$value",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
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

  #parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  this$parEdit <- Qt$QLineEdit(initText)

  qconnect(parEdit, "editingFinished", function() {
    setValue(parEdit$text)
  })

  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
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

# label for a given parameter, with appropriate text and tooltip
qsetClass("ParLabel", Qt$QLabel, function(gp, par, parent = NULL) {
  super(parent)

  this$gp <- gp; this$par <- par

  parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  setText(paste(parInfo,":",sep=""))
  setToolTip(
    gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])
    
})




## for parameters class

# should this inherit from QDialog?
# submit button not currently necessary due to automatic updating
qsetClass("ParametersControlPanel", Qt$QWidget, function(gp, parent = NULL) {
  super(parent)

  #this$submit <- Qt$QPushButton("Submit")
  this$reset <- Qt$QPushButton("Reset to Defaults")

  ## qconnect(reset, "clicked", function() {
  ##   gp$reset()
  ##   sapply(l.wid, function(i) {
  ##     i$setDefault()
  ##   })
  ## })

  blyt <- Qt$QHBoxLayout()
  blyt$insertStretch(0,1)
  blyt$addWidget(reset)
  #blyt$addWidget(submit)

  olyt <- Qt$QVBoxLayout()
  lyt <- Qt$QFormLayout()
  lyt$setRowWrapPolicy(Qt$QFormLayout$WrapLongRows)

  # best way to check for a particular class
  #sapply(pars$output()$value, function(i) is(i,"SingleEnum"))

  this$l.lab <- list()
  this$l.wid <- list()

  # color widgets
  sapply(names(gp$parameters())[gp$parameters() == "Color"], function(i) {
    l.wid[[i]] <<- ColorParWidget(gp, i)
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  sapply(names(gp$parameters())[sapply(names(gp$parameters()),
                            function(i) is(gp$field(i),"ColorEnum"))],
    function(i) {
      l.wid[[i]] <<- ColorEnumParWidget(gp, i)
      l.lab[[i]] <<- ParsLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # character widgets
  sapply(names(gp$parameters())[gp$parameters() == "character"], function(i) {
    l.wid[[i]] <<- CharParWidget(gp, i)
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # numeric with range widgets
  sapply(names(gp$parameters())[gp$parameters() == "NumericWithMin0Max1"],
                                function(i) {
    l.wid[[i]] <<- RangeParWidget(gp, i, "double")
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # integer with range widgets
  sapply(names(gp$parameters())[gp$parameters() == "IntegerWithRange"],
         function(i) {
    l.wid[[i]] <<- RangeParWidget(gp, i, "int")
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # integer widgets
  sapply(names(gp$parameters())[gp$parameters() %in% c("PositiveInteger",
    "NonnegativeInteger","NegativeInteger","NonpositiveInteger")],
         function(i) {
    l.wid[[i]] <<- IntParWidget(gp, i, substr(gp$parameters()[i],1,6))
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  sapply(names(gp$parameters())[gp$parameters() == "AsIsOrnumeric"],
         function(i) {
    l.wid[[i]] <<- CharParWidget(gp, i)
    l.lab[[i]] <<- ParsLabel(gp, i)
    lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # single enum widgets (not color or glyph enum)
  sapply(names(gp$parameters())[(sapply(names(gp$parameters()),
                            function(i) is(gp$field(i),"SingleEnum"))) &
                                (sapply(names(gp$parameters()),
                            function(i) !is(gp$field(i),"ColorEnum"))) &
                                (sapply(names(gp$parameters()),
                            function(i) !is(gp$field(i),"GlyphEnum")))],
    function(i) {
      l.wid[[i]] <<- SingleEnumParWidget(gp, i)
      l.lab[[i]] <<- ParsLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })  

  # multiple enum widgets
  sapply(names(gp$parameters())[sapply(names(gp$parameters()),
                            function(i) is(gp$field(i),"MultipleEnum"))],
    function(i) {
      l.wid[[i]] <<- MultEnumParWidget(gp, i)
      l.lab[[i]] <<- ParsLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })

  # glyph enum widgets
  sapply(names(gp$parameters())[sapply(names(gp$parameters()),
                            function(i) is(gp$field(i),"GlyphEnum"))],
    function(i) {
      l.wid[[i]] <<- GlyphEnumParWidget(gp, i)
      l.lab[[i]] <<- ParsLabel(gp, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
  })
    

  
  olyt$addLayout(lyt)
  olyt$addLayout(blyt)

  setLayout(olyt)
})

qsetMethod("setValue", ParametersControlPanel, function(par, val) {
  l.wid[[par]]$setValue(val)
})

## # label for a given parameter, with appropriate text and tooltip
qsetClass("ParsLabel", Qt$QLabel, function(gp, par, parent = NULL) {
  super(parent)

  this$gp <- gp; this$par <- par

  ## # eventually, fix these so they display the text field and tooltip info
  ## parInfo <- gp$output()$parinfo[names(gp$output()$parinfo) == par]
  ## setText(paste(parInfo,":",sep=""))
  ## setToolTip(
  ##   gp$output()$tooltipinfo[names(gp$output()$tooltipinfo) == par])

  setText(paste(par,":",sep=""))
    
})





