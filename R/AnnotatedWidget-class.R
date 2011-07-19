AnnotatedWidget.gen <- setRefClass("AnnotatedWidget",
                                   fields = list(icon = "QIcon",
                                     text = "character"),
                                   methods = list(
                                     setIcon = function(value){
                                       icon <<- value
                                     },
                                     setText = function(value){
                                       text <<- value
                                     }))

