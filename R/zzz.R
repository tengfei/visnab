.onLoad <- function(libname,pkgname){
  bioc <- getOption("BioC")
  bioc$visnab <- .DefaultOpts()
  options(BioC = bioc)
}

