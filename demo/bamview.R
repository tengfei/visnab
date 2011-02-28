require(visnab)
data(bamtest)
library(IRanges)
visenv$themes[["default"]][["bg.col"]] <- "white"
visplot(bamtest,lower=1)
