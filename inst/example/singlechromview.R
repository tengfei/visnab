library(MutableRanges)
library(qtbase)
library(qtpaint)
library(scales)
library(visnab)
## how to generate those ideograms in data
chrmodel <- paste("chr",c(1:22,"X","Y"),sep="")
save(hg19Ideogram, file="~/Codes/gitrepos/visnab/data/hg19Ideogram.rda")
gr <- getIdeogram("hg19",subchr=chrmodel,cytobands=TRUE)
hg19IdeogramCyto <- gr
save(hg19IdeogramCyto, file="~/Codes/gitrepos/visnab/data/hg19IdeogramCyto.rda")
## prepare for demo
data(hg19IdeogramCyto)
obj <- SingleChromView(hg19IdeogramCyto)
print(obj)

## TODO:
## viewranges?
