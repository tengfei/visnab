require(visnab)
## With no specific name, a menu containing all the current genomes
## in UCSCGenome browsers will be prompted.
obj <- getIdeogram()
## give the number which indicate the genome you want to get
obj
## Specify an "official" name if you know it
obj <- getIdeogram(species="mm9")
obj
## provide a subset 
subchrs <- paste("chr",1:8,sep="")
## get it
obj <- getIdeogram(species="mm9",subchr=subchrs)
obj
