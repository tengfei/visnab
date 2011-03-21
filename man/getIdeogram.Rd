\name{getIdeogram}
\alias{getIdeogram}
\title{Get ideogram for model organism from UCSC genome browser}

\description{
  Engined by rtracklayer, get ideogram information for specific genome,
  and return a \link{GenomicRanges} object, contains chromosome information.
}

\usage{
getIdeogram(species=NULL, subchr=NULL)
}
\arguments{
  
  \item{species}{A character indicate an "official" name for species
  from UCSC genome, if use default value, will prompt a genome list, so
  you don't have to remember all the names.}

  \item{subchr}{A vector containing the chromosomes you want to
    subset. Since sometimes the returned object contains more
    chromosomes than you expected, it's not good for the following
    virsualization,you could subset chromosomes after you got all the
    information or you can pass a chromosome vector to subset it.}

}

\value{
  
  \item{gr}{A GenomicRanges object containing chromosome informations
    for specific genome}
  
}
% \details{
  
% }
% \references{
% }
% \seealso{
% }
\examples{
## With no specific name, a menu containing all the current genomes
## in UCSCGenome browsers will be prompted.
\dontrun{
obj <- getIdeogram()
## give the number which indicate the genome you want to get
obj
}
## Specify an "official" name if you know it
obj <- getIdeogram(species="mm9")
obj
## provide a subset 
subchrs <- paste("chr",1:8,sep="")
## get it
obj <- getIdeogram(species="mm9",subchr=subchrs)
obj
}
\author{Tengfei Yin}
% \keyword{}
