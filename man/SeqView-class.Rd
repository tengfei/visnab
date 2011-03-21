\name{SeqView-class}
\Rdversion{1.1}
\docType{class}
\alias{SeqView-class}
\alias{print,SeqView-method}

\title{Class \code{"SeqView"}}
\description{
  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SeqView", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{track}:}{Object of class \code{"BSgenome"} ~~ }
    \item{\code{strand}:}{Object of class \code{"characterORNULL"} ~~ }
    \item{\code{pars}:}{Object of class \code{"environment"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{GraphicPars}"}, directly.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "SeqView")}: ... }
	 }
}

\author{
  Tengfei Yin
}



\examples{
showClass("SeqView")
}
\keyword{classes}
