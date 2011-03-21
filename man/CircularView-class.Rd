\name{CircularView-class}
\Rdversion{1.1}
\docType{class}
\alias{CircularView-class}
\alias{print,CircularView-method}

\title{Class \code{"CircularView"}}
\description{
  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CircularView", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{tracks}:}{Object of class \code{"list"} ~~ }
    \item{\code{scene}:}{Object of class \code{"QGraphicsSceneORNULL"} ~~ }
    \item{\code{view}:}{Object of class \code{"Qanviz::PlotViewORNULL"} ~~ }
    \item{\code{rootLayer}:}{Object of class \code{"Qanviz::RLayerORNULL"} ~~ }
    \item{\code{row}:}{Object of class \code{"integer"} ~~ }
    \item{\code{col}:}{Object of class \code{"integer"} ~~ }
    \item{\code{rowSpan}:}{Object of class \code{"integer"} ~~ }
    \item{\code{colSpan}:}{Object of class \code{"integer"} ~~ }
    \item{\code{show}:}{Object of class \code{"logical"} ~~ }
    \item{\code{seqnames}:}{Object of class \code{"characterORNULL"} ~~ }
    \item{\code{pars}:}{Object of class \code{"environment"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{QtVisnabView}"}, directly.
Class \code{"\linkS4class{VisnabView}"}, by class "QtVisnabView", distance 2.
Class \code{"\linkS4class{GraphicPars}"}, by class "QtVisnabView", distance 3.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "CircularView")}: ... }
	 }
}

\author{
  Tengfei Yin
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
showClass("CircularView")
}
\keyword{classes}
