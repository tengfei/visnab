\name{QtVisnabView-class}
\Rdversion{1.1}
\docType{class}
\alias{QtVisnabView-class}

\title{Class \code{"QtVisnabView"}}
\description{
  QtVisnabView is a view object containing qt device, such as scene,view,layer.
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
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
Class \code{"\linkS4class{VisnabView}"}, directly.
Class \code{"\linkS4class{GraphicPars}"}, by class "VisnabView", distance 2.
}
\section{Methods}{
No methods defined with class "QtVisnabView" in the signature.
}

\author{
  Tengfei Yin
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\linkS4class{VisnabView}}
}
\examples{
showClass("QtVisnabView")
}
\keyword{classes}
