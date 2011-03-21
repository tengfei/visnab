\name{AlignmentView-class}
\Rdversion{1.1}
\docType{class}
\alias{AlignmentView-class}
\alias{print,AlignmentView-method}

\title{Class \code{"AlignmentView"}}
\description{
  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AlignmentView", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{track}:}{Object of class \code{"list"} ~~ }
    \item{\code{lower}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{cutbin}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{pars}:}{Object of class \code{"environment"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{GraphicPars}"}, directly.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "AlignmentView")}: ... }
	 }
}

\author{
  Tengfei Yin
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
showClass("AlignmentView")
}
\keyword{classes}
