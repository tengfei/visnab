\name{GraphicPars-class}
\Rdversion{1.1}
\docType{class}
\alias{GraphicPars-class}
\alias{getPar,GraphicPars-method}
\alias{setPar,GraphicPars-method}
\alias{show,GraphicPars-method}

\title{Class \code{"GraphicPars"}}
\description{
  %%  ~~ A concise (1-5 lines) description of what the class is. ~~
  The GraphicPars class is an environment containing sets of pre-defined
  graphic attributes.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GraphicPars", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{pars}:}{Object of class \code{"environment"}, keep
      graphic attributes}
  }
}
\section{Methods}{
  \describe{
    \item{getPar}{\code{signature(obj = "GraphicPars")}: ... }
    \item{setPar}{\code{signature(obj = "GraphicPars")}: ... }
    \item{show}{\code{signature(object = "GraphicPars")}: ... }
	 }
}

\author{
Tengfei Yin
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
showClass("GraphicPars")
}
\keyword{classes}
