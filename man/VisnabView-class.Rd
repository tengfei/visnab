\name{VisnabView-class}
\Rdversion{1.1}
\docType{class}
\alias{VisnabView-class}
\alias{addAttr,VisnabView-method}
\alias{seqnames<-,VisnabView-method}

\title{Class \code{"VisnabView"}}
\description{
  Top level view object.
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{show}:}{Object of class \code{"logical"}}
    \item{\code{seqnames}:}{Object of class \code{"characterORNULL"} ~~ }
    \item{\code{pars}:}{Object of class \code{"environment"} ~~ }
  }
}
%\section{Extends}{
%Class \code{"\linkS4class{GraphicPars}"}, directly.
%}
\section{Methods}{
  \describe{
    \item{addAttr}{\code{signature(obj = "VisnabView")}: ... }
    \item{seqnames<-}{\code{signature(x = "VisnabView")}: ... }
	 }
}

\author{
  Tengfei Yin
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%%\code{\linkS4class{GrahpicPars}}
%%}
\examples{
showClass("VisnabView")
}
\keyword{classes}
