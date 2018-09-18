\name{plotDiagram}
\alias{plotDiagram}

\title{Plot Classification Diagram}
\description{
  Plots the classification diagram, stored in the Figaro style sheet. 
}

\usage{
    plotDiagram(diagram,select.samples=TRUE,new=TRUE,main=NULL,width=6.5,height=6.5,pointsize=10,bg="transparent",interactive=FALSE,...)
}

\arguments{
  \item{diagram}{a valid name of the classification diagram. See details.}
  \item{main}{optional alternative main title to the diagram.}
  \item{select.samples}{logical; should be invoked the dialogue for selecting samples?}
  \item{new}{logical; should be opened a new plotting window?}
  \item{width, height}{dimensions of the plotting window in inches.}
  \item{pointsize}{the default pointsize of plotted text, interpreted as big points (1/72 inch).}
  \item{bg}{plotting window background colour.}
  \item{interactive}{logical; for internal use by our French colleagues.}
  \item{...}{optional parameters to the plotting function}
}

\details{
The argument '\code{diagram}' may acquire one of following values:

\preformatted{
    
'AFM', 'PeceTaylor', 'Shand', 'TAS', 'CoxVolc', 

'TASMiddlemostVolc', 'Jensen', 'LarocheVolc', 

'WinFloyd1', 'WinFloyd2', 'CoxPlut', 

'TASMiddlemostPlut', 'LarochePlut', 'DebonPQ', 'DebonBA', 

'MiddlemostPlut', 'QAPFPlut',

'OConnorPlut', 'QAPFVolc','OConnorVolc',

'Miyashiro', 'Hastie', 'Pearce1996', 'Villaseca', 'NaAlK', 

'Harris', 'Sylvester', 'Meschede', 'Mullen', 'Norry', 

'Shervais', 'PearceEtAl'.}

In case some analyses plot outside the limit, a warning is shown.
}

\value{
none
}

\examples{
    plotDiagram("TAS")
    plotDiagram("LarochePlut",FALSE)
}

\keyword{hplot}
\keyword{internal}
\concept{Plot}
\concept{Classification}

\seealso{
\code{\link{classify}}, \code{\link{figaro}}, 
\code{\link{AFM}}, 
\code{\link{Miyashiro}}, 
\code{\link{PeceTaylor}},
\code{\link{Hastie}},  
\code{\link{NaAlK}}, 
\code{\link{Shand}},
\code{\link{TAS}},
\code{\link{Cox}},
\code{\link{TASMiddlemost}},  
\code{\link{Jensen}}, 
\code{\link{Laroche}}, 
\code{\link{WinFloyd1}}, 
\code{\link{WinFloyd2}}, 
\code{\link{DebonPQ}}, 
\code{\link{DebonBA}}, 
\code{\link{Middlemost}},
\code{\link{QAPF}},
\code{\link{OConnor}},
\code{\link{Pearce1996}},
\code{\link{Villaseca}}.
}

\author{
    Vojtech Erban, \email{vojtech.erban@geology.cz}
}
