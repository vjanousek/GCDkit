\name{classify}
\title{Generic Classification Algorithm}
\alias{classify}

\description{
Classifies rocks using specified diagram.
}

\usage{
classify(diagram = NULL, grp = TRUE, labs = FALSE, 
source.sheet = TRUE, overlap = FALSE, X = x.data, 
Y = y.data, silent = FALSE, clas=sheet$d$t, ...) 
}

\arguments{
  \item{diagram}{name of diagram to be used, see details for more info}
  \item{grp}{logical: if TRUE, results are assigned to the variable 'groups'}
  \item{labs}{logical: if TRUE, yes/no dialogue for results assignment into the matrix 'labels' appears}
  \item{source.sheet}{logical: if TRUE, the sheet for \code{diagram} is newly assigned}
  \item{overlap}{logical: if TRUE, possible overlap between polygons of diagram is expected, and duplicate positive result for one sample is treated as polygon intersection}
  \item{X}{vector of values for abscissa}
  \item{Y}{vector of values for ordinate}
  \item{silent}{logical: if TRUE, informative outputs are reduced to minimum}
  \item{clas}{classification template to be used}
  \item{...}{any additional graphical parameters}
}

\details{
Function looks for the name of the polygon within the classification diagram,
into which falls the rock analysis represented by the coordinates \code{[x.data,y.data]}.


In some cases (TAS diagram, Winchester & Floyd's diagram) additional computations 
are performed. The argument '\code{diagram}' may acquire one of following values:

\preformatted{
    
'AFM', 'PeceTaylor', 'Shand', 'TAS', 'CoxPlut', 'CoxVolc', 

'Jensen', 'LarochePlut', 'LarocheVolc', 'WinFloyd1', 

'WinFloyd2','TASMiddlemostPlut', 'TASMiddlemostVolc', 

'DebonPQ', 'DebonBA', 'MiddlemostPlut', 'QAPFPlut', 

'QAPFVolc', 'OConnorPlut', 'QAPFVolc','OConnorVolc',

'Miyashiro', 'Hastie', 'Pearce1996', 'Villaseca', 'NaAlK'.}

The function is based on the sp package.
}

\value{
Vector of resulting rock names is stored in a variable 'results'. 
If '\code{grp = TRUE}' results are also assigned to the '\code{groups}' and 
'\code{grouping}' is set to -1 (as if called from the menu \code{'Data handling'}).
If rock projection falls on the boundary between two or more fields, rock names 
in question are merged together with comment 'boundary between ...'.
}

\author{
The sp package was written by Edzer Pebesma, Roger Bivand and others.

Vojtech Erban, \email{vojtech.erban@geology.cz}
}

\seealso{
\code{\link{plotDiagram}} 

\code{\link{.claslist}} 

\code{\link{figaro}}

\code{\link{AFM}},  
\code{\link{PeceTaylor}},  
\code{\link{Shand}}, 
\code{\link{NaAlK}},
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
\code{\link{OConnor}}
\code{\link{Miyashiro}}
\code{\link{Hastie}}
\code{\link{Pearce1996}}
\code{\link{Villaseca}}
}

\keyword{manip}
\concept{Grouping}
\concept{Classification}
\concept{Plot}
\concept{Figaro}
\concept{Subsetting}
