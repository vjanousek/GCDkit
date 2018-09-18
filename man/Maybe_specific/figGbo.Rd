\name{figGbo}
\title{Defining groups on Figaro-compatible plots}
\alias{figGbo}


\description{
Interactive definition of groups on any Figaro-compatible plot. 
}

\usage{
figGbo(x.tol = 0, y.tol = 0, max.points = 100, max.polygons = 25)
}

\arguments{
\item{x.tol, y.tol}{tolerance for the automatic closing of polygons.}
\item{max.points}{maximum number of vertices for a single polygon.}
\item{max.polygons}{maximum number of polygons.}
}

\details{
Each of the groups is defined by clicking vertices of a polygon with the 
corresponding data points. The polygons are closed automatically. To finish, 
right click anywhere on the plot and select '\code{Stop}'. The groups are numbered 
consecutively, points falling into two or more fields form extra groups, as do 
unclassified samples.
}

\seealso{
    '\code{\link{figaro}}'
}

\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
\concept{Grouping}
\concept{Classification}

\author{
    Vojtech Erban, \email{vojtech.erban@geology.cz} and Vojtech Janousek, 
    \email{vojtech.janousek@geology.cz}
}
