\name{crosstab}
\title{Cross table of labels}
\alias{crosstab}

\description{
Prints a cross table (contingency table) for 1-3 labels.
}

\usage{
crosstab(plot = TRUE)
}

\arguments{
  \item{plot}{logical; should be also a barplot plotted?}
}

\details{
This command prints a frequency distribution (for a single label) or a contingency 
table (for 2-3 labels) useful for inspection of the data structure. Optionally a 
barplot is plotted (for 1-2 labels).

Just press Enter (enter an empty field), when the desired number of variables is 
reached.
}



\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\value{
  \item{results}{the frequency/contingency table}
}

\keyword{misc}
\concept{Menu: GCDkit}
\concept{Print}
