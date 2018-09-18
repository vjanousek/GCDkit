\encoding{latin1}
\name{mins2deg}
\alias{mins2deg}

\title{
    Recasts degrees and minutes to degrees (with decimal places).
}

\description{
    The functions serves to recast strings in the form 48°53.6814' to decimal values in
    degrees. As separators are used the 'degree' sign (unicode + 00B0) and apostrophe, respectively.
    There are no spaces. 
}

\usage{
mins2deg(x="Easting",varname="XX")
}

\arguments{
  \item{x}{a text string, to be interpreted directly, or a colname of variable in 'labels'}
  \item{varname}{a name of a numeric variable in 'WR' to store the recalculated output}
}


\value{
    Returns, invisibly, the converted numbers. 
    Appends the recalculated coordinate to the data matrix 'WR'.
}

\author{
  Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}


\examples{
data(sazava)
accessVar("sazava")
WR<-WR[1:5,]
x<-c("48°53.6814'","48°53.6814'","48°53.6814'","48°53.6814'","48°53.6814'")
mins2deg(x,"E")
}

\keyword{hplot}
\concept{Plot}
\concept{GIS}

