\name{ID}
\alias{ID}
\title{Sample identification}

\description{
Identification/labelling of individual data points on plots.
}

\usage{
ID(x, y, labs = getOption("gcd.ident"), offset = 0.4, 
    col = "gray30", cex = 1)
}

\arguments{
  \item{x, y}{vector with x-y coordinates of the data points}
  \item{labs}{text to label  individual data points, see details}
  \item{offset}{distance (in char widths) between label and identified points.}
  \item{col}{colour of the text}
  \item{cex}{its size}
}

\details{
In GCDkit, the option '\code{ident}' determines whether the user wishes 
to identify data points  on binary and ternary plots. The default is zero, which 
means no identification.

If '\code{ident}' differs from zero, internal function '\code{ID}' can be invoked.
Its parameter \code{labs} is either a single number, or character vector.

In the former case, the variable '\code{labs}' contains either 1 
(identification by sample name), or the sequential number of the column 
in the data frame '\code{labels}' increased by one (identification by a user-
defined label).

Alternatively, a character vector \code{labs} can be used to specify the text 
directly.
}

\value{
None.
}

\seealso{
\code{\link{identify}}  \code{\link{gcdOptions}}  \code{\link{options}}
}

\examples{
getOption("ident")        # yields the current value of the given option
}


\keyword{iplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
