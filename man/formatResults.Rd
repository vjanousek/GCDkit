\name{formatResults}
\alias{formatResults}

\title{Reshape the data}
\description{
Function to reshape matrix to match the rownames of the numeric matrix \code{WR}.
}

\usage{
formatResults(x)
}

\arguments{
  \item{x}{matrix or data frame}
}
\details{
This auxiliary function is typically used to reshape the matrix/data frame 
\code{results} produced by many functions of GCDkit. The resulting matrix/data 
frame matches the sample names/order in the numeric matrix \code{WR} and data frame
\code{labels} and thus is ready to be appended by the function\code{\link{addResults}}.
}

\value{
A reshaped data frame/matrix.
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\keyword{internal}
