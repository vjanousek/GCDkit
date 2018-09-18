\name{pickColour}
\alias{pickColour}
\title{Select a valid colour name}

\description{
An auxiliary function to enter and check a validity of the color name.
}

\usage{
pickColour(prompt = "Enter plotting colour 
(or press Enter to pick from a list)", default = "")
}

\arguments{
  \item{prompt}{Text prompting to enter a new colour name}
  \item{default}{an optional default colour name}
}

\value{
Returns a colour name or code.
}

\seealso{
\code{\link{colours}}.
} 

\keyword{manip}
\keyword{color}
\keyword{aplot}
\keyword{internal}
\concept{Edit}
\concept{Figaro}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
