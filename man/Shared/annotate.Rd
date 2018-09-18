\name{annotate}
\alias{annotate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reformatting labels for the plots}
\description{
Function to reformat sub- and superscripts for production of  "publication 
quality" plots.
}

\usage{
annotate(text)
}

\arguments{
  \item{text}{a text string}
}

\details{
Apart from names of oxides, the function can handle the following strings used 
by the plugin '\code{SrNd.r}':
\preformatted{
'87Rb/86Sr' '87Sr/86Sr' '87Sr/86Sri'
'147Sm/144Nd' '143Nd/144Nd' '143Nd/144Ndi' 'EpsNdi' 
}
}

\value{
  An expression with reformatted text string.
}

\examples{
  plot(1,1,xlab=annotate("SiO2/Fe2O3"),ylab=annotate("K2O-P2O5"))
  plot(1,1,xlab=annotate("87Rb/86Sr"),ylab=annotate("87Sr/86Sr"))
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{internal}
