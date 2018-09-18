\name{figaro.identify}
\title{Plot editing: Identification of plotted symbols}
\alias{Identify points}
\alias{Highlight multiple points}
\alias{highlightSelection}
\alias{figIdentify}

\description{
These functions allow the user to identify points in Figaro-compatible plots.
}

\usage{
figIdentify()

highlightSelection()
}

\details{
'\code{figIdentify}' identifies points closest to a mouse pointer, if a mouse button 
is pressed. For binary and ternary plots, sample names are plotted; for spider 
plots the function prints the sample name, concentration of the given element (in ppm) 
and highlights the whole pattern. The identification is terminated by pressing 
the right button and selecting \code{'Stop'} from the menu.

'\code{highlightSelection}' allows the selected analyses to be highlighted. The 
samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see \code{\link{selectSubset}} 
for details. 

If the search results are empty or embrace all samples, the user is given a chance 
to select the samples from the list of their names. Press Ctrl+click to select 
multiple ones.  

For binary and ternary plots, Press Esc in the Console window to stop the points 
blinking. In spider plots are shown overall ranges of normalized concentrations 
(by a gray field) with superimposed patterns for selected samples. 
}

\seealso{
    \code{\link{identify}} \code{\link{selectSubset}} '\code{\link{figaro}}'
}

\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{c.farrow@compserv.gla.ac.uk} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

