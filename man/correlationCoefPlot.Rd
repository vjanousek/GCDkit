\name{correlationCoefPlot}
\title{Statistics: Correlation coefficient patterns}
\alias{Correlation coefficient patterns}
\alias{correlationCoefPlot}


\description{
    Produces, for each group a separate, set of plots of correlation coefficient patterns. 
}

\usage{
    correlationCoefPlot(elems = NULL)
}

\arguments{
    \item{elems}{list of desired elements}
}

\details{
    The utility of correlation coefficient patterns was discussed by \emph{Rollinson 
    (1993 and references therein)}. Basically similarity in correlation patterns 
    between two or more elements means their analogous geochemical behaviour, 
    potentially controlled by the same geochemical process (fractional crystallization, 
    partial melting, weathering, hydrothermal alteration...)
    
    \if{html}{\figure{correlationCoefPlot.png}} 
    \if{latex}{\figure{correlationCoefPlot.png}{options: width=5.5in}}

    The variables are selected using the function '\code{\link{selectColumnsLabels}}'.
}

\value{
    None.
}

\examples{
    correlationCoefPlot(elems="K,Rb,Sr,Cr,Nb,Ti")
}

\references{
    Rollinson H R (1993) Using Geochemical Data: Evaluation, Presentation, 
    Interpretation. Longman, London, p. 1-352
}

\keyword{hplot}
\concept{Statistics}
\concept{Plot}
\concept{Menu: Calculations: Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
