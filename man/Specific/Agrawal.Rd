\name{Agrawal}
\Rdversion{1.1}
\alias{Agrawal}

\title{Trace-element based discrimination plots for (ultra-)basic rocks (Agrawal et al. 2008)}

\description{
    Plots data stored in '\code{WR}' into discrimination plots proposed by 
    \emph{Agrawal et al. (2008)} for (ultra-) basic rocks (\eqn{SiO_{2}}{SiO2} < 52 wt. \%).
}
\usage{
    Agrawal(plot.txt = getOption("gcd.plot.text"),GUI=FALSE)
}

\arguments{
    \item{plot.txt}{logical, annotate fields by their names?}
    \item{GUI}{logical, is the function called from a GUI?}
}

\details{
    Suite of five diagrams for discrimination of geotectonic environment of ultrabasic
    and basic rocks, proposed by \emph{Agrawal et al. (2008)}.
    It is based on linear discriminant analysis applied to log-transformed concentration 
    ratios of five trace elements (La, Sm, Yb, Nb, and Th), i.e., using four
    ratios \eqn{ln(La/Th)}{ln(La/Th)}, \eqn{ln(Sm/Th)}{ln(Sm/Th)}, \eqn{ln(Yb/Th)}{ln(Yb/Th)}, 
    and \eqn{ln(Nb/Th)}{ln(Nb/Th)}. The two discriminant functions,
    DF1 and DF2, are mathematically designed to maximize the separation between the groups and
    account for 100 percent of the variance in the data.

    Note that only samples with \eqn{SiO_{2}}{SiO2} < 52 wt. \% are 
    plotted. 

    Also note that each diagram applies only to environments explicitly mentioned. 
    Samples from the environment not taken into account will be misinterpreted 
    (the CRB + OIB + MORB diagram is not designed for IAB etc.) 
    See the Agrawal et al (2008) for further details.

    Following geotectonic settings may be deduced:

    \tabular{ll}{
    Abbreviation used \tab Environment \cr
        IAB \tab \emph{island arc basic rocks} \cr
        CRB \tab \emph{continental rift basic rocks} \cr
        OIB \tab \emph{ocean-island basic rocks} \cr
        MORB \tab \emph{mid-ocean ridge basic rocks} \cr
    }
    
    \if{html}{\figure{Agrawal.png}} 
    \if{latex}{\figure{Agrawal.png}{options: width=5.5in}}
}

\value{
    None.
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots.
     
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
    Agrawal S, Guevara M, Verma S (2008) Tectonic discrimination of basic 
    and ultrabasic volcanic rocks through log-transformed ratios of immobile trace elements. 
    Int Geol Review 50: 1057-1079 
    \href{http://dx.doi.org/10.2747/0020-6814.50.12.1057}{doi: 10.2747/0020-6814.50.12.1057}
}

\examples{
#plot the diagrams
plotPlate("Agrawal")
}

\seealso{
\link{Verma}, \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
