\name{Whalen}
\title{A type granitoids (Whalen et al. 1987)}
\alias{Whalen}

\description{
    Set of discrimination plots to distinguish A-type granitoids 
    as defined by \emph{Whalen et al.(1987)}. 
}

\usage{
    Whalen(plot.txt = getOption("gcd.plot.text"))
}

\arguments{
    \item{plot.txt}{logical, annotate fields by their names?}
}

\details{
    Set of binary plots proposed by \emph{Whalen et al.(1987)} to distinguish A-type
    granitoids on the one hand from ordinary/fractionated I- and S-types on the other.
 
    In total 12 diagrams are plotted split into two pages. Apart from fields for 
    I and S type granites ('\code{I & S}'), sometimes split into ordinary ('\code{OGT}')
    and fractionated ('\code{'FG'}')domains, average composition of the A type granites 
    (labeled '\code{A}') are shown. See Figs 1, 2 and 5 in the original  paper 
    \emph{(Whalen et al.1987)} for comparison.
    
    \if{html}{\figure{Whalen.png}} 
    \if{latex}{\figure{Whalen.png}{options: width=5.5in}}

    The following diagrams are plotted: 
    \eqn{Zr+Nb+Ce+Y}{Zr+Nb+Ce+Y} vs. 
    \eqn{FeOt/MgO}{FeOt/MgO} and 
    \eqn{(K_{2}O+Na_{2}O)/CaO}{(K2O+Na2O)/CaO}; 
    \eqn{10000Ga/Al}{10000Ga/Al} vs. \eqn{K_{2}O+Na_{2}O}{K2O+Na2O}, 
    \eqn{(K_{2}O+Na_{2}O)/CaO}{(K2O+Na2O)/CaO},
    \eqn{K_{2}O/MgO}{K2O/MgO} and \eqn{FeOt/MgO}{FeOt/MgO}; 
    \eqn{10000Ga/Al}{10000Ga/Al} vs. \eqn{Zr, Nb, Ce, Y, Zn}{Zr, Nb, Ce, Y, Zn} and 
    Agpaitic Index. 
}

\value{
To the matrix '\code{WR}' are appended two columns, with Ga/Al ratios and 
values of the Agpaitic Index (labeled 'A.I.'). 
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
Whalen J B, Currie K L, Chappell B W (1987) A-type granites: geochemical 
characteristics,discrimination and petrogenesis. Contrib Mineral Petrol 95: 
407-419. \href{http://dx.doi.org/10.1007/BF00402202}{doi: 10.1007/BF00402202}
}

\seealso{
\link{Plate}
\link{Plate editing}
\link{plotPlate}
\link{figaro}
}

\examples{
#plot the diagrams
plotPlate("Whalen")
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
