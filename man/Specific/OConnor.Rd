\name{OConnor}
\alias{OConnor}
\alias{OConnor.mod}
\alias{OConnorVolc}
\alias{OConnorPlut}
\title{Classification diagram for siliceous igneous rocks, 
    based on Fsp composition (O'Connor 1965)
}

\description{
    Assigns data for O'Connor's triangular diagram into Figaro template 
    (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.   
}
\usage{
    OConnorVolc()

    OConnorPlut(ab=NULL,an=NULL,or=NULL)
}

\arguments{
    \item{ab,an,or}{character; specification of the plotting variables.}
}

\details{
    The O'Connor's triangular diagram is based on combination of Albite, Anorthite and 
    K-feldspar modal or normative data. While the function '\code{OConnorPlut}'  can 
    plot either modal or normative diagrams for plutonic rocks, '\code{OConnorVolc}' 
    is to be used exclusively with normative data computed from chemical compositions 
    of volcanic rocks.

    \if{html}{\figure{OConnorVolc.png}} 
    \if{latex}{\figure{OConnorVolc.png}{options: width=4in}}

    In fact, the triangle represents projection of the Quartz - K-feldspar - Albite 
    - Anorthite tetrahedron. All three diagrams are designed for quartz-rich rocks, 
    i.e. those with quartz contents higher than 10 %. The author postulates that for 
    such silica-rich samples, the rock type can be determined purely on the basis of 
    the feldspars' proportions.

    \if{html}{\figure{OConnorPlut.png}} 
    \if{latex}{\figure{OConnorPlut.png}{options: width=4in}}

    As the specific version of the normative calculation is not mentioned in the 
    original paper by '\emph{O'Connor (1965)}', the function '\code{CIPW}',
    designed after '\emph{Hutchison (1974, 1975)}' was implemented as a default calculation 
    scheme.
    
    Alternatively, the plotting variables can be present already in the dataset (variable 
    \code{WR}). The variables to be plotted can be then specified upon call or can be 
    selected using the function '\code{\link{selectColumnLabel}}'. 

    The samples can be selected based on combination of three searching 
    mechanisms (by sample name/label, range or a Boolean condition) - see 
    \code{\link{selectSubset}} for details.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{An, Ab and Or data (see details) transformed to orthogonal coordinates}
}


\references{
    O'Connor J T (1965) A classification for Quartz-rich igneous rocks based on 
    feldspar ratios. U.S. Geol. Survey Prof Paper 525-B: B79-B84 

    Hutchison C S (1974) Laboratory Handbook of Petrographic Techniques. John Wiley 
    & Sons, New York, p. 1-527

    Hutchison C S (1975) The norm, its variations, their calculation and relationships. 
    Schweiz Mineral Petrogr Mitt 55: 243-256
}

\author{
    Vojtech Erban, \email{vojtech.erban@geology.cz}
    
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
}

\seealso{
    \link{classify}
    \link{figaro}
    \link{CIPW}
    \link{plotDiagram}
}

\examples{
    plotDiagram("OConnorVolc",FALSE)

    classify("OConnorVolc")

    results<-Mesonorm(WR)
    addResults()
    plotDiagram("OConnorPlut",FALSE,ab="Albite",an="Anorthite",or="Orthoclase")
    
}

\keyword{hplot}
\concept{Menu: Plots}
\concept{Plots}
\concept{Plot}
