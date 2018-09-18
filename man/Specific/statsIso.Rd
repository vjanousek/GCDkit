\name{statsIso}
\title{Statistical plots of isotopic ratios/model ages}
\alias{boxplotIso}
\alias{stripplotIso}
\alias{Boxplot isotopic ratios/model ages}
\alias{Stripplot isotopic ratios/model ages}

\section{Plugin}{SrNd.r}

\description{
    Plots a boxplot or stripplot for a given isotopic parameter, respecting groups.
}

\usage{
boxplotIso(what=NULL)

stripplotIso(what=NULL)
}

\arguments{
  \item{what}{the variable name; see Details.}
}

\details{
The boxplot portrays realistically a statistical distribution of the data. The 
box represents, for each of the groups, the two quartiles, the line inside is a 
median, the whiskers show the whole range without outliers.
    \if{html}{\figure{boxplotIso.png}} 
    \if{latex}{\figure{boxplotIso.png}{options: width=4.5in}}
    
Stripplot shows 1D scatter plots for each of the groups, with some artificial 
noise (jitter) added to make the individual points better visible. Stripplots 
are a good alternative to boxplots when sample sizes are small.
    \if{html}{\figure{stripplotIso.png}} 
    \if{latex}{\figure{stripplotIso.png}{options: width=4.5in}}
    
The variables to choose from are:
    \tabular{ll}{
        \bold{Menu item} \tab \bold{Explanation}\cr
        87Sr/86Sri\tab Initial Sr isotopic ratios\cr
        143Nd/144Ndi\tab Initial Nd isotopic ratios\cr
        EpsNdi\tab Initial \eqn{\epsilon(Nd)} values\cr
        1 stg DM model ages \emph{(Goldstein et al. 1988)}\tab Single-stage DM Nd model ages\cr
        1 stg DM model ages \emph{(Liew & Hofmann 1988)}\tab Single-stage DM Nd model ages\cr
        2 stg DM model ages \emph{(Liew & Hofmann 1988)}\tab Two-stage DM Nd model ages
    }
In addition, any variable names starting with the text \code{"delta"} will appear in this list.

The variable names can be specified also upon the function call, as the parameter \code{"what"}. 
The possibilities are \code{"87Sr/86Sri"}, \code{"143Nd/144Ndi"}, \code{"EpsNdi"}, 
\code{"TDM.Gold"}, \code{"TDM"} or \code{"TDM.2stg"}.
}

\value{
  \item{}{a list object with data produced by the function '\link{boxplot}'.}
}

\seealso{
  '\link{boxplot}'
}

\references{
    Goldstein S L, O'Nions R K & Hamilton P J (1984) A Sm-Nd isotopic study of 
    atmospheric dusts and particulates from major river systems. Earth Planet Sci 
    Lett 70: 221-236
    \href{http://dx.doi.org/10.1016/0012-821X(84)90007-4}{doi: 10.1016/0012-821X(84)90007-4}

    Liew T C & Hofmann A W (1988) Precambrian crustal components, plutonic 
    associations, plate environment of the Hercynian Fold Belt of Central Europe: 
    indications from a Nd and Sr isotopic study. Contrib Mineral Petrol 98: 129-138
    \href{http://dx.doi.org/10.1007/BF00402106}{doi: 10.1007/BF00402106}
}

\keyword{univar}
\concept{Calculation}
\concept{Isotopic}
\concept{Menu: Plugins}
\concept{Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
