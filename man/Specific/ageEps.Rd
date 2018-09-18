\name{ageEps}
\title{Plot Sr or Nd growth lines}
\alias{ageEps}
\alias{ageEps2}
\alias{ageSr}

\section{Plugin}{SrNd.r}

\description{
Plots Nd or Sr growth curves in the binary diagram age-\eqn{\epsilon(Nd)} or
age-Sr isotopic ratio.
    \if{html}{\figure{ageEps.png}} 
    \if{latex}{\figure{ageEps.png}{options: width=5.5in}}
}

\usage{
    ageEps(GUI=FALSE,...)

    ageEps2(GUI=FALSE,...)

    ageSr(GUI=FALSE,...)
}

\arguments{
    \item{GUI}{logical; is the function called from the GUI?}  
    \item{...}{optional parameters to the underlying function \code{{plotWithLimits}}}
}

\details{
    The Nd growth curves in individual samples can be plotted using either a single-
    or two-stage \emph{(Liew & Hofmann 1988)} models.

    In case of Nd are shown growth curves for the two main mantle reservoirs, CHUR 
    and Depleted Mantle (DM) (the latter in two modifications, after \emph{Goldstein et
    al. (1988)} and \emph{Liew & Hofmann (1988)}.

    For Sr only uniform reservoir (UR) development is calculated using parameters of
    \emph{Faure (1986 and references therein)}. 

    The small ticks, or rugs, on x axis correspond to Nd model ages, on y axis to
    initial \eqn{\epsilon(Nd)} values.

    This function is Figaro compatible.
}

\value{
    None.
}

\references{
    Faure G (1986) Principles of Isotope Geology. J.Wiley & Sons, Chichester, 589 pp

    Goldstein S L, O'Nions R K & Hamilton P J (1984) A Sm-Nd isotopic study of 
    atmospheric dusts and particulates from major river systems. Earth Planet Sci 
    Lett 70: 221-236
    \href{http://dx.doi.org/10.1016/0012-821X(84)90007-4}{doi: 10.1016/0012-821X(84)90007-4}

    Liew T C & Hofmann A W (1988) Precambrian crustal components, plutonic 
    associations, plate environment of the Hercynian Fold Belt of Central Europe: 
    indications from a Nd and Sr isotopic study. Contrib Mineral Petrol 98: 129-138
    \href{http://dx.doi.org/10.1007/BF00402106}{doi: 10.1007/BF00402106}
}

\seealso{
    The actual plotting is done by the function \code{\link{plotWithLimits}}.
}

\keyword{hplot}
\concept{Plot}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
